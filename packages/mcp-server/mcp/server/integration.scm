;; MCP Server Integration with Guile Infrastructure
;; This module integrates the MCP server with existing Guile-based home lab infrastructure

(define-module (mcp server integration)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (mcp server jsonrpc)
  #:use-module (mcp server protocol)
  #:use-module (mcp server transport)
  #:use-module (mcp server router)
  #:use-module (mcp server validation)
  #:use-module (mcp server error-handling)
  #:export (create-integrated-mcp-server
            register-lab-tools
            register-lab-resources
            register-lab-prompts
            lab-command-executor
            lab-config-reader
            lab-status-checker
            setup-mcp-server
            start-mcp-server))

;; Lab command executor - integrates with existing shell commands
(define (lab-command-executor command args)
  "Execute a lab command and return the result"
  (catch #t
    (lambda ()
      (let* ((cmd-string (string-join (cons command args) " "))
             (port (open-input-pipe cmd-string))
             (output (read-string port))
             (exit-code (close-pipe port)))
        (if (= exit-code 0)
            `(("success" . #t)
              ("output" . ,output)
              ("exit_code" . ,exit-code))
            `(("success" . #f)
              ("output" . ,output)
              ("error" . "Command failed")
              ("exit_code" . ,exit-code)))))
    (lambda (key . args)
      `(("success" . #f)
        ("error" . ,(format #f "Exception: ~a" key))
        ("details" . ,args)))))

;; Configuration reader - reads lab configuration
(define (lab-config-reader config-path)
  "Read lab configuration from file"
  (catch #t
    (lambda ()
      (if (file-exists? config-path)
          (call-with-input-file config-path
            (lambda (port)
              (json->scm port)))
          `(("error" . "Configuration file not found")
            ("path" . ,config-path))))
    (lambda (key . args)
      `(("error" . ,(format #f "Failed to read config: ~a" key))
        ("details" . ,args)))))

;; Status checker - checks lab infrastructure status
(define (lab-status-checker)
  "Check the status of lab infrastructure"
  (let ((services '("ssh" "docker" "nixos-rebuild"))
        (status-results '()))
    
    (for-each
     (lambda (service)
       (let ((result (lab-command-executor "systemctl" (list "is-active" service))))
         (set! status-results
               (cons `(,service . ,result) status-results))))
     services)
    
    `(("timestamp" . ,(current-time))
      ("services" . ,status-results))))

;; Tool registration functions
(define (register-lab-tools server)
  "Register lab management tools with the MCP server"
  
  ;; Machine management tools
  (register-route (mcp-server-handlers server) "tools/machine/list"
                  (lambda (server params)
                    (lab-command-executor "ls" '("/etc/nixos/machines"))))
  
  (register-route (mcp-server-handlers server) "tools/machine/status"
                  (lambda (server params)
                    (let ((machine (hash-ref params "machine" #f)))
                      (if machine
                          (lab-command-executor "ping" (list "-c" "1" machine))
                          '(("error" . "Machine name required"))))))
  
  (register-route (mcp-server-handlers server) "tools/machine/deploy"
                  (lambda (server params)
                    (let ((machine (hash-ref params "machine" #f))
                          (config (hash-ref params "config" #f)))
                      (if (and machine config)
                          (lab-command-executor "nixos-rebuild" 
                                                (list "switch" "--target-host" machine 
                                                      "--flake" config))
                          '(("error" . "Machine and config required"))))))
  
  ;; Service management tools
  (register-route (mcp-server-handlers server) "tools/service/status"
                  (lambda (server params)
                    (let ((service (hash-ref params "service" #f)))
                      (if service
                          (lab-command-executor "systemctl" (list "status" service))
                          '(("error" . "Service name required"))))))
  
  (register-route (mcp-server-handlers server) "tools/service/restart"
                  (lambda (server params)
                    (let ((service (hash-ref params "service" #f)))
                      (if service
                          (lab-command-executor "systemctl" (list "restart" service))
                          '(("error" . "Service name required"))))))
  
  ;; Docker management tools
  (register-route (mcp-server-handlers server) "tools/docker/ps"
                  (lambda (server params)
                    (lab-command-executor "docker" '("ps" "--format" "json"))))
  
  (register-route (mcp-server-handlers server) "tools/docker/logs"
                  (lambda (server params)
                    (let ((container (hash-ref params "container" #f))
                          (lines (hash-ref params "lines" "100")))
                      (if container
                          (lab-command-executor "docker" 
                                                (list "logs" "--tail" lines container))
                          '(("error" . "Container name required"))))))
  
  ;; Network tools
  (register-route (mcp-server-handlers server) "tools/network/scan"
                  (lambda (server params)
                    (let ((network (hash-ref params "network" "192.168.1.0/24")))
                      (lab-command-executor "nmap" (list "-sn" network)))))
  
  ;; Configuration tools
  (register-route (mcp-server-handlers server) "tools/config/validate"
                  (lambda (server params)
                    (let ((config-path (hash-ref params "path" "/etc/nixos/configuration.nix")))
                      (lab-command-executor "nixos-rebuild" (list "dry-build" "--flake" config-path))))))

(define (register-lab-resources server)
  "Register lab infrastructure resources with the MCP server"
  
  ;; Configuration files
  (register-route (mcp-server-handlers server) "resources/config/nixos"
                  (lambda (server params)
                    (lab-config-reader "/etc/nixos/configuration.nix")))
  
  (register-route (mcp-server-handlers server) "resources/config/machines"
                  (lambda (server params)
                    (lab-command-executor "find" '("/etc/nixos/machines" "-name" "*.nix"))))
  
  ;; System information
  (register-route (mcp-server-handlers server) "resources/system/info"
                  (lambda (server params)
                    `(("hostname" . ,(gethostname))
                      ("uptime" . ,(lab-command-executor "uptime" '()))
                      ("load" . ,(lab-command-executor "cat" '("/proc/loadavg")))
                      ("memory" . ,(lab-command-executor "free" '("-h"))))))
  
  ;; Network information
  (register-route (mcp-server-handlers server) "resources/network/interfaces"
                  (lambda (server params)
                    (lab-command-executor "ip" '("addr" "show"))))
  
  (register-route (mcp-server-handlers server) "resources/network/routes"
                  (lambda (server params)
                    (lab-command-executor "ip" '("route" "show"))))
  
  ;; Storage information
  (register-route (mcp-server-handlers server) "resources/storage/disk"
                  (lambda (server params)
                    (lab-command-executor "df" '("-h"))))
  
  (register-route (mcp-server-handlers server) "resources/storage/zfs"
                  (lambda (server params)
                    (lab-command-executor "zfs" '("list"))))
  
  ;; Log files
  (register-route (mcp-server-handlers server) "resources/logs/system"
                  (lambda (server params)
                    (let ((lines (hash-ref params "lines" "100")))
                      (lab-command-executor "journalctl" (list "--lines" lines "--no-pager")))))
  
  (register-route (mcp-server-handlers server) "resources/logs/service"
                  (lambda (server params)
                    (let ((service (hash-ref params "service" #f))
                          (lines (hash-ref params "lines" "100")))
                      (if service
                          (lab-command-executor "journalctl" 
                                                (list "-u" service "--lines" lines "--no-pager"))
                          '(("error" . "Service name required")))))))

(define (register-lab-prompts server)
  "Register lab management prompts with the MCP server"
  
  ;; Deployment prompts
  (register-route (mcp-server-handlers server) "prompts/deploy/machine"
                  (lambda (server params)
                    `(("prompt" . "Deploy configuration to machine")
                      ("description" . "Deploy NixOS configuration to a target machine")
                      ("parameters" . (("machine" . (("type" . "string")
                                                     ("description" . "Target machine hostname")))
                                      ("config" . (("type" . "string")
                                                  ("description" . "Configuration flake path")))
                                      ("dry_run" . (("type" . "boolean")
                                                   ("description" . "Perform dry run only"))))))))
  
  ;; Troubleshooting prompts
  (register-route (mcp-server-handlers server) "prompts/troubleshoot/service"
                  (lambda (server params)
                    `(("prompt" . "Troubleshoot service issues")
                      ("description" . "Diagnose and troubleshoot service problems")
                      ("parameters" . (("service" . (("type" . "string")
                                                     ("description" . "Service name to troubleshoot")))
                                      ("include_logs" . (("type" . "boolean")
                                                        ("description" . "Include service logs"))))))))
  
  ;; Monitoring prompts
  (register-route (mcp-server-handlers server) "prompts/monitor/system"
                  (lambda (server params)
                    `(("prompt" . "Monitor system health")
                      ("description" . "Check overall system health and performance")
                      ("parameters" . (("detailed" . (("type" . "boolean")
                                                      ("description" . "Include detailed metrics")))
                                      ("alerts_only" . (("type" . "boolean")
                                                        ("description" . "Show only alerts and warnings")))))))))

;; Main integration setup
(define* (setup-mcp-server #:key (name "home-lab-mcp") (version "1.0.0") (transport-type 'stdio) (port 8080))
  "Set up and configure the integrated MCP server"
  (let* ((server (create-mcp-server name version))
         (router (create-default-router))
         (error-handler (create-default-error-handler))
         (transport (case transport-type
                      ((stdio) (stdio-transport))
                      ((http) (http-transport port))
                      ((websocket) (websocket-transport port))
                      (else (stdio-transport)))))
    
    ;; Register lab-specific handlers
    (register-lab-tools server)
    (register-lab-resources server)
    (register-lab-prompts server)
    
    ;; Return configured server and transport
    (values server transport router error-handler)))

(define* (start-mcp-server #:key (transport-type 'stdio) (port 8080))
  "Start the integrated MCP server"
  (receive (server transport router error-handler)
      (setup-mcp-server #:transport-type transport-type #:port port)
    
    (format (current-error-port) "Starting MCP server with ~a transport~%" transport-type)
    
    ;; Start the server
    (catch #t
      (lambda ()
        (run-mcp-server server transport))
      (lambda (key . args)
        (handle-error error-handler 'internal-error 
                      (cons key args) 
                      "MCP server startup")))
    
    (format (current-error-port) "MCP server stopped~%")))

;; Convenience function for creating integrated server
(define (create-integrated-mcp-server)
  "Create a fully integrated MCP server with all lab tools"
  (receive (server transport router error-handler)
      (setup-mcp-server)
    server))
