#!/usr/bin/env guile
!#

;; Guile MCP Server for Home Lab Integration
;; Implements Model Context Protocol for VS Code extension

(use-modules (json)
             (ice-9 textual-ports)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 match)
             (ice-9 threads)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26))

;; MCP Protocol Implementation
(define mcp-protocol-version "2024-11-05")
(define request-id-counter 0)

;; Server capabilities and state
(define server-capabilities
  `((tools . ())
    (resources . ())
    (prompts . ())))

(define server-info
  `((name . "guile-homelab-mcp")
    (version . "0.1.0")))

;; Request/Response utilities
(define (make-response id result)
  `((jsonrpc . "2.0")
    (id . ,id)
    (result . ,result)))

(define (make-error id code message)
  `((jsonrpc . "2.0")
    (id . ,id)
    (error . ((code . ,code)
              (message . ,message)))))

(define (send-response response)
  (let ((json-str (scm->json-string response)))
    (display json-str)
    (newline)
    (force-output)))

;; Home Lab Tools Implementation
(define (list-machines)
  "List all available machines in the home lab"
  (let* ((proc (open-input-pipe "find /etc/nixos/hosts -name '*.nix' -type f"))
         (output (read-string proc)))
    (close-pipe proc)
    (if (string-null? output)
        '()
        (map (lambda (path)
               (basename path ".nix"))
             (string-split (string-trim-right output #\newline) #\newline)))))

(define (get-machine-status machine)
  "Get status of a specific machine"
  (let* ((cmd (format #f "ping -c 1 -W 1 ~a > /dev/null 2>&1" machine))
         (status (system cmd)))
    (if (= status 0) "online" "offline")))

(define (deploy-machine machine method)
  "Deploy configuration to a machine"
  (match method
    ("deploy-rs"
     (let ((cmd (format #f "deploy '.#~a'" machine)))
       (deploy-with-command cmd machine)))
    ("hybrid-update"
     (let ((cmd (format #f "nixos-rebuild switch --flake '.#~a' --target-host ~a --use-remote-sudo" machine machine)))
       (deploy-with-command cmd machine)))
    ("legacy"
     (let ((cmd (format #f "nixos-rebuild switch --flake '.#~a'" machine)))
       (deploy-with-command cmd machine)))
    (_ (throw 'deployment-error "Unknown deployment method" method))))

(define (deploy-with-command cmd machine)
  "Execute deployment command and return result"
  (let* ((proc (open-input-pipe cmd))
         (output (read-string proc))
         (status (close-pipe proc)))
    `((success . ,(= status 0))
      (output . ,output)
      (machine . ,machine)
      (timestamp . ,(date->string (current-date))))))

(define (generate-nix-config machine-name services)
  "Generate NixOS configuration for a new machine"
  (let ((config (format #f "# Generated NixOS configuration for ~a
# Generated on ~a

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # Machine name
  networking.hostName = \"~a\";

  # Basic system configuration
  system.stateVersion = \"23.11\";
  
  # Enable services
~a

  # Network configuration
  networking.firewall.enable = true;
  
  # SSH access
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    # Add your public key here
  ];
}
"
                        machine-name
                        (date->string (current-date))
                        machine-name
                        (string-join 
                         (map (lambda (service)
                                (format #f "  services.~a.enable = true;" service))
                              services)
                         "\n"))))
    `((content . ,config)
      (filename . ,(format #f "~a.nix" machine-name)))))

(define (get-infrastructure-status)
  "Get comprehensive infrastructure status"
  (let* ((machines (list-machines))
         (machine-status (map (lambda (m)
                               `((name . ,m)
                                 (status . ,(get-machine-status m))))
                             machines)))
    `((machines . ,machine-status)
      (timestamp . ,(date->string (current-date)))
      (total-machines . ,(length machines))
      (online-machines . ,(length (filter (lambda (m)
                                            (equal? (assoc-ref m 'status) "online"))
                                          machine-status))))))

;; MCP Tools Registry
(define mcp-tools
  `(((name . "deploy-machine")
     (description . "Deploy NixOS configuration to a home lab machine")
     (inputSchema . ((type . "object")
                     (properties . ((machine . ((type . "string")
                                                (description . "Machine hostname to deploy to")))
                                   (method . ((type . "string")
                                             (enum . ("deploy-rs" "hybrid-update" "legacy"))
                                             (description . "Deployment method to use")))))
                     (required . ("machine" "method")))))
    
    ((name . "list-machines")
     (description . "List all available machines in the home lab")
     (inputSchema . ((type . "object")
                     (properties . ()))))
    
    ((name . "check-status")
     (description . "Check status of home lab infrastructure")
     (inputSchema . ((type . "object")
                     (properties . ((machine . ((type . "string")
                                                (description . "Specific machine to check (optional)")))))))
    
    ((name . "generate-nix-config")
     (description . "Generate NixOS configuration for a new machine")
     (inputSchema . ((type . "object")
                     (properties . ((machine-name . ((type . "string")
                                                     (description . "Name for the new machine")))
                                   (services . ((type . "array")
                                               (items . ((type . "string")))
                                               (description . "List of services to enable")))))
                     (required . ("machine-name")))))
    
    ((name . "list-services")
     (description . "List available NixOS services")
     (inputSchema . ((type . "object")
                     (properties . ()))))))

;; MCP Resources Registry  
(define mcp-resources
  `(((uri . "homelab://status/all")
     (name . "Infrastructure Status")
     (description . "Complete status of all home lab machines and services")
     (mimeType . "application/json"))
    
    ((uri . "homelab://status/summary")
     (name . "Status Summary")
     (description . "Summary of infrastructure health")
     (mimeType . "text/plain"))
    
    ((uri . "homelab://context/copilot")
     (name . "Copilot Context")
     (description . "Context information for GitHub Copilot integration")
     (mimeType . "text/markdown"))))

;; Tool execution dispatcher
(define (execute-tool name arguments)
  "Execute a registered MCP tool"
  (match name
    ("deploy-machine"
     (let ((machine (assoc-ref arguments 'machine))
           (method (assoc-ref arguments 'method)))
       (deploy-machine machine method)))
    
    ("list-machines"
     `((machines . ,(list-machines))))
    
    ("check-status"
     (let ((machine (assoc-ref arguments 'machine)))
       (if machine
           `((machine . ,machine)
             (status . ,(get-machine-status machine)))
           (get-infrastructure-status))))
    
    ("generate-nix-config"
     (let ((machine-name (assoc-ref arguments 'machine-name))
           (services (or (assoc-ref arguments 'services) '())))
       (generate-nix-config machine-name services)))
    
    ("list-services"
     `((services . ("nginx" "postgresql" "redis" "mysql" "docker" "kubernetes"
                   "grafana" "prometheus" "gitea" "nextcloud" "jellyfin"))))
    
    (_ (throw 'unknown-tool "Tool not found" name))))

;; Resource content providers
(define (get-resource-content uri)
  "Get content for a resource URI"
  (match uri
    ("homelab://status/all"
     `((content . ,(get-infrastructure-status))))
    
    ("homelab://status/summary"
     (let ((status (get-infrastructure-status)))
       `((content . ,(format #f "Home Lab Status: ~a/~a machines online"
                            (assoc-ref status 'online-machines)
                            (assoc-ref status 'total-machines))))))
    
    ("homelab://context/copilot"
     (let ((status (get-infrastructure-status)))
       `((content . ,(format #f "# Home Lab Infrastructure Context

## Current Status
- Total Machines: ~a
- Online Machines: ~a
- Last Updated: ~a

## Available Operations
Use the home lab extension commands or MCP tools for:
- Deploying configurations (deploy-machine)
- Checking infrastructure status (check-status)  
- Generating new machine configs (generate-nix-config)
- Managing services across the fleet

## Machine List
~a

This context helps GitHub Copilot understand your home lab infrastructure state."
                            (assoc-ref status 'total-machines)
                            (assoc-ref status 'online-machines)
                            (assoc-ref status 'timestamp)
                            (string-join
                             (map (lambda (m)
                                    (format #f "- ~a: ~a"
                                           (assoc-ref m 'name)
                                           (assoc-ref m 'status)))
                                  (assoc-ref status 'machines))
                             "\n"))))))
    
    (_ (throw 'unknown-resource "Resource not found" uri))))

;; MCP Protocol Handlers
(define (handle-initialize params)
  "Handle MCP initialize request"
  `((protocolVersion . ,mcp-protocol-version)
    (capabilities . ((tools . ((listChanged . #f)))
                    (resources . ((subscribe . #f)
                                 (listChanged . #f)))
                    (prompts . ((listChanged . #f)))))
    (serverInfo . ,server-info)))

(define (handle-tools-list params)
  "Handle tools/list request"
  `((tools . ,mcp-tools)))

(define (handle-tools-call params)
  "Handle tools/call request"
  (let ((name (assoc-ref params 'name))
        (arguments (assoc-ref params 'arguments)))
    (execute-tool name arguments)))

(define (handle-resources-list params)
  "Handle resources/list request"
  `((resources . ,mcp-resources)))

(define (handle-resources-read params)
  "Handle resources/read request"
  (let ((uri (assoc-ref params 'uri)))
    (get-resource-content uri)))

;; Main request dispatcher
(define (handle-request request)
  "Main request handler"
  (let ((method (assoc-ref request 'method))
        (params (assoc-ref request 'params))
        (id (assoc-ref request 'id)))
    
    (catch #t
      (lambda ()
        (let ((result
               (match method
                 ("initialize" (handle-initialize params))
                 ("tools/list" (handle-tools-list params))
                 ("tools/call" (handle-tools-call params))
                 ("resources/list" (handle-resources-list params))
                 ("resources/read" (handle-resources-read params))
                 (_ (throw 'method-not-found "Method not supported" method)))))
          (send-response (make-response id result))))
      
      (lambda (key . args)
        (send-response (make-error id -32603 (format #f "~a: ~a" key args)))))))

;; Main server loop
(define (run-mcp-server)
  "Run the MCP server main loop"
  (let loop ()
    (let ((line (read-line)))
      (unless (eof-object? line)
        (catch #t
          (lambda ()
            (let ((request (json-string->scm line)))
              (handle-request request)))
          (lambda (key . args)
            (send-response (make-error 0 -32700 "Parse error"))))
        (loop)))))

;; Export main function for use as module
(define-public run-mcp-server run-mcp-server)

;; Run server if called directly
(when (equal? (car (command-line)) (current-filename))
  (run-mcp-server))
