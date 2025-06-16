;; lab/core.scm - Core home lab operations

(define-module (lab core)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (get-infrastructure-status
            check-system-health
            update-flake
            validate-environment
            execute-nixos-rebuild
            check-network-connectivity
            option-ref))

;; Simple option reference function
(define (option-ref options key default)
  "Get option value from options alist with default"
  (let ((value (assoc-ref options key)))
    (if value value default)))

;; Stub logging functions (to be replaced with proper logging module)
(define (log-info format-str . args)
  "Log info message"
  (apply format #t (string-append "[INFO] " format-str "~%") args))

(define (log-debug format-str . args)
  "Log debug message"
  (apply format #t (string-append "[DEBUG] " format-str "~%") args))

(define (log-success format-str . args)
  "Log success message"
  (apply format #t (string-append "[SUCCESS] " format-str "~%") args))

(define (log-error format-str . args)
  "Log error message"
  (apply format #t (string-append "[ERROR] " format-str "~%") args))

(define (log-warn format-str . args)
  "Log warning message"
  (apply format #t (string-append "[WARN] " format-str "~%") args))

;; Stub configuration functions
(define (get-all-machines)
  "Get list of all machines"
  '(grey-area sleeper-service congenital-optimist reverse-proxy))

(define (get-machine-config machine-name)
  "Get configuration for a machine"
  `((services . (systemd ssh))
    (type . server)))

(define (get-ssh-config machine-name)
  "Get SSH configuration for a machine"
  `((hostname . ,(symbol->string machine-name))
    (is-local . #f)))

(define (get-homelab-root)
  "Get home lab root directory"
  "/home/geir/Home-lab")

;; Stub SSH functions
(define (test-ssh-connection machine-name)
  "Test SSH connection to machine"
  (zero? (system (format #f "ssh -o ConnectTimeout=5 -o BatchMode=yes ~a exit 2>/dev/null" machine-name))))

(define (run-remote-command machine-name command . args)
  "Run command on remote machine via SSH"
  (let* ((full-command (if (null? args)
                          command
                          (string-join (cons command args) " ")))
         (ssh-command (format #f "ssh ~a '~a'" machine-name full-command))
         (port (open-input-pipe ssh-command))
         (output (read-string port))
         (status (close-pipe port)))
    (values (zero? status) output)))

;; Utility function for spinner (stub)
(define (with-spinner message proc)
  "Execute procedure with spinner (stub implementation)"
  (display (format #f "~a..." message))
  (let ((result (proc)))
    (display " done.\n")
    result))

;; Get comprehensive infrastructure status
(define (get-infrastructure-status . args)
  "Get status of all machines or specific machine if provided"
  (let ((target-machine (if (null? args) #f (car args)))
        (machines (if (null? args) 
                     (get-all-machines) 
                     (list (car args)))))
    
    (log-info "Checking infrastructure status...")
    
    (map (lambda (machine-name)
           (let ((start-time (current-time)))
             (log-debug "Checking ~a..." machine-name)
             
             (let* ((ssh-config (get-ssh-config machine-name))
                    (is-local (and ssh-config (assoc-ref ssh-config 'is-local)))
                    (connection-status (test-ssh-connection machine-name))
                    (services-status (if connection-status
                                       (get-machine-services-status machine-name)
                                       '()))
                    (system-info (if connection-status
                                   (get-machine-system-info machine-name)
                                   #f))
                    (elapsed (- (current-time) start-time)))
               
               `((machine . ,machine-name)
                 (type . ,(if is-local 'local 'remote))
                 (connection . ,(if connection-status 'online 'offline))
                 (services . ,services-status)
                 (system . ,system-info)
                 (check-time . ,elapsed)))))
         machines)))

;; Get services status for a machine
(define (get-machine-services-status machine-name)
  "Check status of services on a machine"
  (let ((machine-config (get-machine-config machine-name)))
    (if machine-config
        (let ((services (assoc-ref machine-config 'services)))
          (if services
              (map (lambda (service)
                     (call-with-values 
                         (lambda () (run-remote-command machine-name 
                                                       "systemctl is-active" 
                                                       (symbol->string service)))
                       (lambda (success output)
                         `(,service . ,(if success 
                                          (string-trim-right output)
                                          "unknown")))))
                   services)
              '()))
        '())))

;; Get basic system information from a machine
(define (get-machine-system-info machine-name)
  "Get basic system information from a machine"
  (let ((info-commands 
         '(("uptime" "uptime -p")
           ("load" "cat /proc/loadavg | cut -d' ' -f1-3")
           ("memory" "free -h | grep Mem | awk '{print $3\"/\"$2}'")
           ("disk" "df -h / | tail -1 | awk '{print $5}'")
           ("kernel" "uname -r"))))
    
    (fold (lambda (cmd-pair acc)
            (let ((key (car cmd-pair))
                  (command (cadr cmd-pair)))
              (call-with-values 
                  (lambda () (run-remote-command machine-name command))
                (lambda (success output)
                  (if success
                      (assoc-set! acc (string->symbol key) (string-trim-right output))
                      acc)))))
          '()
          info-commands)))

;; Check system health with comprehensive tests
(define (check-system-health machine-name)
  "Perform comprehensive health check on a machine"
  (log-info "Performing health check on ~a..." machine-name)
  
  (let ((health-checks 
         '(("connectivity" . test-ssh-connection)
           ("disk-space" . check-disk-space)
           ("system-load" . check-system-load)
           ("critical-services" . check-critical-services)
           ("network" . check-network-connectivity))))
    
    (map (lambda (check-pair)
           (let ((check-name (car check-pair))
                 (check-proc (cdr check-pair)))
             (log-debug "Running ~a check..." check-name)
             (catch #t
               (lambda ()
                 (let ((result (check-proc machine-name)))
                   `(,check-name . ((status . ,(if result 'pass 'fail))
                                  (result . ,result))))
               (lambda (key . args)
                 (log-warn "Health check ~a failed: ~a" check-name key)
                 `(,check-name . ((status . error)
                                  (error . ,key)))))))
         health-checks)))

;; Individual health check functions
(define (check-disk-space machine-name)
  "Check if disk space is below critical threshold"
  (call-with-values 
      (lambda () (run-remote-command machine-name "df / | tail -1 | awk '{print $5}' | sed 's/%//'"))
    (lambda (success output)
      (if success
          (let ((usage (string->number (string-trim-right output))))
            (< usage 90)) ; Pass if usage < 90%
          #f))))

(define (check-system-load machine-name)
  "Check if system load is reasonable"
  (call-with-values 
      (lambda () (run-remote-command machine-name "cat /proc/loadavg | cut -d' ' -f1"))
    (lambda (success output)
      (if success
          (let ((load (string->number (string-trim-right output))))
            (< load 5.0)) ; Pass if load < 5.0
          #f))))

(define (check-critical-services machine-name)
  "Check that critical services are running"
  (let ((critical-services '("sshd")))
    (every (lambda (service)
             (call-with-values 
                 (lambda () (run-remote-command machine-name "systemctl is-active" service))
               (lambda (success output)
                 (and success (string=? (string-trim-right output) "active")))))
           critical-services)))

(define (check-network-connectivity machine-name)
  "Check basic network connectivity"
  (call-with-values 
      (lambda () (run-remote-command machine-name "ping -c 1 -W 5 8.8.8.8 > /dev/null 2>&1; echo $?"))
    (lambda (success output)
      (and success (string=? (string-trim-right output) "0")))))

;; Update flake inputs
(define (update-flake options)
  "Update flake inputs in the home lab repository"
  (let ((homelab-root (get-homelab-root))
        (dry-run (option-ref options 'dry-run #f)))
    
    (log-info "Updating flake inputs...")
    
    (if dry-run
        (begin
          (log-info "DRY RUN: Would execute: nix flake update")
          #t)
        (let* ((update-cmd (format #f "cd ~a && nix flake update" homelab-root))
               (port (open-pipe* OPEN_READ "/bin/sh" "-c" update-cmd))
               (output (get-string-all port))
               (status (close-pipe port)))
          
          (if (zero? status)
              (begin
                (log-success "Flake inputs updated successfully")
                (log-debug "Update output: ~a" output)
                #t)
              (begin
                (log-error "Flake update failed (exit: ~a)" status)
                (log-error "Error output: ~a" output)
                #f))))))

;; Validate home lab environment
(define (validate-environment)
  "Validate that the home lab environment is properly configured"
  (log-info "Validating home lab environment...")
  
  (let ((checks
         `(("homelab-root" . ,(lambda () (file-exists? (get-homelab-root))))
           ("flake-file" . ,(lambda () (file-exists? (string-append (get-homelab-root) "/flake.nix"))))
           ("ssh-config" . ,(lambda () (file-exists? (string-append (getenv "HOME") "/.ssh/config"))))
           ("nix-command" . ,(lambda () (zero? (system "which nix > /dev/null 2>&1"))))
           ("machines-config" . ,(lambda () (not (null? (get-all-machines))))))))
    
    (let ((results (map (lambda (check-pair)
                          (let ((check-name (car check-pair))
                                (check-proc (cdr check-pair)))
                            (let ((result (check-proc)))
                              (if result
                                  (log-success "✓ ~a" check-name)
                                  (log-error "✗ ~a" check-name))
                              `(,check-name . ,result))))
                        checks)))
      
      (let ((failures (filter (lambda (result) (not (cdr result))) results)))
        (if (null? failures)
            (begin
              (log-success "Environment validation passed")
              #t)
            (begin
              (log-error "Environment validation failed: ~a" (map car failures))
              #f))))))

;; Execute nixos-rebuild with proper error handling
(define (execute-nixos-rebuild machine-name mode options)
  "Execute nixos-rebuild on a machine with comprehensive error handling"
  (let ((homelab-root (get-homelab-root))
        (dry-run (option-ref options 'dry-run #f))
        (ssh-config (get-ssh-config machine-name)))
    
    (if (not ssh-config)
        (begin
          (log-error "No SSH configuration for machine: ~a" machine-name)
          #f)
        (let* ((is-local (assoc-ref ssh-config 'is-local))
               (flake-ref (format #f "~a#~a" homelab-root machine-name))
               (rebuild-cmd (if is-local
                               (format #f "sudo nixos-rebuild ~a --flake ~a" mode flake-ref)
                               (format #f "nixos-rebuild ~a --flake ~a --target-host ~a --use-remote-sudo" 
                                      mode flake-ref (assoc-ref ssh-config 'hostname)))))
          
          (log-info "Executing nixos-rebuild for ~a (mode: ~a)" machine-name mode)
          (log-debug "Command: ~a" rebuild-cmd)
          
          (if dry-run
              (begin
                (log-info "DRY RUN: Would execute: ~a" rebuild-cmd)
                #t)
              (with-spinner 
                (format #f "Rebuilding ~a" machine-name)
                (lambda ()
                  (let* ((port (open-pipe* OPEN_READ "/bin/sh" "-c" rebuild-cmd))
                         (output (get-string-all port))
                         (status (close-pipe port)))
                    
                    (if (zero? status)
                        (begin
                          (log-success "nixos-rebuild completed successfully for ~a" machine-name)
                          (log-debug "Build output: ~a" output)
                          #t)
                        (begin
                          (log-error "nixos-rebuild failed for ~a (exit: ~a)" machine-name status)
                          (log-error "Error output: ~a" output)
                          #f))))))))))
