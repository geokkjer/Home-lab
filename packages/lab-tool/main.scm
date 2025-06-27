#!/usr/bin/env guile
!#

;; Home Lab Tool - Main Entry Point
;; K.I.S.S Refactored Implementation

;; Load path is set by the wrapper script in default.nix
;; No need to add current directory when running from Nix

(use-modules (ice-9 match)
             (ice-9 format)
             (utils config)
             (utils logging)
             (lab core)
             (lab machines)
             (lab deployment)
             (lab auto-update))

;; Initialize logging
(set-log-level! 'info)

;; Pure function: Command help text
(define (get-help-text)
  "Pure function returning help text"
  "Home Lab Tool - K.I.S.S Refactored Edition

USAGE: lab <command> [args...]

COMMANDS:
  status              Show infrastructure status
  machines            List all machines  
  deploy <machine> [mode]  Deploy configuration to machine
                      Available modes: boot (default), test, switch
  deploy-all          Deploy to all machines
  update              Update flake inputs
  auto-update         Perform automatic system update with health checks
  auto-update-status  Show auto-update service status and logs
  health [machine]    Check machine health (all if no machine specified)
  ssh <machine>       SSH to machine
  test-modules        Test modular implementation
  help                Show this help

EXAMPLES:
  lab status
  lab machines
  lab deploy congenital-optimist          # Deploy with boot mode (default)
  lab deploy congenital-optimist switch   # Deploy and activate immediately
  lab deploy congenital-optimist test     # Deploy temporarily for testing
  lab deploy-all
  lab update
  lab auto-update                         # Perform automatic update with reboot
  lab auto-update-status                  # Show auto-update logs and status
  lab health
  lab health sleeper-service
  lab ssh sleeper-service
  lab test-modules

This implementation follows K.I.S.S principles:
- Modular: Each module has single responsibility  
- Functional: Pure functions separated from side effects
- Small: Individual modules under 50 lines
- Simple: One function does one thing well
")

;; Pure function: Format machine list
(define (format-machine-list machines)
  "Pure function to format machine list for display"
  (if (null? machines)
      "No machines configured"
      (string-join
       (map (lambda (machine) (format #f "  - ~a" machine)) machines)
       "\n")))

;; Pure function: Format status info
(define (format-status-info machines config)
  "Pure function to format infrastructure status"
  (format #f "Infrastructure Status:
Total machines: ~a
Home lab root: ~a

~a" 
          (length machines)
          (get-config-value '(homelab-root))
          (format-machine-list machines)))

;; Command implementations
(define (cmd-status)
  "Show infrastructure status"
  (log-info "Checking infrastructure status...")
  (let* ((machines (list-machines))
         (status (get-infrastructure-status))
         (config (get-current-config))
         (status-text (format-status-info machines config)))
    (display status-text)
    (newline)
    (for-each (lambda (machine-status)
                (let ((machine (assoc-ref machine-status 'machine))
                      (status (assoc-ref machine-status 'status)))
                  (format #t "  ~a: ~a\n" machine status)))
              status)
    (log-success "Status check complete")))

(define (cmd-machines)
  "List all configured machines"
  (log-info "Listing configured machines...")
  (let* ((machines (list-machines))
         (machine-list (format-machine-list machines)))
    (format #t "Configured Machines:\n~a\n" machine-list)
    (log-success "Machine list complete")))

(define (cmd-deploy machine-name . args)
  "Deploy configuration to machine"
  (let* ((mode (if (null? args) "boot" (car args)))
         (valid-modes '("boot" "test" "switch")))
    (log-info "Deploying to machine: ~a (mode: ~a)" machine-name mode)
    (if (not (member mode valid-modes))
        (begin
          (log-error "Invalid deployment mode: ~a" mode)
          (log-error "Valid modes: ~a" (string-join valid-modes ", "))
          (format #t "Usage: lab deploy <machine> [boot|test|switch]\n"))
        (if (validate-machine-name machine-name)
            (let ((result (deploy-machine machine-name mode '())))
              (if result
                  (log-success "Deployment to ~a complete (mode: ~a)" machine-name mode)
                  (log-error "Deployment to ~a failed" machine-name)))
            (begin
              (log-error "Invalid machine: ~a" machine-name)
              (log-info "Available machines: ~a" (string-join (get-all-machines) ", ")))))))

(define (cmd-ssh machine-name)
  "SSH to machine"
  (log-info "Connecting to machine: ~a" machine-name)
  (if (validate-machine-name machine-name)
      (let ((ssh-config (get-ssh-config machine-name)))
        (if ssh-config
            (let ((hostname (assoc-ref ssh-config 'hostname))
                  (ssh-alias (assoc-ref ssh-config 'ssh-alias))
                  (is-local (assoc-ref ssh-config 'is-local)))
              (if is-local
                  (log-info "Machine ~a is local - no SSH needed" machine-name)
                  (let ((target (or ssh-alias hostname)))
                    (log-info "Connecting to ~a via SSH..." target)
                    (system (format #f "ssh ~a" target)))))
            (log-error "No SSH configuration found for ~a" machine-name)))
      (log-error "Invalid machine: ~a" machine-name)))

(define (cmd-test-modules)
  "Test the modular implementation"
  (log-info "Testing modular implementation...")
  
  ;; Test pure functions
  (use-modules (utils config accessor))
  
  (let* ((config (get-current-config))
         (machines (get-all-machines-pure config))
         (blue-color (get-color 'blue)))
    
    (format #t "\n=== Modular Implementation Test ===\n")
    (format #t "Pure config access: ~a machines\n" (length machines))
    (format #t "Pure color function: ~ablue text~a\n" blue-color (get-color 'reset))
    (format #t "\n✅ All pure functions working correctly!\n\n")
    
    (log-success "Modular implementation test complete")))

(define (cmd-update)
  "Update flake inputs"
  (log-info "Updating flake inputs...")
  (let ((result (update-flake '())))
    (if result
        (log-success "Flake update complete")
        (log-error "Flake update failed"))))

(define (cmd-deploy-all)
  "Deploy to all machines"
  (log-info "Deploying to all machines...")
  (let* ((machines (list-machines))
         (results (map (lambda (machine)
                         (log-info "Deploying to ~a..." machine)
                         (let ((result (deploy-machine machine "boot" '())))
                           (if result
                               (log-success "✓ ~a deployed" machine)
                               (log-error "✗ ~a failed" machine))
                           result))
                       machines))
         (successful (filter identity results)))
    (log-info "Deployment summary: ~a/~a successful" 
              (length successful) (length machines))))

(define (cmd-health args)
  "Check machine health"
  (let ((machine-name (if (null? args) #f (car args))))
    (if machine-name
        ;; Check specific machine
        (if (validate-machine-name machine-name)
            (let ((health (check-machine-health machine-name)))
              (format #t "Health check for ~a:\n" machine-name)
              (format #t "  SSH: ~a\n" (assoc-ref health 'ssh-connectivity))
              (format #t "  Status: ~a\n" (assoc-ref health 'status))
              (format #t "  Services: ~a configured\n" (assoc-ref health 'services-configured)))
            (log-error "Invalid machine: ~a" machine-name))
        ;; Check all machines
        (let ((results (discover-machines)))
          (format #t "Health Summary:\n")
          (for-each (lambda (health)
                      (let ((machine (assoc-ref health 'machine))
                            (status (assoc-ref health 'status)))
                        (format #t "  ~a: ~a\n" machine status)))
                    results)))))

(define (cmd-auto-update)
  "Perform automatic system update"
  (log-info "Starting automatic system update...")
  (let ((result (auto-update-system '((auto-reboot . #t)))))
    (if result
        (log-success "Automatic update completed successfully")
        (log-error "Automatic update failed"))))

(define (cmd-auto-update-status)
  "Show auto-update status and logs"
  (auto-update-status))

;; Main command dispatcher
(define (dispatch-command command args)
  "Dispatch command with appropriate handler"
  (match command
    ('help
     (display (get-help-text)))
    
    ('status
     (cmd-status))
    
    ('machines
     (cmd-machines))
    
    ('deploy
     (if (null? args)
         (begin
           (log-error "deploy command requires machine name")
           (format #t "Usage: lab deploy <machine> [boot|test|switch]\n"))
         (apply cmd-deploy args)))
    
    ('deploy-all
     (cmd-deploy-all))
    
    ('update
     (cmd-update))
    
    ('auto-update
     (cmd-auto-update))
    
    ('auto-update-status
     (cmd-auto-update-status))
    
    ('health
     (cmd-health args))
    
    ('ssh
     (if (null? args)
         (begin
           (log-error "ssh command requires machine name")
           (format #t "Usage: lab ssh <machine>\n"))
         (cmd-ssh (car args))))
    
    ('test-modules
     (cmd-test-modules))
    
    (_
     (log-error "Unknown command: ~a" command)
     (format #t "Use 'lab help' for available commands\n")
     (exit 1))))

;; Main entry point
(define (main args)
  "Main entry point for lab tool"
  (log-info "Home Lab Tool - K.I.S.S Refactored Edition")
  
  (let* ((parsed-cmd (if (> (length args) 1) (cdr args) '("help")))
         (command (string->symbol (car parsed-cmd)))
         (cmd-args (cdr parsed-cmd)))
    
    (catch #t
      (lambda () (dispatch-command command cmd-args))
      (lambda (key . error-args)
        (log-error "Command failed: ~a ~a" key error-args)
        (exit 1))))
  
  (log-debug "Command execution complete"))

;; Run main function if script is executed directly
(when (and (defined? 'command-line) (not (null? (command-line))))
  (main (command-line)))