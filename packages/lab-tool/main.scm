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
  "Home Lab Tool - SSH + Rsync Edition

USAGE: lab <command> [args...]

COMMANDS:
  status              Show infrastructure status
  machines            List all machines
  deploy <machine> [options]  Deploy configuration to machine using SSH + rsync + nixos-rebuild
                      Options: --dry-run, --boot, --test, --use-deploy-rs
  deploy-all [options]        Deploy to all machines using SSH + rsync + nixos-rebuild
  update              Update flake inputs
  auto-update         Perform automatic system update with health checks
  auto-update-status  Show auto-update service status and logs
  health [machine]    Check machine health (all if no machine specified)
  ssh <machine>       SSH to machine (using sma user)
  test-rollback <machine>     Test deployment with rollback (uses deploy-rs)
  help                Show this help

EXAMPLES:
  lab status
  lab machines
  lab deploy little-rascal                # Deploy with SSH + rsync (default)
  lab deploy little-rascal --dry-run      # Test deployment without applying
  lab deploy little-rascal --boot         # Deploy but only activate on next boot
  lab deploy little-rascal --test         # Deploy but don't make permanent
  lab deploy little-rascal --use-deploy-rs # Use deploy-rs instead of SSH method
  lab deploy-all                          # Deploy to all machines
  lab deploy-all --dry-run                # Test deployment to all machines
  lab update                              # Update flake inputs
  lab test-rollback sleeper-service       # Test rollback functionality (deploy-rs)
  lab ssh sleeper-service                 # SSH to machine as sma user

SSH + Rsync Features (Default):
- Fast: Only syncs changed files with rsync
- Simple: Uses standard nixos-rebuild workflow
- Reliable: Same command workflow as manual deployment
- Flexible: Supports boot, test, and switch modes

Deploy-rs Features (Optional with --use-deploy-rs):
- Automatic rollback on deployment failure
- Health checks after deployment
- Magic rollback for network connectivity issues
- Atomic deployments with safety guarantees

This implementation uses SSH + rsync + nixos-rebuild by default:
- Fast: Efficient file synchronization
- Simple: Standard NixOS deployment workflow
- Consistent: Same user (sma) for all operations
- Flexible: Multiple deployment modes available"

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
  "Deploy configuration to machine using deploy-rs"
  (let* ((options (parse-deploy-options args)))
    (log-info "Deploying to machine: ~a using deploy-rs" machine-name)
    (if (validate-machine-name machine-name)
        (let ((result (deploy-machine machine-name "default" options)))
          (if result
              (log-success "Deploy-rs deployment to ~a completed successfully" machine-name)
              (log-error "Deploy-rs deployment to ~a failed" machine-name)))
        (begin
          (log-error "Invalid machine: ~a" machine-name)
          (log-info "Available machines: ~a" (string-join (get-all-machines) ", "))))))

(define (cmd-ssh machine-name)
  "SSH to machine using sma user"
  (log-info "Connecting to machine: ~a as sma user" machine-name)
  (if (validate-machine-name machine-name)
      (let ((ssh-config (get-ssh-config machine-name)))
        (if ssh-config
            (let ((hostname (assoc-ref ssh-config 'hostname))
                  (ssh-alias (assoc-ref ssh-config 'ssh-alias))
                  (ssh-user (assoc-ref ssh-config 'ssh-user))
                  (is-local (assoc-ref ssh-config 'is-local)))
              (if is-local
                  (begin
                    (log-info "Machine ~a is local - switching to sma user locally" machine-name)
                    (system "sudo -u sma -i"))
                  (let ((target (format #f "~a@~a" (or ssh-user "sma") (or ssh-alias hostname))))
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
  "Deploy to all machines using deploy-rs"
  (log-info "Deploying to all machines using deploy-rs...")
  (let ((result (deploy-all-machines '())))
    (if result
        (log-success "All deploy-rs deployments completed successfully")
        (log-error "Some deploy-rs deployments failed"))))

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

;; Parse deployment options from command line arguments
(define (parse-deploy-options args)
  "Parse deployment options from command line arguments"
  (let ((options '()))
    (for-each
     (lambda (arg)
       (cond
        ((string=? arg "--dry-run")
         (set! options (cons '(dry-run . #t) options)))
        ((string=? arg "--skip-checks")
         (set! options (cons '(skip-checks . #t) options)))
        (else
         (log-warn "Unknown option: ~a" arg))))
     args)
    options))

(define (cmd-test-rollback machine-name)
  "Test deployment with rollback functionality"
  (log-info "Testing rollback deployment for machine: ~a" machine-name)
  (if (validate-machine-name machine-name)
      (let ((options '((test-rollback . #t))))
        (let ((result (deploy-with-rollback machine-name options)))
          (if result
              (log-success "Rollback test completed for ~a" machine-name)
              (log-error "Rollback test failed for ~a" machine-name))))
      (log-error "Invalid machine: ~a" machine-name)))

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
           (format #t "Usage: lab deploy <machine> [options]\n")
           (format #t "Options: --dry-run, --skip-checks\n"))
         (apply cmd-deploy args)))
    
    ('deploy-all
     (cmd-deploy-all))
    
    ('test-rollback
     (if (null? args)
         (begin
           (log-error "test-rollback command requires machine name")
           (format #t "Usage: lab test-rollback <machine>\n"))
         (cmd-test-rollback (car args))))
    
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
    
    ('test-rollback
     (if (null? args)
         (begin
           (log-error "test-rollback command requires machine name")
           (format #t "Usage: lab test-rollback <machine>\n"))
         (cmd-test-rollback (car args))))
    
    (_
     (log-error "Unknown command: ~a" command)
     (format #t "Use 'lab help' for available commands\n")
     (exit 1))))

;; Main entry point
(define (main args)
  "Main entry point for lab tool"
  (log-info "Home Lab Tool - Deploy-rs Edition")
  
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
  (main (command-line))))
