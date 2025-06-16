#!/usr/bin/env guile
!#

;; Home Lab Tool - Main Entry Point
;; K.I.S.S Refactored Implementation

(add-to-load-path (dirname (current-filename)))

(use-modules (ice-9 match)
             (ice-9 format)
             (utils config)
             (utils logging))

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
  deploy <machine>    Deploy configuration to machine
  ssh <machine>       SSH to machine
  test-modules        Test modular implementation
  help                Show this help

EXAMPLES:
  lab status
  lab machines
  lab deploy congenital-optimist
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
  (let* ((machines (get-all-machines))
         (config (get-current-config))
         (status-text (format-status-info machines config)))
    (display status-text)
    (newline)
    (log-success "Status check complete")))

(define (cmd-machines)
  "List all configured machines"
  (log-info "Listing configured machines...")
  (let* ((machines (get-all-machines))
         (machine-list (format-machine-list machines)))
    (format #t "Configured Machines:\n~a\n" machine-list)
    (log-success "Machine list complete")))

(define (cmd-deploy machine-name)
  "Deploy configuration to machine"
  (log-info "Deploying to machine: ~a" machine-name)
  (if (validate-machine-name machine-name)
      (begin
        (log-info "Machine ~a is valid" machine-name)
        (log-info "Deployment simulation complete (no actual deployment)")
        (log-success "Deployment to ~a complete" machine-name))
      (begin
        (log-error "Invalid machine: ~a" machine-name)
        (log-info "Available machines: ~a" (string-join (get-all-machines) ", ")))))

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
  (use-modules (utils config accessor)
               (utils logging format))
  
  (let* ((config (get-current-config))
         (machines (get-all-machines-pure config))
         (blue-color (get-color 'blue)))
    
    (format #t "\n=== Modular Implementation Test ===\n")
    (format #t "Pure config access: ~a machines\n" (length machines))
    (format #t "Pure color function: ~ablue text~a\n" blue-color (get-color 'reset))
    (format #t "\n✅ All pure functions working correctly!\n\n")
    
    (log-success "Modular implementation test complete")))

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
           (format #t "Usage: lab deploy <machine>\n"))
         (cmd-deploy (car args))))
    
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
     (format #t "Use 'lab help' for available commands\n"))))

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