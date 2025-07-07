;; lab/deploy-rs.scm - Deploy-rs based deployment operations (extracted)

(define-module (lab deploy-rs)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:export (deploy-machine-deploy-rs
            deploy-all-machines-deploy-rs
            deploy-with-rollback
            option-ref))

;; Helper function for option handling
(define (option-ref options key default)
  "Get option value with default fallback"
  (let ((value (assoc-ref options key)))
    (if value value default)))

;; Main deployment function using deploy-rs
(define (deploy-machine-deploy-rs machine-name . args)
  "Deploy configuration to machine using deploy-rs (impure - has side effects)"
  (let* ((mode (if (null? args) "default" (car args)))
         (options (if (< (length args) 2) '() (cadr args)))
         (dry-run (option-ref options 'dry-run #f))
         (skip-checks (option-ref options 'skip-checks #f)))
    
    (if (not (validate-machine-name machine-name))
        #f
        (begin
          (log-info "Starting deploy-rs deployment: ~a" machine-name)
          (execute-deploy-rs machine-name mode options)))))

;; Execute deploy-rs deployment
(define (execute-deploy-rs machine-name mode options)
  "Execute deployment using deploy-rs with automatic rollback"
  (let* ((homelab-root (get-homelab-root))
         (dry-run (option-ref options 'dry-run #f))
         (skip-checks (option-ref options 'skip-checks #f))
         (auto-rollback (option-ref options 'auto-rollback #t))
         (magic-rollback (option-ref options 'magic-rollback #t)))
    
    (log-info "Deploying ~a using deploy-rs..." machine-name)
    
    (if dry-run
        (begin
          (log-info "DRY RUN: Would execute deploy-rs for ~a" machine-name)
          (log-info "Command would be: deploy '.#~a'" machine-name)
          #t)
        (let* ((deploy-cmd (build-deploy-command machine-name skip-checks auto-rollback magic-rollback))
               (start-time (current-time)))
          
          (log-info "Deploy command: ~a" deploy-cmd)
          (log-info "Executing deployment with automatic rollback protection...")
          
          (let* ((port (open-pipe* OPEN_READ "/bin/sh" "-c" deploy-cmd))
                 (output (get-string-all port))
                 (status (close-pipe port))
                 (elapsed (- (current-time) start-time)))
            
            (if (zero? status)
                (begin
                  (log-success "Deploy-rs deployment completed successfully in ~as" elapsed)
                  (log-info "Deployment output:")
                  (log-info "~a" output)
                  #t)
                (begin
                  (log-error "Deploy-rs deployment failed (exit: ~a)" status)
                  (log-error "Error output:")
                  (log-error "~a" output)
                  (log-info "Deploy-rs automatic rollback should have been triggered")
                  #f)))))))

;; Build deploy-rs command with options
(define (build-deploy-command machine-name skip-checks auto-rollback magic-rollback)
  "Build the deploy-rs command with appropriate flags"
  (let ((base-cmd (format #f "cd ~a && deploy '.#~a'" (get-homelab-root) machine-name))
        (flags '()))
    
    ;; Add flags based on options
    (when skip-checks
      (set! flags (cons "--skip-checks" flags)))
    
    (when auto-rollback
      (set! flags (cons "--auto-rollback=true" flags)))
    
    (when magic-rollback
      (set! flags (cons "--magic-rollback=true" flags)))
    
    ;; Combine command with flags
    (if (null? flags)
        base-cmd
        (format #f "~a ~a" base-cmd (string-join (reverse flags) " ")))))

;; Deploy to all machines
(define (deploy-all-machines-deploy-rs . args)
  "Deploy to all machines using deploy-rs"
  (let* ((options (if (null? args) '() (car args)))
         (dry-run (option-ref options 'dry-run #f))
         (machines (get-all-machines)))
    
    (log-info "Starting deployment to all machines (~a total)" (length machines))
    
    (let ((results 
           (map (lambda (machine)
                  (log-info "Deploying to ~a..." machine)
                  (let ((result (deploy-machine-deploy-rs machine "default" options)))
                    (if result
                        (log-success "✓ ~a deployed successfully" machine)
                        (log-error "✗ ~a deployment failed" machine))
                    (cons machine result)))
                machines)))
      
      ;; Summary
      (let ((successful (filter cdr results))
            (failed (filter (lambda (r) (not (cdr r))) results)))
        (log-info "Deployment summary:")
        (log-info "  Successful: ~a/~a machines" (length successful) (length machines))
        (when (not (null? failed))
          (log-error "  Failed: ~a" (string-join (map car failed) ", ")))
        
        ;; Return true if all succeeded
        (= (length successful) (length machines))))))

;; Deploy with explicit rollback testing
(define (deploy-with-rollback machine-name . args)
  "Deploy with explicit rollback capability testing"
  (let* ((options (if (null? args) '() (car args)))
         (test-rollback (option-ref options 'test-rollback #f)))
    
    (log-info "Deploying ~a with rollback testing..." machine-name)
    
    (if test-rollback
        (begin
          (log-info "Testing rollback mechanism (deploy will be reverted)")
          ;; Deploy with magic rollback disabled to test manual rollback
          (let ((modified-options (cons '(magic-rollback . #f) options)))
            (execute-deploy-rs machine-name "default" modified-options)))
        (execute-deploy-rs machine-name "default" options))))