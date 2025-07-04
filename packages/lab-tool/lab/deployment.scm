;; lab/deployment.scm - Unified deployment operations (SSH + rsync by default)

(define-module (lab deployment)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:use-module (lab ssh-deploy)
  #:use-module (lab deploy-rs)
  #:export (deploy-machine
            update-flake
            deploy-all-machines
            deploy-with-rollback
            option-ref))

;; Helper function for option handling (re-exported from ssh-deploy)
(define (option-ref options key default)
  "Get option value with default fallback"
  (let ((value (assoc-ref options key)))
    (if value value default)))

;; Main deployment function - SSH by default, deploy-rs optional
(define (deploy-machine machine-name . args)
  "Deploy configuration to machine using SSH + rsync (default) or deploy-rs (optional)"
  (let* ((mode (if (null? args) "default" (car args)))
         (options (if (< (length args) 2) '() (cadr args)))
         (use-deploy-rs (option-ref options 'use-deploy-rs #f)))
    
    (if (not (validate-machine-name machine-name))
        #f
        (if use-deploy-rs
            (begin
              (log-info "Using deploy-rs deployment method")
              (deploy-machine-deploy-rs machine-name mode options))
            (begin
              (log-info "Using SSH + rsync deployment method")
              (deploy-machine-ssh machine-name mode options))))))

;; Deploy to all machines - delegate to appropriate module
(define (deploy-all-machines . args)
  "Deploy to all machines using SSH + rsync (default) or deploy-rs (optional)"
  (let* ((options (if (null? args) '() (car args)))
         (use-deploy-rs (option-ref options 'use-deploy-rs #f)))
    
    (if use-deploy-rs
        (begin
          (log-info "Using deploy-rs for all machines")
          (deploy-all-machines-deploy-rs options))
        (begin
          (log-info "Using SSH + rsync for all machines")
          (deploy-all-machines-ssh options)))))

;; Deploy with rollback testing - only available with deploy-rs
(define (deploy-with-rollback machine-name . args)
  "Deploy with explicit rollback capability testing (deploy-rs only)"
  (let* ((options (if (null? args) '() (car args)))
         (modified-options (cons '(use-deploy-rs . #t) options)))
    
    (log-info "Rollback testing requires deploy-rs - switching to deploy-rs mode")
    (deploy-with-rollback machine-name modified-options)))

;; Update flake inputs - delegate to ssh-deploy module  
(define update-flake
  "Update flake inputs (impure - has side effects)"
  (@ (lab ssh-deploy) update-flake))
