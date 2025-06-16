;; lab/core/deployment.scm - Deployment functionality

(define-module (lab core deployment)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (lab core logging)
  #:use-module (lab core config)
  #:use-module (lab core utils)
  #:export (update-flake
            validate-environment
            execute-nixos-rebuild))

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
                          #f))))))))
