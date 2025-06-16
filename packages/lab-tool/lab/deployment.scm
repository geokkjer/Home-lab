;; lab/deployment.scm - Deployment operations (impure)

(define-module (lab deployment)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:use-module (utils ssh)
  #:export (deploy-machine
            update-flake
            execute-nixos-rebuild
            option-ref))

;; Helper function for option handling
(define (option-ref options key default)
  "Get option value with default fallback"
  (let ((value (assoc-ref options key)))
    (if value value default)))

;; Impure function: Deploy machine configuration
(define (deploy-machine machine-name . args)
  "Deploy configuration to machine (impure - has side effects)"
  (let* ((mode (if (null? args) "boot" (car args)))
         (options (if (< (length args) 2) '() (cadr args)))
         (valid-modes '("boot" "test" "switch"))
         (dry-run (option-ref options 'dry-run #f)))
    
    (if (not (validate-machine-name machine-name))
        #f
        (if (not (member mode valid-modes))
            (begin
              (log-error "Invalid deployment mode: ~a" mode)
              (log-error "Valid modes: ~a" (string-join valid-modes ", "))
              #f)
            (begin
              (log-info "Starting deployment: ~a (mode: ~a)" machine-name mode)
              (execute-nixos-rebuild machine-name mode options))))))

;; Impure function: Update flake inputs
(define (update-flake . args)
  "Update flake inputs (impure - has side effects)"
  (let* ((options (if (null? args) '() (car args)))
         (dry-run (option-ref options 'dry-run #f)))
    
    (log-info "Updating flake inputs...")
    
    (if dry-run
        (begin
          (log-info "DRY RUN: Would execute: nix flake update")
          #t)
        (let* ((homelab-root (get-homelab-root))
               (update-cmd (format #f "cd ~a && nix flake update" homelab-root))
               (port (open-pipe* OPEN_READ "/bin/sh" "-c" update-cmd))
               (output (get-string-all port))
               (status (close-pipe port)))
          
          (if (zero? status)
              (begin
                (log-success "Flake inputs updated successfully")
                #t)
              (begin
                (log-error "Flake update failed (exit: ~a)" status)
                (log-error "Error output: ~a" output)
                #f))))))

;; Impure function: Execute nixos-rebuild
(define (execute-nixos-rebuild machine-name mode options)
  "Execute nixos-rebuild command (impure - has side effects)"
  (let* ((dry-run (option-ref options 'dry-run #f))
         (ssh-config (get-ssh-config machine-name))
         (is-local (and ssh-config (assoc-ref ssh-config 'is-local)))
         (homelab-root (get-homelab-root)))
    
    (if is-local
        ;; Local deployment
        (let ((rebuild-cmd (format #f "sudo nixos-rebuild ~a --flake ~a#~a" 
                                  mode homelab-root machine-name)))
          (log-debug "Local rebuild command: ~a" rebuild-cmd)
          
          (if dry-run
              (begin
                (log-info "DRY RUN: Would execute: ~a" rebuild-cmd)
                #t)
              (let* ((port (open-pipe* OPEN_READ "/bin/sh" "-c" rebuild-cmd))
                     (output (get-string-all port))
                     (status (close-pipe port)))
                
                (if (zero? status)
                    (begin
                      (log-success "Local nixos-rebuild completed")
                      #t)
                    (begin
                      (log-error "Local nixos-rebuild failed (exit: ~a)" status)
                      #f)))))
        
        ;; Remote deployment
        (let* ((hostname (assoc-ref ssh-config 'hostname))
               (ssh-alias (or (assoc-ref ssh-config 'ssh-alias) hostname))
               (temp-dir "/tmp/homelab-deploy")
               (sync-cmd (format #f "rsync -av --delete ~a/ ~a:~a/"
                                homelab-root ssh-alias temp-dir))
               (rebuild-cmd (format #f "ssh ~a 'cd ~a && sudo nixos-rebuild ~a --flake .#~a'"
                                   ssh-alias temp-dir mode machine-name)))
          
          (log-debug "Remote sync command: ~a" sync-cmd)
          (log-debug "Remote rebuild command: ~a" rebuild-cmd)
          
          (if dry-run
              (begin
                (log-info "DRY RUN: Would sync and rebuild remotely")
                #t)
              (begin
                ;; Sync configuration
                (log-info "Syncing configuration to ~a..." machine-name)
                (let* ((sync-port (open-pipe* OPEN_READ "/bin/sh" "-c" sync-cmd))
                       (sync-output (get-string-all sync-port))
                       (sync-status (close-pipe sync-port)))
                  
                  (if (zero? sync-status)
                      (begin
                        (log-success "Configuration synced")
                        ;; Execute rebuild
                        (log-info "Executing nixos-rebuild on ~a..." machine-name)
                        (let* ((rebuild-port (open-pipe* OPEN_READ "/bin/sh" "-c" rebuild-cmd))
                               (rebuild-output (get-string-all rebuild-port))
                               (rebuild-status (close-pipe rebuild-port)))
                          
                          (if (zero? rebuild-status)
                              (begin
                                (log-success "Remote nixos-rebuild completed")
                                #t)
                              (begin
                                (log-error "Remote nixos-rebuild failed (exit: ~a)" rebuild-status)
                                #f))))
                      (begin
                        (log-error "Configuration sync failed (exit: ~a)" sync-status)
                        #f)))))))))
