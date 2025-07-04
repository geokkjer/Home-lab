;; lab/ssh-deploy.scm - SSH + rsync + nixos-rebuild deployment operations

(define-module (lab ssh-deploy)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:use-module (utils ssh)
  #:export (deploy-machine-ssh
            deploy-all-machines-ssh
            update-flake
            sync-config-to-machine
            option-ref))

;; Helper function for option handling
(define (option-ref options key default)
  "Get option value with default fallback"
  (let ((value (assoc-ref options key)))
    (if value value default)))

;; Main SSH deployment function
(define (deploy-machine-ssh machine-name . args)
  "Deploy configuration to machine using SSH + rsync + nixos-rebuild"
  (let* ((mode (if (null? args) "default" (car args)))
         (options (if (< (length args) 2) '() (cadr args)))
         (dry-run (option-ref options 'dry-run #f))
         (boot-mode (option-ref options 'boot #f)))
    
    (if (not (validate-machine-name machine-name))
        #f
        (begin
          (log-info "Starting SSH deployment: ~a" machine-name)
          (execute-ssh-deploy machine-name mode options)))))

;; Execute SSH-based deployment
(define (execute-ssh-deploy machine-name mode options)
  "Execute deployment using SSH + rsync + nixos-rebuild"
  (let* ((homelab-root (get-homelab-root))
         (dry-run (option-ref options 'dry-run #f))
         (boot-mode (option-ref options 'boot #f))
         (test-mode (option-ref options 'test #f))
         (remote-path "/tmp/home-lab-config"))
    
    (log-info "Deploying ~a using SSH + rsync + nixos-rebuild..." machine-name)
    
    (if dry-run
        (begin
          (log-info "DRY RUN: Would sync config and rebuild ~a" machine-name)
          (log-info "Would execute: rsync + nixos-rebuild --flake /tmp/home-lab-config#~a" machine-name)
          #t)
        (let ((start-time (current-time)))
          
          ;; Step 1: Sync configuration to remote machine
          (log-info "Step 1: Syncing configuration to ~a:~a" machine-name remote-path)
          (if (sync-config-to-machine machine-name remote-path)
              ;; Step 2: Execute nixos-rebuild on remote machine
              (begin
                (log-info "Step 2: Executing nixos-rebuild on ~a" machine-name)
                (execute-remote-rebuild machine-name remote-path boot-mode test-mode start-time))
              (begin
                (log-error "Failed to sync configuration to ~a" machine-name)
                #f))))))

;; Sync configuration to remote machine
(define (sync-config-to-machine machine-name remote-path)
  "Sync Home-lab configuration to remote machine using rsync"
  (let* ((homelab-root (get-homelab-root))
         (ssh-config (get-ssh-config machine-name)))
    
    (if (not ssh-config)
        (begin
          (log-error "No SSH configuration found for ~a" machine-name)
          #f)
        (if (assoc-ref ssh-config 'is-local)
            ;; Local "sync" - just ensure path exists
            (begin
              (log-debug "Local machine ~a, copying to ~a" machine-name remote-path)
              (let* ((cp-cmd (format #f "sudo mkdir -p ~a && sudo cp -r ~a/* ~a/" 
                                     remote-path homelab-root remote-path))
                     (status (system cp-cmd)))
                (if (zero? status)
                    (begin
                      (log-debug "Local configuration copied successfully")
                      #t)
                    (begin
                      (log-error "Local configuration copy failed (exit: ~a)" status)
                      #f))))
            ;; Remote sync using rsync
            (let* ((hostname (assoc-ref ssh-config 'hostname))
                   (ssh-alias (assoc-ref ssh-config 'ssh-alias))
                   (user (assoc-ref ssh-config 'user))
                   (identity-file (assoc-ref ssh-config 'identity-file))
                   (target (if user (format #f "~a@~a" user (or ssh-alias hostname)) (or ssh-alias hostname)))
                   (key-arg (if identity-file (format #f "-i ~a" identity-file) ""))
                   (rsync-cmd (format #f "rsync -avz --delete -e 'ssh ~a' ~a/ ~a:~a/" 
                                      key-arg homelab-root target remote-path)))
              
              (log-debug "Rsync command: ~a" rsync-cmd)
              (let* ((port (open-pipe* OPEN_READ "/bin/sh" "-c" rsync-cmd))
                     (output (get-string-all port))
                     (status (close-pipe port)))
                
                (if (zero? status)
                    (begin
                      (log-debug "Configuration synced successfully")
                      (log-debug "Rsync output: ~a" output)
                      #t)
                    (begin
                      (log-error "Configuration sync failed (exit: ~a)" status)
                      (log-error "Rsync error: ~a" output)
                      #f))))))))

;; Execute nixos-rebuild on remote machine
(define (execute-remote-rebuild machine-name remote-path boot-mode test-mode start-time)
  "Execute nixos-rebuild on the remote machine"
  (let* ((rebuild-mode (cond
                        (test-mode "test")
                        (boot-mode "boot") 
                        (else "switch")))
         (rebuild-cmd (format #f "sudo nixos-rebuild ~a --flake ~a#~a" 
                              rebuild-mode remote-path machine-name)))
    
    (log-info "Executing: ~a" rebuild-cmd)
    
    (call-with-values
        (lambda () (run-remote-command machine-name rebuild-cmd))
      (lambda (success output)
        (let ((elapsed (- (current-time) start-time)))
          (if success
              (begin
                (log-success "SSH deployment completed successfully in ~as" elapsed)
                (log-info "Rebuild output:")
                (log-info "~a" output)
                #t)
              (begin
                (log-error "SSH deployment failed (exit code indicates failure)")
                (log-error "Rebuild error output:")
                (log-error "~a" output)
                #f)))))))

;; Deploy to all machines using SSH
(define (deploy-all-machines-ssh . args)
  "Deploy to all machines using SSH + rsync + nixos-rebuild"
  (let* ((options (if (null? args) '() (car args)))
         (dry-run (option-ref options 'dry-run #f))
         (machines (get-all-machines)))
    
    (log-info "Starting SSH deployment to all machines (~a total)" (length machines))
    
    (let ((results 
           (map (lambda (machine)
                  (log-info "Deploying to ~a..." machine)
                  (let ((result (deploy-machine-ssh machine "default" options)))
                    (if result
                        (log-success "✓ ~a deployed successfully" machine)
                        (log-error "✗ ~a deployment failed" machine))
                    (cons machine result)))
                machines)))
      
      ;; Summary
      (let ((successful (filter cdr results))
            (failed (filter (lambda (r) (not (cdr r))) results)))
        (log-info "SSH deployment summary:")
        (log-info "  Successful: ~a/~a machines" (length successful) (length machines))
        (when (not (null? failed))
          (log-error "  Failed: ~a" (string-join (map car failed) ", ")))
        
        ;; Return true if all succeeded
        (= (length successful) (length machines))))))

;; Update flake inputs
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