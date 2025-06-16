;; lab/deployment.scm - Deployment operations for Home Lab Tool

(define-module (lab deployment)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:use-module (utils ssh)
  #:use-module (lab core)
  #:export (deploy-machine
            update-all-machines
            hybrid-update
            validate-deployment
            rollback-deployment
            deployment-status
            option-ref))

;; Helper function for option handling
(define (option-ref options key default)
  "Get option value with default fallback"
  (let ((value (assoc-ref options key)))
    (if value value default)))

;; Deploy configuration to a specific machine
(define (deploy-machine machine-name mode options)
  "Deploy NixOS configuration to a specific machine"
  (let ((valid-modes '("boot" "test" "switch"))
        (dry-run (option-ref options 'dry-run #f)))
    
    ;; Validate inputs
    (if (not (validate-machine-name machine-name))
        #f
        (if (not (member mode valid-modes))
            (begin
              (log-error "Invalid deployment mode: ~a" mode)
              (log-error "Valid modes: ~a" (string-join valid-modes ", "))
              #f)
            
            ;; Proceed with deployment
            (begin
              (log-info "Starting deployment: ~a -> ~a (mode: ~a)" 
                       machine-name machine-name mode)
              
              ;; Pre-deployment validation
              (if (not (validate-deployment-prerequisites machine-name))
                  (begin
                    (log-error "Pre-deployment validation failed")
                    #f)
                  
                  ;; Execute deployment
                  (let ((deployment-result 
                         (execute-deployment machine-name mode options)))
                    
                    ;; Post-deployment validation
                    (if deployment-result
                        (begin
                          (log-info "Deployment completed, validating...")
                          (if (validate-post-deployment machine-name mode)
                              (begin
                                (log-success "Deployment successful and validated ✓")
                                #t)
                              (begin
                                (log-warn "Deployment completed but validation failed")
                                ;; Don't fail completely - deployment might still be functional
                                #t)))
                        (begin
                          (log-error "Deployment failed")
                          #f)))))))))

;; Execute the actual deployment
(define (execute-deployment machine-name mode options)
  "Execute the deployment based on machine type and configuration"
  (let ((ssh-config (get-ssh-config machine-name))
        (machine-config (get-machine-config machine-name)))
    
    (if (not ssh-config)
        (begin
          (log-error "No SSH configuration found for ~a" machine-name)
          #f)
        
        (let ((deployment-method (assoc-ref machine-config 'deployment-method))
              (is-local (assoc-ref ssh-config 'is-local)))
          
          (log-debug "Using deployment method: ~a" (or deployment-method 'nixos-rebuild))
          
          (match (or deployment-method 'nixos-rebuild)
            ('nixos-rebuild
             (execute-nixos-rebuild machine-name mode options))
            
            ('deploy-rs
             (execute-deploy-rs machine-name mode options))
            
            ('hybrid
             (execute-hybrid-deployment machine-name mode options))
            
            (method
             (log-error "Unknown deployment method: ~a" method)
             #f))))))

;; Execute deploy-rs deployment
(define (execute-deploy-rs machine-name mode options)
  "Deploy using deploy-rs for atomic deployments"
  (let ((homelab-root (get-homelab-root))
        (dry-run (option-ref options 'dry-run #f)))
    
    (log-info "Deploying ~a using deploy-rs..." machine-name)
    
    (if dry-run
        (begin
          (log-info "DRY RUN: Would execute deploy-rs for ~a" machine-name)
          #t)
        
        (let* ((deploy-cmd (format #f "cd ~a && deploy '.#~a' --magic-rollback --auto-rollback" 
                                  homelab-root machine-name))
               (start-time (current-time)))
          
          (log-debug "Deploy command: ~a" deploy-cmd)
          
          (with-spinner
            (format #f "Deploying ~a with deploy-rs" machine-name)
            (lambda ()
              (let* ((port (open-pipe* OPEN_READ "/bin/sh" "-c" deploy-cmd))
                     (output (get-string-all port))
                     (status (close-pipe port))
                     (elapsed (- (current-time) start-time)))
                
                (if (zero? status)
                    (begin
                      (log-success "deploy-rs completed in ~as" elapsed)
                      (log-debug "Deploy output: ~a" output)
                      #t)
                    (begin
                      (log-error "deploy-rs failed (exit: ~a)" status)
                      (log-error "Error output: ~a" output)
                      #f)))))))))

;; Execute hybrid deployment (flake update + deploy)
(define (execute-hybrid-deployment machine-name mode options)
  "Execute hybrid deployment: update flake then deploy"
  (log-info "Starting hybrid deployment for ~a..." machine-name)
  
  ;; First update flake
  (if (update-flake options)
      ;; Then deploy
      (execute-nixos-rebuild machine-name mode options)
      (begin
        (log-error "Flake update failed, aborting deployment")
        #f)))

;; Validate deployment prerequisites
(define (validate-deployment-prerequisites machine-name)
  "Validate that deployment prerequisites are met"
  (log-debug "Validating deployment prerequisites for ~a..." machine-name)
  
  (let ((checks
         `(("machine-config" . ,(lambda () (get-machine-config machine-name)))
           ("ssh-connectivity" . ,(lambda () (test-ssh-connection machine-name)))
           ("flake-exists" . ,(lambda () (file-exists? (string-append (get-homelab-root) "/flake.nix"))))
           ("machine-flake-config" . ,(lambda () (validate-machine-flake-config machine-name))))))
    
    (let ((results (map (lambda (check-pair)
                          (let ((check-name (car check-pair))
                                (check-proc (cdr check-pair)))
                            (let ((result (check-proc)))
                              (if result
                                  (log-debug "✓ Prerequisite: ~a" check-name)
                                  (log-error "✗ Prerequisite failed: ~a" check-name))
                              result)))
                        checks)))
      
      (every identity results))))

;; Validate machine has flake configuration
(define (validate-machine-flake-config machine-name)
  "Check that machine has a configuration in the flake"
  (let ((machine-dir (string-append (get-homelab-root) "/machines/" machine-name)))
    (and (file-exists? machine-dir)
         (file-exists? (string-append machine-dir "/configuration.nix")))))

;; Validate post-deployment state
(define (validate-post-deployment machine-name mode)
  "Validate system state after deployment"
  (log-debug "Validating post-deployment state for ~a..." machine-name)
  
  ;; Wait a moment for services to stabilize
  (sleep 3)
  
  (let ((checks
         `(("ssh-connectivity" . ,(lambda () (test-ssh-connection machine-name)))
           ("system-responsive" . ,(lambda () (check-system-responsive machine-name)))
           ("critical-services" . ,(lambda () (check-critical-services machine-name))))))
    
    (let ((results (map (lambda (check-pair)
                          (let ((check-name (car check-pair))
                                (check-proc (cdr check-pair)))
                            (catch #t
                              (lambda ()
                                (let ((result (check-proc)))
                                  (if result
                                      (log-debug "✓ Post-deployment: ~a" check-name)
                                      (log-warn "✗ Post-deployment: ~a" check-name))
                                  result))
                              (lambda (key . args)
                                (log-warn "Post-deployment check ~a failed: ~a" check-name key)
                                #f))))
                        checks)))
      
      (every identity results))))

;; Check if system is responsive after deployment
(define (check-system-responsive machine-name)
  "Check if system is responsive after deployment"
  (call-with-values (((success output) 
               (run-remote-command machine-name "echo 'system-check' && uptime")))
    (and success (string-contains output "system-check"))))

;; Update all machines
(define (update-all-machines mode options)
  "Update all configured machines"
  (let ((machines (get-all-machines))
        (dry-run (option-ref options 'dry-run #f)))
    
    (log-info "Starting update of all machines (mode: ~a)..." mode)
    
    (if dry-run
        (begin
          (log-info "DRY RUN: Would update machines: ~a" (string-join machines ", "))
          #t)
        
        (let ((results 
               (map (lambda (machine-name)
                      (log-info "Updating ~a..." machine-name)
                      (let ((result (deploy-machine machine-name mode options)))
                        (if result
                            (log-success "✓ ~a updated successfully" machine-name)
                            (log-error "✗ ~a update failed" machine-name))
                        `(,machine-name . ,result)))
                    machines)))
          
          (let ((successful (filter cdr results))
                (failed (filter (lambda (r) (not (cdr r))) results)))
            
            (log-info "Update summary:")
            (log-info "  Successful: ~a/~a" (length successful) (length results))
            
            (when (not (null? failed))
              (log-warn "  Failed: ~a" (map car failed)))
            
            ;; Return success if all succeeded
            (= (length successful) (length results)))))))

;; Hybrid update: flake update + selective deployment
(define (hybrid-update target options)
  "Perform hybrid update: flake update followed by deployment"
  (log-info "Starting hybrid update for target: ~a" target)
  
  ;; First update flake
  (if (update-flake options)
      
      ;; Then deploy based on target
      (match target
        ("all"
         (update-all-machines "boot" options))
        
        (machine-name
         (if (validate-machine-name machine-name)
             (deploy-machine machine-name "boot" options)
             #f)))
      
      (begin
        (log-error "Flake update failed, aborting hybrid update")
        #f)))

;; Get deployment status
(define (deployment-status . machine-name)
  "Get current deployment status for machines"
  (let ((machines (if (null? machine-name) 
                     (get-all-machines) 
                     machine-name)))
    
    (map (lambda (machine)
           (let ((last-deployment (get-last-deployment-info machine))
                 (current-generation (get-current-generation machine)))
             `((machine . ,machine)
               (last-deployment . ,last-deployment)
               (current-generation . ,current-generation)
               (status . ,(get-deployment-health machine)))))
         machines)))

;; Get last deployment information
(define (get-last-deployment-info machine-name)
  "Get information about the last deployment"
  (call-with-values (((success output)
               (run-remote-command machine-name 
                                  "ls -la /nix/var/nix/profiles/system* | tail -1")))
    (if success
        (string-trim-right output)
        "unknown")))

;; Get current system generation
(define (get-current-generation machine-name)
  "Get current NixOS generation information"
  (call-with-values (((success output)
               (run-remote-command machine-name 
                                  "nixos-version")))
    (if success
        (string-trim-right output)
        "unknown")))

;; Get deployment health status
(define (get-deployment-health machine-name)
  "Check if deployment is healthy"
  (if (test-ssh-connection machine-name)
      'healthy
      'unhealthy))

;; Rollback deployment (placeholder for future implementation)
(define (rollback-deployment machine-name . generation)
  "Rollback to previous generation (deploy-rs feature)"
  (log-warn "Rollback functionality not yet implemented")
  (log-info "For manual rollback on ~a:" machine-name)
  (log-info "  1. SSH to machine")
  (log-info "  2. Run: sudo nixos-rebuild switch --rollback")
  #f)
