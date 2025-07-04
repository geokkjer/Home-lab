;; core/commands.scm - Pure command building logic

(define-module (core commands)
  #:use-module (ice-9 format)
  #:use-module (core config)
  #:use-module (deploy ssh-strategy)
  #:use-module (deploy executor)
  #:export (build-flake-update-command
            deploy-to-machine
            list-available-machines))

;; Pure function to build flake update command
(define (build-flake-update-command . flake-path-override)
  "Build a command to update flake inputs"
  (let ((flake-path (if (null? flake-path-override) 
                       (get-flake-path) 
                       (car flake-path-override))))
    (format #f "nix flake update ~a" flake-path)))

;; Command to deploy to a specific machine
(define (deploy-to-machine machine-name . options)
  "Deploy to a specific machine using centralized configuration"
  (let* ((ssh-config (get-ssh-config machine-name))
         (deploy-options (if (null? options) '() (car options)))
         (deploy-plan (build-ssh-deploy-commands machine-name deploy-options)))
    
    (if ssh-config
        (begin
          (display (format #f "Deploying to machine: ~a\n" machine-name))
          (display (format #f "SSH Config: ~a\n" ssh-config))
          
          ;; Execute the deployment
          (execute-deploy-commands deploy-plan))
        (begin
          (display (format #f "Error: Unknown machine '~a'\n" machine-name))
          (display "Available machines:\n")
          (list-available-machines)
          #f))))

;; Command to list available machines
(define (list-available-machines)
  "List all available machines for deployment"
  (let ((machines (get-all-hosts)))
    (display "Available machines:\n")
    (for-each (lambda (machine)
                (let ((ssh-config (get-ssh-config machine)))
                  (if ssh-config
                      (display (format #f "  ~a - ~a@~a\n" 
                                     machine 
                                     (assoc-ref ssh-config 'user)
                                     (assoc-ref ssh-config 'hostname))))))
              machines)
    machines))
