;; deploy/executor.scm - Impure execution layer

(define-module (deploy executor)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (core config)
  #:export (execute-command
            execute-ssh-command
            execute-deploy-commands
            run-with-timeout))

;; Execute a single command locally
(define (execute-command cmd)
  "Execute a command locally and return (exit-code . output)"
  (let* ((port (open-input-pipe cmd))
         (output (get-string-all port))
         (exit-code (close-pipe port)))
    (cons exit-code output)))

;; Execute a command over SSH
(define (execute-ssh-command ssh-config cmd)
  "Execute a command over SSH using centralized SSH key configuration"
  (let* ((hostname (assoc-ref ssh-config 'hostname))
         (user (assoc-ref ssh-config 'user))
         (ssh-key (get-ssh-key))
         (ssh-cmd (format #f "ssh -i ~a -o BatchMode=yes ~a@~a '~a'" 
                         ssh-key user hostname cmd)))
    (execute-command ssh-cmd)))

;; Execute deployment commands in sequence
(define (execute-deploy-commands deploy-plan)
  "Execute deployment commands from a deployment plan"
  (let ((rsync-cmd (assoc-ref deploy-plan 'rsync))
        (rebuild-cmd (assoc-ref deploy-plan 'rebuild))
        (ssh-config (assoc-ref deploy-plan 'ssh-config)))
    
    (display "Starting deployment...\n")
    
    ;; Step 1: Rsync flake to remote
    (display "Step 1: Syncing flake to remote host...\n")
    (display (format #f "Running: ~a\n" rsync-cmd))
    (let ((rsync-result (execute-command rsync-cmd)))
      (if (= (car rsync-result) 0)
          (begin
            (display "Rsync completed successfully\n")
            
            ;; Step 2: Run nixos-rebuild on remote
            (display "Step 2: Running nixos-rebuild on remote host...\n")
            (display (format #f "Running: ~a\n" rebuild-cmd))
            (let ((rebuild-result (execute-ssh-command ssh-config rebuild-cmd)))
              (if (= (car rebuild-result) 0)
                  (begin
                    (display "Deployment completed successfully!\n")
                    (display (cdr rebuild-result))
                    #t)
                  (begin
                    (display "nixos-rebuild failed:\n")
                    (display (cdr rebuild-result))
                    #f))))
          (begin
            (display "Rsync failed:\n")
            (display (cdr rsync-result))
            #f)))))

;; Run command with timeout (placeholder for future implementation)
(define (run-with-timeout cmd timeout)
  "Run command with timeout - simplified version"
  (execute-command cmd))