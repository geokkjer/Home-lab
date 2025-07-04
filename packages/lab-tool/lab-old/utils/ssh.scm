;; utils/ssh.scm - SSH operations for Home Lab Tool
;; Refactored using functional programming principles

(define-module (utils ssh)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:use-module (io ssh)
  #:use-module (io shell)
  #:export (test-ssh-connection
            run-remote-command
            copy-file-to-remote
            run-command-with-retry
            with-ssh-connection))

;; Test SSH connectivity to a machine
(define (test-ssh-connection machine-name)
  "Test SSH connectivity using functional composition"
  (let ((ssh-config (get-ssh-config machine-name)))
    (cond
      ((not ssh-config)
       (log-error "No SSH configuration found for ~a" machine-name)
       #f)
      ((assoc-ref ssh-config 'is-local)
       (log-debug "Machine ~a is local, skipping SSH test" machine-name)
       #t)
      (else
       (test-remote-ssh-connection machine-name ssh-config)))))

;; Helper: Test remote SSH connection
(define (test-remote-ssh-connection machine-name ssh-config)
  "Test remote SSH connection with error handling"
  (let* ((hostname (assoc-ref ssh-config 'hostname))
         (ssh-alias (assoc-ref ssh-config 'ssh-alias))
         (user (assoc-ref ssh-config 'user))
         (identity-file (assoc-ref ssh-config 'identity-file))
         (target (make-ssh-target user (or ssh-alias hostname)))
         (options (make-ssh-options identity-file 5))
         (test-cmd (build-ssh-command target options "echo OK")))
    
    (log-debug "Testing SSH connection to ~a (~a) as ~a using key ~a" 
               machine-name hostname user identity-file)
    
    (catch #t
      (lambda ()
        (let ((result (execute-with-output test-cmd)))
          (if (car result)
              (begin
                (log-debug "SSH connection to ~a successful" machine-name)
                #t)
              (begin
                (log-warn "SSH connection to ~a failed" machine-name)
                #f))))
      (lambda (key . args)
        (log-error "SSH test failed for ~a: ~a ~a" machine-name key args)
        #f))))

;; Run a command on a remote machine
(define (run-remote-command machine-name command . args)
  "Run command remotely using functional composition"
  (let ((ssh-config (get-ssh-config machine-name))
        (full-command (build-full-command command args)))
    (cond
      ((not ssh-config)
       (values #f "No SSH configuration found"))
      ((assoc-ref ssh-config 'is-local)
       (execute-local-command full-command))
      (else
       (execute-remote-command machine-name ssh-config full-command)))))

;; Helper: Build full command string
(define (build-full-command command args)
  "Build complete command string from command and arguments"
  (if (null? args)
      command
      (format #f "~a ~a" command (string-join args " "))))

;; Helper: Execute command locally
(define (execute-local-command command)
  "Execute command locally and return (success . output)"
  (log-debug "Executing locally: ~a" command)
  (let ((result (execute-with-output command)))
    (values (car result) (cdr result))))

;; Helper: Execute command remotely
(define (execute-remote-command machine-name ssh-config command)
  "Execute command remotely using SSH"
  (let* ((hostname (assoc-ref ssh-config 'hostname))
         (ssh-alias (assoc-ref ssh-config 'ssh-alias))
         (user (assoc-ref ssh-config 'user))
         (identity-file (assoc-ref ssh-config 'identity-file))
         (target (make-ssh-target user (or ssh-alias hostname)))
         (options (make-ssh-options identity-file #f))
         (ssh-cmd (build-ssh-command target options command)))
    
    (log-debug "Executing remotely on ~a: ~a" machine-name command)
    
    (catch #t
      (lambda ()
        (let ((result (execute-with-output ssh-cmd)))
          (values (car result) (cdr result))))
      (lambda (key . args)
        (log-error "SSH command failed for ~a: ~a ~a" machine-name key args)
        (values #f "")))))

;; Copy file to remote machine using scp
(define (copy-file-to-remote machine-name local-path remote-path)
  "Copy file to remote machine using functional composition"
  (let ((ssh-config (get-ssh-config machine-name)))
    (cond
      ((not ssh-config)
       (log-error "No SSH configuration found for ~a" machine-name)
       #f)
      ((assoc-ref ssh-config 'is-local)
       (copy-file-locally local-path remote-path))
      (else
       (copy-file-remotely machine-name ssh-config local-path remote-path)))))

;; Helper: Copy file locally
(define (copy-file-locally local-path remote-path)
  "Copy file locally using cp command"
  (log-debug "Copying locally: ~a -> ~a" local-path remote-path)
  (let ((copy-cmd (format #f "cp '~a' '~a'" local-path remote-path)))
    (execute-command copy-cmd)))

;; Helper: Copy file remotely
(define (copy-file-remotely machine-name ssh-config local-path remote-path)
  "Copy file remotely using scp command"
  (let* ((hostname (assoc-ref ssh-config 'hostname))
         (ssh-alias (assoc-ref ssh-config 'ssh-alias))
         (user (assoc-ref ssh-config 'user))
         (identity-file (assoc-ref ssh-config 'identity-file))
         (target (make-ssh-target user (or ssh-alias hostname)))
         (options (make-ssh-options identity-file #f))
         (scp-cmd (format #f "scp ~a '~a' '~a:~a'" options local-path target remote-path)))
    
    (log-debug "Copying to ~a: ~a -> ~a as ~a using key ~a" 
               machine-name local-path remote-path user identity-file)
    
    (let ((success (execute-command scp-cmd)))
      (if success
          (begin
            (log-debug "File copy successful")
            #t)
          (begin
            (log-error "File copy failed")
            #f)))))

;; Run command with retry logic
(define (run-command-with-retry machine-name command max-retries . args)
  "Run command with retry logic using functional recursion"
  (retry-command machine-name command max-retries 0 args))

;; Helper: Retry command implementation
(define (retry-command machine-name command max-retries current-retry args)
  "Recursive retry implementation"
  (call-with-values
    (lambda () (apply run-remote-command machine-name command args))
    (lambda (success output)
      (cond
        (success
         (values #t output))
        ((< current-retry max-retries)
         (log-warn "Command failed, retrying (~a/~a)..." (+ current-retry 1) max-retries)
         (sleep 2)
         (retry-command machine-name command max-retries (+ current-retry 1) args))
        (else
         (values #f output))))))

;; Execute a thunk with SSH connection context
(define (with-ssh-connection machine-name thunk)
  "Execute thunk with SSH connection context using functional composition"
  (cond
    ((test-ssh-connection machine-name)
     (execute-with-ssh-context thunk machine-name))
    (else
     (log-error "Cannot establish SSH connection to ~a" machine-name)
     #f)))

;; Helper: Execute thunk with error handling
(define (execute-with-ssh-context thunk machine-name)
  "Execute thunk with proper error handling"
  (catch #t
    (lambda () (thunk))
    (lambda (key . args)
      (log-error "SSH operation failed: ~a ~a" key args)
      #f)))