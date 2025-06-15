;; utils/ssh.scm - SSH operations for Home Lab Tool

(define-module (utils ssh)
  #:use-module (ssh session)
  #:use-module (ssh channel)
  #:use-module (ssh popen)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 call-with-values)
  #:use-module (srfi srfi-1)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:export (test-ssh-connection
            run-remote-command
            copy-file-to-remote
            run-command-with-retry
            with-ssh-connection))

;; Test SSH connectivity to a machine
(define (test-ssh-connection machine-name)
  (let ((ssh-config (get-ssh-config machine-name)))
    (if (not ssh-config)
        (begin
          (log-error "No SSH configuration found for ~a" machine-name)
          #f)
        (if (assoc-ref ssh-config 'is-local)
            (begin
              (log-debug "Machine ~a is local, skipping SSH test" machine-name)
              #t)
            (let ((hostname (assoc-ref ssh-config 'hostname))
                  (ssh-alias (assoc-ref ssh-config 'ssh-alias)))
              (log-debug "Testing SSH connection to ~a (~a)" machine-name hostname)
              (catch #t
                (lambda ()
                  ;; Use system ssh command for compatibility with existing configuration
                  (let* ((test-cmd (if ssh-alias
                                      (format #f "ssh -o ConnectTimeout=5 -o BatchMode=yes ~a echo OK" ssh-alias)
                                      (format #f "ssh -o ConnectTimeout=5 -o BatchMode=yes ~a echo OK" hostname)))
                         (port (open-pipe* OPEN_READ "/bin/sh" "-c" test-cmd))
                         (output (get-string-all port))
                         (status (close-pipe port)))
                    (if (zero? status)
                        (begin
                          (log-debug "SSH connection to ~a successful" machine-name)
                          #t)
                        (begin
                          (log-warn "SSH connection to ~a failed (exit: ~a)" machine-name status)
                          #f))))
                (lambda (key . args)
                  (log-error "SSH test failed for ~a: ~a ~a" machine-name key args)
                  #f)))))))

;; Run a command on a remote machine
(define (run-remote-command machine-name command . args)
  (let ((ssh-config (get-ssh-config machine-name))
        (full-command (if (null? args) 
                          command 
                          (format #f "~a ~a" command (string-join args " ")))))
    (if (not ssh-config)
        (values #f "No SSH configuration found")
        (if (assoc-ref ssh-config 'is-local)
            ;; Local execution
            (begin
              (log-debug "Executing locally: ~a" full-command)
              (let* ((port (open-pipe* OPEN_READ "/bin/sh" "-c" full-command))
                     (output (get-string-all port))
                     (status (close-pipe port)))
                (values (zero? status) output)))
            ;; Remote execution
            (let ((ssh-alias (assoc-ref ssh-config 'ssh-alias))
                  (hostname (assoc-ref ssh-config 'hostname)))
              (log-debug "Executing on ~a: ~a" machine-name full-command)
              (let* ((ssh-cmd (format #f "ssh ~a '~a'" 
                                     (or ssh-alias hostname) 
                                     full-command))
                     (port (open-pipe* OPEN_READ "/bin/sh" "-c" ssh-cmd))
                     (output (get-string-all port))
                     (status (close-pipe port)))
                (values (zero? status) output)))))))

;; Copy file to remote machine using scp
(define (copy-file-to-remote machine-name local-path remote-path)
  (let ((ssh-config (get-ssh-config machine-name)))
    (if (not ssh-config)
        (begin
          (log-error "No SSH configuration found for ~a" machine-name)
          #f)
        (if (assoc-ref ssh-config 'is-local)
            ;; Local copy
            (begin
              (log-debug "Copying locally: ~a -> ~a" local-path remote-path)
              (let* ((copy-cmd (format #f "cp '~a' '~a'" local-path remote-path))
                     (status (system copy-cmd)))
                (zero? status)))
            ;; Remote copy
            (let ((ssh-alias (assoc-ref ssh-config 'ssh-alias))
                  (hostname (assoc-ref ssh-config 'hostname)))
              (log-debug "Copying to ~a: ~a -> ~a" machine-name local-path remote-path)
              (let* ((scp-cmd (format #f "scp '~a' '~a:~a'" 
                                     local-path 
                                     (or ssh-alias hostname) 
                                     remote-path))
                     (status (system scp-cmd)))
                (if (zero? status)
                    (begin
                      (log-debug "File copy successful")
                      #t)
                    (begin
                      (log-error "File copy failed (exit: ~a)" status)
                      #f))))))))

;; Run command with retry logic
(define (run-command-with-retry machine-name command max-retries . args)
  (let loop ((retries 0))
    (call-with-values (success output) (apply run-remote-command machine-name command args)
      (if success
          (values #t output)
          (if (< retries max-retries)
              (begin
                (log-warn "Command failed, retrying (~a/~a)..." (+ retries 1) max-retries)
                (sleep 2)
                (loop (+ retries 1)))
              (values #f output))))))

;; Execute a thunk with SSH connection context
(define (with-ssh-connection machine-name thunk)
  (if (test-ssh-connection machine-name)
      (catch #t
        (lambda () (thunk))
        (lambda (key . args)
          (log-error "SSH operation failed: ~a ~a" key args)
          #f))
      (begin
        (log-error "Cannot establish SSH connection to ~a" machine-name)
        #f)))
