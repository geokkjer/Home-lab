;; utils/ssh/remote-command.scm - Pure remote command execution

(define-module (utils ssh remote-command)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:export (run-remote-command-pure
            execute-command-pure
            build-execution-context
            run-remote-command))

;; Pure function: Execute command with given execution context
;; Input: execution-context alist, command string, args list
;; Output: (values success? output-string)
(define (execute-command-pure execution-context command args)
  "Pure function to execute command in given context"
  (let ((is-local (assoc-ref execution-context 'is-local))
        (target (assoc-ref execution-context 'target))
        (full-command (if (null? args) 
                          command 
                          (format #f "~a ~a" command (string-join args " ")))))
    (let* ((exec-cmd (if is-local
                         full-command
                         (format #f "ssh ~a '~a'" target full-command)))
           (port (open-pipe* OPEN_READ "/bin/sh" "-c" exec-cmd))
           (output (get-string-all port))
           (status (close-pipe port)))
      (values (zero? status) output))))

;; Pure function: Build execution context from ssh-config
(define (build-execution-context ssh-config)
  "Pure function to build execution context from ssh-config"
  (let ((hostname (assoc-ref ssh-config 'hostname))
        (ssh-alias (assoc-ref ssh-config 'ssh-alias))
        (is-local (assoc-ref ssh-config 'is-local)))
    `((is-local . ,is-local)
      (target . ,(or ssh-alias hostname)))))

;; Pure wrapper: Run remote command with pure functions
(define (run-remote-command-pure ssh-config command args)
  "Pure function to run remote command given ssh-config"
  (let ((exec-context (build-execution-context ssh-config)))
    (execute-command-pure exec-context command args)))

;; Impure wrapper: Run remote command with logging and config lookup
(define (run-remote-command machine-name command . args)
  "Run command on remote machine (with side effects: logging, config lookup)"
  (let ((ssh-config (get-ssh-config machine-name)))
    (if (not ssh-config)
        (begin
          (log-error "No SSH configuration found for ~a" machine-name)
          (values #f "No SSH configuration found"))
        (begin
          (log-debug "Executing on ~a: ~a ~a" machine-name command (string-join args " "))
          (run-remote-command-pure ssh-config command args)))))
