;; io/shell.scm - Impure shell execution functions

(define-module (io shell)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (utils logging)
  #:export (execute-command
            execute-with-output
            test-command))

;; Impure function: Execute command and return success/failure
(define (execute-command command)
  "Execute shell command, return true if successful"
  (log-debug "Executing: ~a" command)
  (let ((status (system command)))
    (zero? status)))

;; Impure function: Execute command and capture output
(define (execute-with-output command)
  "Execute command and return (success . output) pair"
  (log-debug "Executing with output: ~a" command)
  (let* ((port (open-pipe* OPEN_READ "/bin/sh" "-c" command))
         (output (get-string-all port))
         (status (close-pipe port))
         (success (zero? status)))
    (log-debug "Command ~a: exit=~a" (if success "succeeded" "failed") status)
    (cons success output)))

;; Impure function: Test if command succeeds (no output)
(define (test-command command)
  "Test if command succeeds, return boolean"
  (log-debug "Testing command: ~a" command)
  (let* ((port (open-pipe* OPEN_READ "/bin/sh" "-c" command))
         (output (get-string-all port))
         (status (close-pipe port)))
    (zero? status)))