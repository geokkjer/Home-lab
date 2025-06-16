;; lab/core/ssh.scm - SSH operations

(define-module (lab core ssh)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:export (test-ssh-connection
            run-remote-command))

(define (test-ssh-connection machine-name)
  "Test SSH connection to machine"
  (zero? (system (format #f "ssh -o ConnectTimeout=5 -o BatchMode=yes ~a exit 2>/dev/null" machine-name))))

(define (run-remote-command machine-name command . args)
  "Run command on remote machine via SSH"
  (let* ((full-command (if (null? args)
                          command
                          (string-join (cons command args) " ")))
         (ssh-command (format #f "ssh ~a '~a'" machine-name full-command))
         (port (open-input-pipe ssh-command))
         (output (read-string port))
         (status (close-pipe port)))
    (values (zero? status) output)))
