;; utils/ssh/connection-test.scm - Pure SSH connection testing

(define-module (utils ssh connection-test)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:export (test-ssh-connection-pure
            test-ssh-connection))

;; Pure function: Test SSH connectivity to a machine
;; Input: ssh-config alist
;; Output: #t if connection successful, #f otherwise
(define (test-ssh-connection-pure ssh-config)
  "Pure function to test SSH connection given ssh-config"
  (let ((hostname (assoc-ref ssh-config 'hostname))
        (ssh-alias (assoc-ref ssh-config 'ssh-alias))
        (is-local (assoc-ref ssh-config 'is-local)))
    (if is-local
        #t  ; Local connections always succeed
        (let* ((target (or ssh-alias hostname))
               (test-cmd (format #f "ssh -o ConnectTimeout=5 -o BatchMode=yes ~a echo OK" target))
               (port (open-pipe* OPEN_READ "/bin/sh" "-c" test-cmd))
               (output (get-string-all port))
               (status (close-pipe port)))
          (zero? status)))))

;; Impure wrapper: Test SSH connection with logging and config lookup
(define (test-ssh-connection machine-name)
  "Test SSH connectivity to a machine (with side effects: logging, config lookup)"
  (let ((ssh-config (get-ssh-config machine-name)))
    (if (not ssh-config)
        (begin
          (log-error "No SSH configuration found for ~a" machine-name)
          #f)
        (let ((result (test-ssh-connection-pure ssh-config)))
          (if result
              (log-debug "SSH connection to ~a successful" machine-name)
              (log-warn "SSH connection to ~a failed" machine-name))
          result))))
