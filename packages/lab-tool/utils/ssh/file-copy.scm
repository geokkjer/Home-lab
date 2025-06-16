;; utils/ssh/file-copy.scm - Pure file copying operations

(define-module (utils ssh file-copy)
  #:use-module (ice-9 format)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:export (copy-file-pure
            build-copy-context
            copy-file-to-remote))

;; Pure function: Copy file with given copy context
;; Input: copy-context alist, local-path string, remote-path string
;; Output: #t if successful, #f otherwise
(define (copy-file-pure copy-context local-path remote-path)
  "Pure function to copy file given copy context"
  (let ((is-local (assoc-ref copy-context 'is-local))
        (target (assoc-ref copy-context 'target)))
    (let* ((copy-cmd (if is-local
                         (format #f "cp '~a' '~a'" local-path remote-path)
                         (format #f "scp '~a' '~a:~a'" local-path target remote-path)))
           (status (system copy-cmd)))
      (zero? status))))

;; Pure function: Build copy context from ssh-config
(define (build-copy-context ssh-config)
  "Pure function to build copy context from ssh-config"
  (let ((hostname (assoc-ref ssh-config 'hostname))
        (ssh-alias (assoc-ref ssh-config 'ssh-alias))
        (is-local (assoc-ref ssh-config 'is-local)))
    `((is-local . ,is-local)
      (target . ,(or ssh-alias hostname)))))

;; Impure wrapper: Copy file to remote with logging and config lookup
(define (copy-file-to-remote machine-name local-path remote-path)
  "Copy file to remote machine (with side effects: logging, config lookup)"
  (let ((ssh-config (get-ssh-config machine-name)))
    (if (not ssh-config)
        (begin
          (log-error "No SSH configuration found for ~a" machine-name)
          #f)
        (let* ((copy-context (build-copy-context ssh-config))
               (is-local (assoc-ref copy-context 'is-local)))
          (log-debug "Copying ~a: ~a -> ~a" 
                     (if is-local "locally" (format #f "to ~a" machine-name))
                     local-path remote-path)
          (let ((result (copy-file-pure copy-context local-path remote-path)))
            (if result
                (log-debug "File copy successful")
                (log-error "File copy failed"))
            result)))))
