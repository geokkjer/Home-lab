;; io/ssh.scm - Pure SSH command building functions

(define-module (io ssh)
  #:use-module (ice-9 format)
  #:export (make-ssh-target
            build-ssh-command
            build-rsync-command
            make-ssh-options))

;; Pure function: Build SSH target string
(define (make-ssh-target user hostname)
  "Build SSH target string from user and hostname"
  (if user
      (format #f "~a@~a" user hostname)
      hostname))

;; Pure function: Build SSH options string
(define (make-ssh-options identity-file timeout)
  "Build SSH options string"
  (let ((opts '()))
    (when identity-file
      (set! opts (cons (format #f "-i ~a" identity-file) opts)))
    (when timeout
      (set! opts (cons (format #f "-o ConnectTimeout=~a" timeout) opts)))
    (set! opts (cons "-o BatchMode=yes" opts))
    (string-join (reverse opts) " ")))

;; Pure function: Build SSH command
(define (build-ssh-command target options command)
  "Build complete SSH command string"
  (format #f "ssh ~a ~a '~a'" options target command))

;; Pure function: Build rsync command
(define (build-rsync-command source-path target dest-path ssh-options)
  "Build rsync command with SSH transport"
  (format #f "rsync -avz --delete -e 'ssh ~a' ~a/ ~a:~a/"
          ssh-options source-path target dest-path))