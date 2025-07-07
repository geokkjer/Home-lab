;; lab/core.scm - Core infrastructure operations (impure)

(define-module (lab core)
  #:use-module (utils config)
  #:use-module (utils ssh)
  #:use-module (utils logging)
  #:export (get-infrastructure-status))

;; Impure function: Get infrastructure status with side effects
(define (get-infrastructure-status)
  "Get status of all machines (impure - has logging side effects)"
  (log-info "Checking infrastructure status...")
  (let ((machines (get-all-machines)))
    (map (lambda (machine)
           (let ((status (test-ssh-connection machine)))
             `((machine . ,machine)
               (status . ,(if status 'online 'offline)))))
         machines)))
