;; lab/machines.scm - Machine management (impure)

(define-module (lab machines)
  #:use-module (utils config)
  #:use-module (utils logging)
  #:export (list-machines
            get-machine-info))

;; Impure function: List all machines with logging
(define (list-machines)
  "List all configured machines (impure - has logging side effects)"
  (log-debug "Listing machines...")
  (get-all-machines))

;; Impure function: Get machine information
(define (get-machine-info machine-name)
  "Get detailed machine information (impure - has logging side effects)"
  (log-debug "Getting info for machine: ~a" machine-name)
  (get-machine-config machine-name))
