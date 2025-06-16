;; lab/machines.scm - Machine management (impure)

(define-module (lab machines)
  #:use-module (utils config)
  #:use-module (utils logging)
  #:use-module (utils ssh)
  #:export (list-machines
            get-machine-info
            check-machine-health
            discover-machines))

;; Impure function: List all machines with logging
(define (list-machines)
  "List all configured machines (impure - has logging side effects)"
  (log-debug "Listing machines...")
  (get-all-machines))

;; Impure function: Get machine information
(define (get-machine-info machine-name)
  "Get detailed machine information (impure - has logging side effects)"
  (log-debug "Getting info for machine: ~a" machine-name)
  (let ((config (get-machine-config machine-name))
        (ssh-config (get-ssh-config machine-name)))
    (if config
        `((name . ,machine-name)
          (config . ,config)
          (ssh . ,ssh-config))
        #f)))

;; Impure function: Check machine health
(define (check-machine-health machine-name)
  "Check machine health status (impure - has side effects)"
  (log-debug "Checking health for ~a..." machine-name)
  (let* ((ssh-status (test-ssh-connection machine-name))
         (config (get-machine-config machine-name))
         (services (if config (assoc-ref config 'services) '())))
    
    `((machine . ,machine-name)
      (ssh-connectivity . ,ssh-status)
      (services-configured . ,(length services))
      (status . ,(if ssh-status 'healthy 'unhealthy)))))

;; Impure function: Discover machines on network
(define (discover-machines)
  "Discover machines on the network (impure - has side effects)"
  (log-info "Discovering machines...")
  (let ((machines (list-machines)))
    (map (lambda (machine)
           (let ((health (check-machine-health machine)))
             (log-debug "Machine ~a: ~a" machine (assoc-ref health 'status))
             health))
         machines)))
