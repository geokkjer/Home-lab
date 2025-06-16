;; lab/core/health.scm - Health check functionality

(define-module (lab core health)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (lab core logging)
  #:use-module (lab core ssh)
  #:export (check-system-health
            check-disk-space
            check-system-load
            check-critical-services
            check-network-connectivity))

(define (check-system-health machine-name)
  "Perform comprehensive health check on a machine"
  (log-info "Performing health check on ~a..." machine-name)
  
  (let ((health-checks 
         '(("connectivity" . test-ssh-connection)
           ("disk-space" . check-disk-space)
           ("system-load" . check-system-load)
           ("critical-services" . check-critical-services)
           ("network" . check-network-connectivity))))
    
    (map (lambda (check-pair)
           (let ((check-name (car check-pair))
                 (check-proc (cdr check-pair)))
             (log-debug "Running ~a check..." check-name)
             (catch #t
               (lambda ()
                 (let ((result (check-proc machine-name)))
                   `(,check-name . ((status . ,(if result 'pass 'fail))
                                  (result . ,result)))))
               (lambda (key . args)
                 (log-warn "Health check ~a failed: ~a" check-name key)
                 `(,check-name . ((status . error)
                                  (error . ,key)))))))
         health-checks)))

(define (check-disk-space machine-name)
  "Check if disk space is below critical threshold"
  (call-with-values 
      (lambda () (run-remote-command machine-name "df / | tail -1 | awk '{print $5}' | sed 's/%//'"))
    (lambda (success output)
      (if success
          (let ((usage (string->number (string-trim-right output))))
            (< usage 90)) ; Pass if usage < 90%
          #f))))

(define (check-system-load machine-name)
  "Check if system load is reasonable"
  (call-with-values 
      (lambda () (run-remote-command machine-name "cat /proc/loadavg | cut -d' ' -f1"))
    (lambda (success output)
      (if success
          (let ((load (string->number (string-trim-right output))))
            (< load 5.0)) ; Pass if load < 5.0
          #f))))

(define (check-critical-services machine-name)
  "Check that critical services are running"
  (let ((critical-services '("sshd")))
    (every (lambda (service)
             (call-with-values 
                 (lambda () (run-remote-command machine-name "systemctl is-active" service))
               (lambda (success output)
                 (and success (string=? (string-trim-right output) "active")))))
           critical-services)))

(define (check-network-connectivity machine-name)
  "Check basic network connectivity"
  (call-with-values 
      (lambda () (run-remote-command machine-name "ping -c 1 -W 5 8.8.8.8 > /dev/null 2>&1; echo $?"))
    (lambda (success output)
      (and success (string=? (string-trim-right output) "0")))))
