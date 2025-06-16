;; lab/core/status.scm - Infrastructure status functionality

(define-module (lab core status)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (lab core logging)
  #:use-module (lab core config)
  #:use-module (lab core ssh)
  #:export (get-infrastructure-status
            get-machine-services-status
            get-machine-system-info))

(define (get-infrastructure-status . args)
  "Get status of all machines or specific machine if provided"
  (let ((target-machine (if (null? args) #f (car args)))
        (machines (if (null? args) 
                     (get-all-machines) 
                     (list (car args)))))
    
    (log-info "Checking infrastructure status...")
    
    (map (lambda (machine-name)
           (let ((start-time (current-time)))
             (log-debug "Checking ~a..." machine-name)
             
             (let* ((ssh-config (get-ssh-config machine-name))
                    (is-local (and ssh-config (assoc-ref ssh-config 'is-local)))
                    (connection-status (test-ssh-connection machine-name))
                    (services-status (if connection-status
                                       (get-machine-services-status machine-name)
                                       '()))
                    (system-info (if connection-status
                                   (get-machine-system-info machine-name)
                                   #f))
                    (elapsed (- (current-time) start-time)))
               
               `((machine . ,machine-name)
                 (type . ,(if is-local 'local 'remote))
                 (connection . ,(if connection-status 'online 'offline))
                 (services . ,services-status)
                 (system . ,system-info)
                 (check-time . ,elapsed)))))
         machines)))

(define (get-machine-services-status machine-name)
  "Check status of services on a machine"
  (let ((machine-config (get-machine-config machine-name)))
    (if machine-config
        (let ((services (assoc-ref machine-config 'services)))
          (if services
              (map (lambda (service)
                     (call-with-values 
                         (lambda () (run-remote-command machine-name 
                                                       "systemctl is-active" 
                                                       (symbol->string service)))
                       (lambda (success output)
                         `(,service . ,(if success 
                                          (string-trim-right output)
                                          "unknown")))))
                   services)
              '()))
        '())))

(define (get-machine-system-info machine-name)
  "Get basic system information from a machine"
  (let ((info-commands 
         '(("uptime" "uptime -p")
           ("load" "cat /proc/loadavg | cut -d' ' -f1-3")
           ("memory" "free -h | grep Mem | awk '{print $3\"/\"$2}'")
           ("disk" "df -h / | tail -1 | awk '{print $5}'")
           ("kernel" "uname -r"))))
    
    (fold (lambda (cmd-pair acc)
            (let ((key (car cmd-pair))
                  (command (cadr cmd-pair)))
              (call-with-values 
                  (lambda () (run-remote-command machine-name command))
                (lambda (success output)
                  (if success
                      (assoc-set! acc (string->symbol key) (string-trim-right output))
                      acc)))))
          '()
          info-commands)))
