;; lab/monitoring.scm - Infrastructure monitoring and health checks

(define-module (lab monitoring)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:use-module (utils ssh)
  #:use-module (lab core)
  #:use-module (lab machines)
  #:export (monitor-infrastructure
            start-monitoring
            stop-monitoring
            get-monitoring-status
            collect-metrics
            generate-monitoring-report))

;; Monitor infrastructure with optional service filtering
(define (monitor-infrastructure service options)
  "Monitor infrastructure, optionally filtering by service"
  (let ((verbose (option-ref options 'verbose #f))
        (machines (get-all-machines)))
    
    (log-info "Starting infrastructure monitoring...")
    
    (if service
        (monitor-specific-service service machines verbose)
        (monitor-all-services machines verbose))))

;; Monitor a specific service across all machines
(define (monitor-specific-service service machines verbose)
  "Monitor a specific service across all configured machines"
  (log-info "Monitoring service: ~a" service)
  
  (let ((service-symbol (string->symbol service)))
    (for-each 
      (lambda (machine-name)
        (let ((machine-config (get-machine-config machine-name)))
          (when machine-config
            (let ((machine-services (assoc-ref machine-config 'services)))
              (when (and machine-services (member service-symbol machine-services))
                (monitor-service-on-machine machine-name service verbose))))))
      machines)))

;; Monitor all services across all machines
(define (monitor-all-services machines verbose)
  "Monitor all services across all machines"
  (log-info "Monitoring all services across ~a machines" (length machines))
  
  (let ((monitoring-results
         (map (lambda (machine-name)
                (log-debug "Monitoring ~a..." machine-name)
                (monitor-machine-services machine-name verbose))
              machines)))
    
    (display-monitoring-summary monitoring-results)))

;; Monitor services on a specific machine
(define (monitor-machine-services machine-name verbose)
  "Monitor all services on a specific machine"
  (let ((machine-config (get-machine-config machine-name))
        (connection-status (test-ssh-connection machine-name)))
    
    (if (not connection-status)
        (begin
          (log-warn "Cannot connect to ~a, skipping monitoring" machine-name)
          `((machine . ,machine-name)
            (status . offline)
            (services . ())))
        
        (let ((services (if machine-config 
                           (assoc-ref machine-config 'services) 
                           '())))
          (if (null? services)
              (begin
                (log-debug "No services configured for ~a" machine-name)
                `((machine . ,machine-name)
                  (status . online)
                  (services . ())))
              
              (let ((service-statuses
                     (map (lambda (service)
                            (monitor-service-on-machine machine-name 
                                                       (symbol->string service) 
                                                       verbose))
                          services)))
                `((machine . ,machine-name)
                  (status . online)
                  (services . ,service-statuses))))))))

;; Monitor a specific service on a specific machine
(define (monitor-service-on-machine machine-name service verbose)
  "Monitor a specific service on a specific machine"
  (log-debug "Checking ~a service on ~a..." service machine-name)
  
  (let ((service-checks
         `(("status" . ,(lambda () (check-service-status machine-name service)))
           ("health" . ,(lambda () (check-service-health machine-name service)))
           ("logs" . ,(lambda () (check-service-logs machine-name service))))))
    
    (let ((results
           (map (lambda (check-pair)
                  (let ((check-name (car check-pair))
                        (check-proc (cdr check-pair)))
                    (catch #t
                      (lambda ()
                        `(,check-name . ,(check-proc)))
                      (lambda (key . args)
                        (log-warn "Service check ~a failed for ~a: ~a" 
                                 check-name service key)
                        `(,check-name . (error . ,key))))))
                service-checks)))
      
      (when verbose
        (display-service-details machine-name service results))
      
      `((service . ,service)
        (machine . ,machine-name)
        (checks . ,results)
        (timestamp . ,(current-date))))))

;; Check service status using systemctl
(define (check-service-status machine-name service)
  "Check if a service is active using systemctl"
  (call-with-values (((success output) 
               (run-remote-command machine-name "systemctl is-active" service)))
    (if success
        (let ((status (string-trim-right output)))
          `((active . ,(string=? status "active"))
            (status . ,status)))
        `((active . #f)
          (status . "unknown")
          (error . "command-failed")))))

;; Check service health with additional metrics
(define (check-service-health machine-name service)
  "Perform health checks for a service"
  (let ((health-commands
         (get-service-health-commands service)))
    
    (if (null? health-commands)
        `((healthy . unknown)
          (reason . "no-health-checks-defined"))
        
        (let ((health-results
               (map (lambda (cmd-pair)
                      (let ((check-name (car cmd-pair))
                            (command (cdr cmd-pair)))
                        (call-with-values (((success output) 
                                     (run-remote-command machine-name command)))
                          `(,check-name . ((success . ,success)
                                          (output . ,(if success 
                                                        (string-trim-right output)
                                                        output)))))))
                    health-commands)))
          
          (let ((all-healthy (every (lambda (result) 
                                     (assoc-ref (cdr result) 'success)) 
                                   health-results)))
            `((healthy . ,all-healthy)
              (checks . ,health-results)))))))

;; Get service-specific health check commands
(define (get-service-health-commands service)
  "Get health check commands for specific services"
  (match service
    ("ollama"
     '(("api-check" . "curl -f http://localhost:11434/api/tags > /dev/null 2>&1; echo $?")
       ("process-check" . "pgrep ollama > /dev/null; echo $?")))
    
    ("forgejo"
     '(("web-check" . "curl -f http://localhost:3000 > /dev/null 2>&1; echo $?")
       ("process-check" . "pgrep forgejo > /dev/null; echo $?")))
    
    ("jellyfin"
     '(("web-check" . "curl -f http://localhost:8096/health > /dev/null 2>&1; echo $?")
       ("process-check" . "pgrep jellyfin > /dev/null; echo $?")))
    
    ("nfs-server"
     '(("service-check" . "showmount -e localhost > /dev/null 2>&1; echo $?")
       ("exports-check" . "test -f /etc/exports; echo $?")))
    
    ("nginx"
     '(("config-check" . "nginx -t 2>/dev/null; echo $?")
       ("web-check" . "curl -f http://localhost > /dev/null 2>&1; echo $?")))
    
    ("sshd"
     '(("port-check" . "ss -tuln | grep ':22 ' > /dev/null; echo $?")))
    
    (_ '())))

;; Check service logs for errors
(define (check-service-logs machine-name service)
  "Check recent service logs for errors"
  (call-with-values (((success output)
               (run-remote-command machine-name 
                                  (format #f "journalctl -u ~a --since='10 minutes ago' --priority=err --no-pager | wc -l" service))))
    (if success
        (let ((error-count (string->number (string-trim-right output))))
          `((recent-errors . ,error-count)
            (status . ,(if (< error-count 5) 'good 'concerning))))
        `((recent-errors . unknown)
          (status . error)
          (reason . "log-check-failed")))))

;; Display service monitoring details
(define (display-service-details machine-name service results)
  "Display detailed service monitoring information"
  (format #t "  🔧 ~a@~a:~%" service machine-name)
  
  (for-each 
    (lambda (check-result)
      (let ((check-name (car check-result))
            (check-data (cdr check-result)))
        (match check-name
          ("status"
           (let ((active (assoc-ref check-data 'active))
                 (status (assoc-ref check-data 'status)))
             (format #t "    Status: ~a ~a~%" 
                     (if active "✅" "❌") 
                     status)))
          
          ("health"
           (let ((healthy (assoc-ref check-data 'healthy)))
             (format #t "    Health: ~a ~a~%" 
                     (cond ((eq? healthy #t) "✅")
                           ((eq? healthy #f) "❌") 
                           (else "❓"))
                     healthy)))
          
          ("logs"
           (let ((errors (assoc-ref check-data 'recent-errors))
                 (status (assoc-ref check-data 'status)))
             (format #t "    Logs: ~a (~a recent errors)~%" 
                     (cond ((eq? status 'good) "✅")
                           ((eq? status 'concerning) "⚠️")
                           (else "❓"))
                     errors)))
          
          (_ (format #t "    ~a: ~a~%" check-name check-data)))))
    results))

;; Display monitoring summary
(define (display-monitoring-summary results)
  "Display a summary of monitoring results"
  (newline)
  (log-info "Infrastructure Monitoring Summary:")
  (newline)
  
  (for-each 
    (lambda (machine-result)
      (let ((machine-name (assoc-ref machine-result 'machine))
            (machine-status (assoc-ref machine-result 'status))
            (services (assoc-ref machine-result 'services)))
        
        (format #t "━━━ ~a (~a) ━━━~%" machine-name machine-status)
        
        (if (eq? machine-status 'offline)
            (format #t "  ❌ Machine offline~%")
            (if (null? services)
                (format #t "  ℹ️  No services configured~%")
                (for-each 
                  (lambda (service-result)
                    (let ((service-name (assoc-ref service-result 'service))
                          (checks (assoc-ref service-result 'checks)))
                      (let ((status-check (assoc-ref checks "status"))
                            (health-check (assoc-ref checks "health")))
                        (let ((is-active (and status-check 
                                             (assoc-ref status-check 'active)))
                              (is-healthy (and health-check 
                                              (eq? (assoc-ref health-check 'healthy) #t))))
                          (format #t "  ~a ~a~%" 
                                  service-name
                                  (cond ((and is-active is-healthy) "✅")
                                        (is-active "⚠️")
                                        (else "❌")))))))
                  services)))
        (newline)))
    results))

;; Start continuous monitoring (placeholder)
(define (start-monitoring options)
  "Start continuous monitoring daemon"
  (log-warn "Continuous monitoring not yet implemented")
  (log-info "For now, use: lab monitor [service]")
  #f)

;; Stop continuous monitoring (placeholder)
(define (stop-monitoring options)
  "Stop continuous monitoring daemon"
  (log-warn "Continuous monitoring not yet implemented")
  #f)

;; Get monitoring status (placeholder)
(define (get-monitoring-status options)
  "Get status of monitoring daemon"
  (log-info "Monitoring Status: Manual mode")
  (log-info "Use 'lab monitor' for on-demand monitoring")
  #t)

;; Collect metrics for analysis
(define (collect-metrics machine-name . time-range)
  "Collect performance and health metrics"
  (let ((range (if (null? time-range) "1h" (car time-range))))
    (log-debug "Collecting metrics for ~a (range: ~a)" machine-name range)
    
    (let ((metrics (get-machine-metrics machine-name range)))
      (log-success "Metrics collected for ~a" machine-name)
      metrics)))

;; Generate monitoring report
(define (generate-monitoring-report . machines)
  "Generate a comprehensive monitoring report"
  (let ((target-machines (if (null? machines) 
                            (get-all-machines) 
                            machines)))
    
    (log-info "Generating monitoring report for ~a machines..." 
             (length target-machines))
    
    (let ((report-data
           (map (lambda (machine)
                  (let ((monitoring-result (monitor-machine-services machine #t))
                        (metrics (collect-metrics machine)))
                    `((machine . ,machine)
                      (monitoring . ,monitoring-result)
                      (metrics . ,metrics)
                      (timestamp . ,(current-date)))))
                target-machines)))
      
      (log-success "Monitoring report generated")
      report-data)))
