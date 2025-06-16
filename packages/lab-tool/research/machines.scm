;; lab/machines.scm - Machine-specific operations

(define-module (lab machines)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (utils logging)
  #:use-module (utils config)
  #:use-module (utils ssh)
  #:use-module (lab core)
  #:export (show-infrastructure-status
            get-machine-details
            discover-machines
            validate-machine-health
            get-machine-metrics
            option-ref))

;; Helper function for option handling
(define (option-ref options key default)
  "Get option value with default fallback"
  (let ((value (assoc-ref options key)))
    (if value value default)))

;; Display infrastructure status in a human-readable format
(define (show-infrastructure-status machine-name options)
  "Display comprehensive infrastructure status"
  (let ((verbose (option-ref options 'verbose #f))
        (status-data (get-infrastructure-status machine-name)))
    
    (log-info "Home-lab infrastructure status:")
    (newline)
    
    (for-each 
      (lambda (machine-status)
        (display-machine-status machine-status verbose))
      status-data)
    
    ;; Summary statistics
    (let ((total-machines (length status-data))
          (online-machines (length (filter 
                                   (lambda (status) 
                                     (eq? (assoc-ref status 'connection) 'online))
                                   status-data))))
      (newline)
      (if (= online-machines total-machines)
          (log-success "All ~a machines online ✓" total-machines)
          (log-warn "~a/~a machines online" online-machines total-machines)))))

;; Display status for a single machine
(define (display-machine-status machine-status verbose)
  "Display formatted status for a single machine"
  (let* ((machine-name (assoc-ref machine-status 'machine))
         (machine-type (assoc-ref machine-status 'type))
         (connection (assoc-ref machine-status 'connection))
         (services (assoc-ref machine-status 'services))
         (system-info (assoc-ref machine-status 'system))
         (check-time (assoc-ref machine-status 'check-time)))
    
    ;; Machine header with connection status
    (let ((status-symbol (if (eq? connection 'online) "✅" "❌"))
          (type-label (if (eq? machine-type 'local) "(local)" "(remote)")))
      (format #t "━━━ ~a ~a ~a ━━━~%" machine-name type-label status-symbol))
    
    ;; Connection details
    (if (eq? connection 'online)
        (begin
          (when system-info
            (let ((uptime (assoc-ref system-info 'uptime))
                  (load (assoc-ref system-info 'load))
                  (memory (assoc-ref system-info 'memory))
                  (disk (assoc-ref system-info 'disk)))
              (when uptime (format #t "⏱️  Uptime: ~a~%" uptime))
              (when load (format #t "📊 Load: ~a~%" load))
              (when memory (format #t "🧠 Memory: ~a~%" memory))
              (when disk (format #t "💾 Disk: ~a~%" disk))))
          
          ;; Services status
          (when (not (null? services))
            (format #t "🔧 Services: ")
            (for-each (lambda (service-status)
                        (let ((service-name (symbol->string (car service-status)))
                              (service-state (cdr service-status)))
                          (let ((status-icon (cond 
                                             ((string=? service-state "active") "✅")
                                             ((string=? service-state "inactive") "❌")
                                             ((string=? service-state "failed") "💥")
                                             (else "❓"))))
                            (format #t "~a ~a " service-name status-icon))))
                      services)
            (newline))
          
          (format #t "⚡ Response: ~ams~%" (inexact->exact (round (* check-time 1000)))))
        (format #t "⚠️  Status: Offline~%"))
    
    ;; Verbose information
    (when verbose
      (let ((ssh-config (get-ssh-config machine-name)))
        (when ssh-config
          (format #t "🔗 SSH: ~a~%" (assoc-ref ssh-config 'hostname))
          (let ((ssh-alias (assoc-ref ssh-config 'ssh-alias)))
            (when ssh-alias
              (format #t "🏷️  Alias: ~a~%" ssh-alias))))))
    
    (newline)))

;; Get detailed information about a specific machine
(define (get-machine-details machine-name)
  "Get comprehensive details about a specific machine"
  (let ((machine-config (get-machine-config machine-name)))
    (if (not machine-config)
        (begin
          (log-error "Machine ~a not found in configuration" machine-name)
          #f)
        (let* ((ssh-config (get-ssh-config machine-name))
               (health-status (check-system-health machine-name))
               (current-status (car (get-infrastructure-status machine-name))))
          
          `((name . ,machine-name)
            (config . ,machine-config)
            (ssh . ,ssh-config)
            (status . ,current-status)
            (health . ,health-status)
            (last-updated . ,(current-date)))))))

;; Discover machines on the network
(define (discover-machines)
  "Discover available machines on the network"
  (log-info "Discovering machines on the network...")
  
  (let ((configured-machines (get-all-machines)))
    (log-debug "Configured machines: ~a" configured-machines)
    
    ;; Test connectivity to each configured machine
    (let ((discovery-results
           (map (lambda (machine-name)
                  (log-debug "Testing connectivity to ~a..." machine-name)
                  (let ((reachable (test-ssh-connection machine-name))
                        (ssh-config (get-ssh-config machine-name)))
                    `((machine . ,machine-name)
                      (configured . #t)
                      (reachable . ,reachable)
                      (type . ,(if (and ssh-config (assoc-ref ssh-config 'is-local))
                                  'local 'remote))
                      (hostname . ,(if ssh-config 
                                      (assoc-ref ssh-config 'hostname)
                                      "unknown")))))
                configured-machines)))
      
      ;; TODO: Add network scanning for unconfigured machines
      ;; This could use nmap or similar tools to discover machines
      
      (log-info "Discovery completed")
      discovery-results)))

;; Validate health of a machine with detailed checks
(define (validate-machine-health machine-name . detailed)
  "Perform comprehensive health validation on a machine"
  (let ((run-detailed (if (null? detailed) #f (car detailed))))
    (log-info "Validating health of ~a..." machine-name)
    
    (let ((basic-health (check-system-health machine-name)))
      (if run-detailed
          ;; Extended health checks for detailed mode
          (let ((extended-checks
                 '(("filesystem" . check-filesystem-health)
                   ("network-services" . check-network-services)
                   ("system-logs" . check-system-logs)
                   ("performance" . check-performance-metrics))))
            
            (let ((extended-results
                   (map (lambda (check-pair)
                          (let ((check-name (car check-pair))
                                (check-proc (cdr check-pair)))
                            (log-debug "Running extended check: ~a" check-name)
                            (catch #t
                              (lambda ()
                                `(,check-name . ,(check-proc machine-name)))
                              (lambda (key . args)
                                (log-warn "Extended check ~a failed: ~a" check-name key)
                                `(,check-name . (error . ,key))))))
                        extended-checks)))
              
              `((basic . ,basic-health)
                (extended . ,extended-results)
                (timestamp . ,(current-date)))))
          
          ;; Just basic health checks
          `((basic . ,basic-health)
            (timestamp . ,(current-date)))))))

;; Extended health check functions
(define (check-filesystem-health machine-name)
  "Check filesystem health and disk usage"
  (call-with-values (((success output) 
               (run-remote-command machine-name "df -h && echo '---' && mount | grep -E '^/' | head -5")))
    (if success
        `((status . pass)
          (details . ,(string-trim-right output)))
        `((status . fail)
          (error . "Could not retrieve filesystem information")))))

(define (check-network-services machine-name)
  "Check network service connectivity"
  (let ((services-to-test '(("ssh" "22") ("http" "80") ("https" "443"))))
    (map (lambda (service-pair)
           (let ((service-name (car service-pair))
                 (port (cadr service-pair)))
             (call-with-values (((success output)
                          (run-remote-command machine-name 
                                             (format #f "netstat -ln | grep ':~a ' > /dev/null 2>&1; echo $?" port))))
               `(,service-name . ,(if (and success (string=? (string-trim-right output) "0"))
                                     'listening 'not-listening)))))
         services-to-test)))

(define (check-system-logs machine-name)
  "Check system logs for recent errors"
  (call-with-values (((success output)
               (run-remote-command machine-name 
                                  "journalctl --since='1 hour ago' --priority=err --no-pager | wc -l")))
    (if success
        (let ((error-count (string->number (string-trim-right output))))
          `((status . ,(if (< error-count 10) 'good 'concerning))
            (error-count . ,error-count)))
        `((status . unknown)
          (error . "Could not check system logs")))))

(define (check-performance-metrics machine-name)
  "Get basic performance metrics"
  (let ((metrics-commands
         '(("cpu-usage" "top -bn1 | grep 'Cpu(s)' | awk '{print $2}' | sed 's/%us,//'")
           ("memory-usage" "free | grep Mem | awk '{printf \"%.1f\", ($3/$2) * 100.0}'")
           ("io-wait" "iostat 1 2 | tail -1 | awk '{print $4}'"))))
    
    (map (lambda (metric-pair)
           (let ((metric-name (car metric-pair))
                 (command (cadr metric-pair)))
             (call-with-values (((success output) (run-remote-command machine-name command)))
               `(,(string->symbol metric-name) . 
                 ,(if success (string-trim-right output) "unknown")))))
         metrics-commands)))

;; Get machine metrics for monitoring
(define (get-machine-metrics machine-name . time-range)
  "Get machine metrics for monitoring and analysis"
  (let ((range (if (null? time-range) "1h" (car time-range))))
    (log-debug "Collecting metrics for ~a (range: ~a)" machine-name range)
    
    (let ((current-time (current-date))
          (performance (check-performance-metrics machine-name))
          (health (validate-machine-health machine-name)))
      
      `((machine . ,machine-name)
        (timestamp . ,current-time)
        (performance . ,performance)
        (health . ,health)
        (range . ,range)))))
