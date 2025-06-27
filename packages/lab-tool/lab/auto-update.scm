;; lab/auto-update.scm - Auto-update system implementation

(define-module (lab auto-update)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19) ; Date/time
  #:use-module (utils logging)
  #:use-module (utils config)
  #:use-module (lab deployment)
  #:use-module (lab machines)
  #:export (auto-update-system
            schedule-auto-update
            check-update-health
            auto-update-status))

;; Pure function: Generate update log entry
(define (format-update-log-entry timestamp operation status details)
  "Pure function to format update log entry"
  (format #f "~a: ~a - ~a (~a)" timestamp operation status details))

;; Pure function: Check if system is healthy for updates
(define (system-health-check-pure)
  "Pure function returning health check criteria"
  '((disk-space-threshold . 90)
    (required-services . ("systemd"))
    (min-uptime-minutes . 30)))

;; Impure function: Check actual system health
(define (check-update-health)
  "Check if system is ready for updates (impure - checks actual system)"
  (log-info "Checking system health before update...")
  
  (let* ((health-checks (system-health-check-pure))
         (disk-threshold (assoc-ref health-checks 'disk-space-threshold))
         (disk-usage (get-disk-usage))
         (system-running (system-is-running?))
         (uptime-ok (check-minimum-uptime)))
    
    (log-debug "Disk usage: ~a%" disk-usage)
    (log-debug "System running: ~a" system-running)
    (log-debug "Uptime check: ~a" uptime-ok)
    
    (cond
     ((> disk-usage disk-threshold)
      (log-error "Disk usage too high: ~a% (threshold: ~a%)" disk-usage disk-threshold)
      #f)
     ((not system-running)
      (log-error "System not in running state")
      #f)
     ((not uptime-ok)
      (log-error "System uptime too low for safe update")
      #f)
     (else
      (log-success "System health check passed")
      #t))))

;; Impure function: Get disk usage percentage
(define (get-disk-usage)
  "Get root filesystem disk usage percentage"
  (let* ((cmd "df / | tail -1 | awk '{print $5}' | sed 's/%//'")
         (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
         (output (string-trim-both (get-string-all port)))
         (status (close-pipe port)))
    (if (zero? status)
        (string->number output)
        95))) ; Return high usage if command fails

;; Impure function: Check if systemd is running
(define (system-is-running?)
  "Check if system is in running state"
  (let* ((cmd "systemctl is-system-running --quiet")
         (status (system cmd)))
    (zero? status)))

;; Impure function: Check minimum uptime
(define (check-minimum-uptime)
  "Check if system has been running long enough"
  (let* ((cmd "cat /proc/uptime | cut -d' ' -f1")
         (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
         (output (string-trim-both (get-string-all port)))
         (status (close-pipe port)))
    (if (zero? status)
        (let ((uptime-seconds (string->number output)))
          (> uptime-seconds 1800)) ; 30 minutes minimum
        #f)))

;; Impure function: Write update log
(define (write-update-log operation status details)
  "Write update operation to log file"
  (let* ((timestamp (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
         (log-entry (format-update-log-entry timestamp operation status details))
         (log-file "/var/log/lab-auto-update.log"))
    
    (catch #t
      (lambda ()
        (call-with-output-file log-file
          (lambda (port)
            (format port "~a\n" log-entry))
          #:append #t))
      (lambda (key . args)
        (log-error "Failed to write update log: ~a" args)))))

;; Impure function: Main auto-update routine
(define (auto-update-system . args)
  "Perform automatic system update (impure - modifies system)"
  (let* ((options (if (null? args) '() (car args)))
         (auto-reboot (option-ref options 'auto-reboot #t))
         (dry-run (option-ref options 'dry-run #f))
         (machine-name (get-hostname)))
    
    (log-info "Starting auto-update for machine: ~a" machine-name)
    (write-update-log "auto-update" "started" machine-name)
    
    (if (not (check-update-health))
        (begin
          (log-error "System health check failed - aborting update")
          (write-update-log "auto-update" "aborted" "health check failed")
          #f)
        (begin
          ;; Update flake inputs
          (log-info "Updating flake inputs...")
          (let ((flake-result (update-flake options)))
            (if flake-result
                (begin
                  (log-success "Flake update completed")
                  (write-update-log "flake-update" "success" "")
                  
                  ;; Deploy configuration
                  (log-info "Deploying updated configuration...")
                  (let ((deploy-result (deploy-machine machine-name "switch" options)))
                    (if deploy-result
                        (begin
                          (log-success "Configuration deployment completed")
                          (write-update-log "deployment" "success" "switch mode")
                          
                          ;; Schedule reboot if enabled
                          (if (and auto-reboot (not dry-run))
                              (begin
                                (log-info "Scheduling system reboot in 2 minutes...")
                                (write-update-log "reboot" "scheduled" "2 minutes")
                                (system "shutdown -r +2 'Auto-update completed - rebooting'")
                                #t)
                              (begin
                                (log-info "Auto-reboot disabled - update complete")
                                (write-update-log "auto-update" "completed" "no reboot")
                                #t)))
                        (begin
                          (log-error "Configuration deployment failed")
                          (write-update-log "deployment" "failed" "switch mode")
                          #f))))
                (begin
                  (log-error "Flake update failed")
                  (write-update-log "flake-update" "failed" "")
                  #f)))))))

;; Impure function: Get current hostname
(define (get-hostname)
  "Get current system hostname"
  (let* ((cmd "hostname")
         (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
         (output (string-trim-both (get-string-all port)))
         (status (close-pipe port)))
    (if (zero? status)
        output
        "unknown")))

;; Impure function: Show auto-update status
(define (auto-update-status)
  "Display auto-update service status and recent logs"
  (log-info "Checking auto-update status...")
  
  (let ((log-file "/var/log/lab-auto-update.log"))
    (if (file-exists? log-file)
        (begin
          (format #t "Recent auto-update activity:\n")
          (let* ((cmd (format #f "tail -10 ~a" log-file))
                 (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
                 (output (get-string-all port))
                 (status (close-pipe port)))
            (if (zero? status)
                (display output)
                (log-error "Failed to read update log"))))
        (log-info "No auto-update log found"))
    
    ;; Check systemd timer status
    (format #t "\nSystemd timer status:\n")
    (let* ((cmd "systemctl status lab-auto-update.timer --no-pager")
           (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
           (output (get-string-all port))
           (status (close-pipe port)))
      (display output))))

;; Impure function: Schedule auto-update (for manual testing)
(define (schedule-auto-update minutes)
  "Schedule auto-update to run in specified minutes"
  (let ((schedule-cmd (format #f "echo 'lab auto-update' | at now + ~a minutes" minutes)))
    (log-info "Scheduling auto-update in ~a minutes..." minutes)
    (let ((status (system schedule-cmd)))
      (if (zero? status)
          (log-success "Auto-update scheduled successfully")
          (log-error "Failed to schedule auto-update")))))