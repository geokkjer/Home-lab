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
            auto-update-status
            get-update-order
            update-single-machine))

;; Helper function for option handling (duplicated from deployment module)
(define (option-ref options key default)
  "Get option value with default fallback"
  (let ((value (assoc-ref options key)))
    (if value value default)))

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

;; Pure function: Determine update order for machines
(define (get-update-order)
  "Get machines in update order - orchestrator last"
  (let* ((all-machines (get-all-machines))
         (current-machine (get-hostname))
         (remote-machines (filter (lambda (machine)
                                   (let* ((machine-str (if (symbol? machine) 
                                                          (symbol->string machine) 
                                                          machine))
                                          (config (get-machine-config machine)))
                                     (and config
                                          (not (equal? machine-str current-machine))
                                          (not (eq? 'local (assoc-ref config 'type))))))
                                 all-machines))
         (local-machines (filter (lambda (machine)
                                  (let* ((machine-str (if (symbol? machine) 
                                                         (symbol->string machine) 
                                                         machine))
                                         (config (get-machine-config machine)))
                                    (or (equal? machine-str current-machine)
                                        (eq? 'local (assoc-ref config 'type)))))
                                all-machines)))
    ;; Return remote machines first, then local/orchestrator machines
    (append remote-machines local-machines)))

;; Impure function: Update a single machine with error handling
(define (update-single-machine machine-name options)
  "Update a single machine with proper error handling"
  (let* ((machine-str (if (symbol? machine-name) 
                         (symbol->string machine-name) 
                         machine-name))
         (is-local (equal? machine-str (get-hostname))))
    
    (log-info "Updating machine: ~a" machine-str)
    (write-update-log "machine-update" "started" machine-str)
    
    (catch #t
      (lambda ()
        (let ((deploy-result (deploy-machine machine-str "switch" options)))
          (if deploy-result
              (begin
                (log-success "Successfully updated ~a" machine-str)
                (write-update-log "machine-update" "success" machine-str)
                #t)
              (begin
                (log-error "Failed to update ~a" machine-str)
                (write-update-log "machine-update" "failed" machine-str)
                #f))))
      (lambda (key . args)
        (log-error "Exception updating ~a: ~a ~a" machine-str key args)
        (write-update-log "machine-update" "error" (format #f "~a: ~a" machine-str key))
        #f))))

;; Impure function: Orchestrated auto-update routine
(define (auto-update-system . args)
  "Perform orchestrated automatic system update (impure - modifies system)"
  (let* ((options (if (null? args) '() (car args)))
         (auto-reboot (option-ref options 'auto-reboot #t))
         (dry-run (option-ref options 'dry-run #f))
         (parallel (option-ref options 'parallel #f))
         (current-machine (get-hostname))
         (update-order (get-update-order)))
    
    (log-info "Starting orchestrated auto-update from: ~a" current-machine)
    (log-info "Update order: ~a" (map (lambda (m) (if (symbol? m) (symbol->string m) m)) update-order))
    (write-update-log "orchestrated-update" "started" current-machine)
    
    (if (not (check-update-health))
        (begin
          (log-error "System health check failed - aborting update")
          (write-update-log "orchestrated-update" "aborted" "health check failed")
          #f)
        (begin
          ;; Update flake inputs first
          (log-info "Updating flake inputs...")
          (let ((flake-result (update-flake options)))
            (if flake-result
                (begin
                  (log-success "Flake update completed")
                  (write-update-log "flake-update" "success" "")
                  
                  ;; Update machines in order
                  (let ((update-results (map (lambda (machine)
                                              (update-single-machine machine options))
                                            update-order)))
                    
                    (let* ((successful-updates (filter identity update-results))
                           (failed-updates (- (length update-results) (length successful-updates)))
                           (all-success (= failed-updates 0)))
                      
                      (log-info "Update summary: ~a successful, ~a failed" 
                               (length successful-updates) failed-updates)
                      
                      (if all-success
                          (begin
                            (log-success "All machines updated successfully")
                            (write-update-log "orchestrated-update" "success" 
                                            (format #f "~a machines" (length successful-updates)))
                            
                            ;; Schedule reboot of orchestrator if enabled and it was updated
                            (if (and auto-reboot (not dry-run)
                                    (member current-machine 
                                           (map (lambda (m) (if (symbol? m) (symbol->string m) m)) 
                                                update-order)))
                                (begin
                                  (log-info "Scheduling orchestrator reboot in 2 minutes...")
                                  (write-update-log "reboot" "scheduled" "orchestrator - 2 minutes")
                                  (system "shutdown -r +2 'Orchestrated auto-update completed - rebooting'")
                                  #t)
                                (begin
                                  (log-info "Orchestrated update complete - no reboot needed")
                                  (write-update-log "orchestrated-update" "completed" "no reboot")
                                  #t)))
                          (begin
                            (log-warn "Some machines failed to update (~a failures)" failed-updates)
                            (write-update-log "orchestrated-update" "partial-failure" 
                                            (format #f "~a failures" failed-updates))
                            ;; Don't reboot orchestrator if there were failures
                            #f)))))
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