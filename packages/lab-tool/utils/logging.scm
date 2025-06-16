;; utils/logging.scm - Logging utilities for Home Lab Tool

(define-module (utils logging)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-19)
  #:export (log-debug
            log-info
            log-warn
            log-error
            log-success
            set-log-level!
            get-color
            with-spinner))

;; ANSI color codes
(define color-codes
  '((reset . "\x1b[0m")
    (bold . "\x1b[1m")
    (red . "\x1b[31m")
    (green . "\x1b[32m")
    (yellow . "\x1b[33m")
    (blue . "\x1b[34m")
    (magenta . "\x1b[35m")
    (cyan . "\x1b[36m")))

;; Current log level
(define current-log-level 'info)

;; Log levels with numeric values for comparison
(define log-levels
  '((debug . 0)
    (info . 1)
    (warn . 2)
    (error . 3)))

;; Get color code by name
(define (get-color name)
  (assoc-ref color-codes name))

;; Set the current log level
(define (set-log-level! level)
  (set! current-log-level level))

;; Check if a message should be logged at current level
(define (should-log? level)
  (<= (assoc-ref log-levels current-log-level)
      (assoc-ref log-levels level)))

;; Format timestamp for log messages
(define (format-timestamp)
  (date->string (current-date) "~H:~M:~S"))

;; Core logging function with color support
(define (log-with-color level color prefix message . args)
  (when (should-log? level)
    (let ((timestamp (format-timestamp))
          (formatted-msg (apply format #f message args))
          (color-start (get-color color))
          (color-end (get-color 'reset)))
      (format (current-error-port) "~a~a[lab]~a ~a ~a~%"
              color-start prefix color-end timestamp formatted-msg))))

;; Specific logging functions
(define (log-debug message . args)
  (apply log-with-color 'debug 'cyan "DEBUG" message args))

(define (log-info message . args)
  (apply log-with-color 'info 'blue "INFO " message args))

(define (log-warn message . args)
  (apply log-with-color 'warn 'yellow "WARN " message args))

(define (log-error message . args)
  (apply log-with-color 'error 'red "ERROR" message args))

(define (log-success message . args)
  (apply log-with-color 'info 'green "SUCCESS" message args))

;; Spinner utility for long-running operations
(define (with-spinner message thunk)
  (log-info "~a..." message)
  (let ((start-time (current-time)))
    (catch #t
      (lambda ()
        (let ((result (thunk)))
          (let ((elapsed (- (current-time) start-time)))
            (log-success "~a completed in ~as" message elapsed))
          result))
      (lambda (key . args)
        (log-error "~a failed: ~a ~a" message key args)
        (throw key args)))))
