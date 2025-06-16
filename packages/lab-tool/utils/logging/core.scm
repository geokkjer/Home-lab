;; utils/logging/core.scm - Core logging functions

(define-module (utils logging core)
  #:use-module (utils logging state)
  #:use-module (utils logging output)
  #:export (log-with-color
            log-debug
            log-info
            log-warn
            log-error
            log-success))

;; Impure function: Core logging with color and level checking
(define (log-with-color level color prefix message . args)
  "Log message with color if level is appropriate"
  (when (should-log? level)
    (log-to-port (current-error-port) level color prefix message args)))

;; Specific logging functions - each does one thing well
(define (log-debug message . args)
  "Log debug message"
  (apply log-with-color 'debug 'cyan "DEBUG" message args))

(define (log-info message . args)
  "Log info message"
  (apply log-with-color 'info 'blue "INFO " message args))

(define (log-warn message . args)
  "Log warning message"
  (apply log-with-color 'warn 'yellow "WARN " message args))

(define (log-error message . args)
  "Log error message"
  (apply log-with-color 'error 'red "ERROR" message args))

(define (log-success message . args)
  "Log success message"
  (apply log-with-color 'info 'green "SUCCESS" message args))
