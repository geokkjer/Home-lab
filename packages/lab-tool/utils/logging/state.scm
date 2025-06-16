;; utils/logging/state.scm - Logging state management

(define-module (utils logging state)
  #:use-module (utils logging level)
  #:export (get-current-log-level
            set-log-level!
            should-log?))

;; Mutable state: Current log level
(define current-log-level 'info)

;; Impure function: Get current log level
(define (get-current-log-level)
  "Get current log level"
  current-log-level)

;; Impure function: Set log level with validation
(define (set-log-level! level)
  "Set current log level (with validation)"
  (if (validate-log-level level)
      (set! current-log-level level)
      (error "Invalid log level" level)))

;; Impure function: Check if message should be logged
(define (should-log? level)
  "Check if message should be logged at current level"
  (should-log-pure current-log-level level))
