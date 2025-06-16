;; utils/logging/level.scm - Pure log level management

(define-module (utils logging level)
  #:export (log-levels
            should-log-pure
            validate-log-level))

;; Pure data: Log levels with numeric values for comparison
(define log-levels
  '((debug . 0)
    (info . 1)
    (warn . 2)
    (error . 3)))

;; Pure function: Check if message should be logged at given levels
;; Input: current-level symbol, message-level symbol
;; Output: #t if should log, #f otherwise
(define (should-log-pure current-level message-level)
  "Pure function to determine if message should be logged"
  (let ((current-value (assoc-ref log-levels current-level))
        (message-value (assoc-ref log-levels message-level)))
    (and current-value message-value
         (<= current-value message-value))))

;; Pure function: Validate log level
;; Input: level symbol
;; Output: #t if valid, #f otherwise
(define (validate-log-level level)
  "Pure function to validate log level"
  (assoc-ref log-levels level))
