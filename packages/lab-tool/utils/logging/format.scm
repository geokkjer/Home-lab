;; utils/logging/format.scm - Pure logging formatting functions

(define-module (utils logging format)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-19)
  #:export (format-timestamp
            format-log-message
            get-color
            color-codes))

;; Pure data: ANSI color codes
(define color-codes
  '((reset . "\x1b[0m")
    (bold . "\x1b[1m")
    (red . "\x1b[31m")
    (green . "\x1b[32m")
    (yellow . "\x1b[33m")
    (blue . "\x1b[34m")
    (magenta . "\x1b[35m")
    (cyan . "\x1b[36m")))

;; Pure function: Get color code by name
(define (get-color name)
  "Pure function to get ANSI color code"
  (assoc-ref color-codes name))

;; Pure function: Format timestamp
(define (format-timestamp)
  "Pure function to format current timestamp"
  (date->string (current-date) "~H:~M:~S"))

;; Pure function: Format complete log message
;; Input: level symbol, color symbol, prefix string, message string, args list
;; Output: formatted log message string
(define (format-log-message level color prefix message args)
  "Pure function to format a complete log message"
  (let ((timestamp (format-timestamp))
        (formatted-msg (apply format #f message args))
        (color-start (get-color color))
        (color-end (get-color 'reset)))
    (format #f "~a~a[lab]~a ~a ~a~%"
            color-start prefix color-end timestamp formatted-msg)))
