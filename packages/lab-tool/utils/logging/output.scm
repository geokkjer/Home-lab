;; utils/logging/output.scm - Pure logging output functions

(define-module (utils logging output)
  #:use-module (utils logging format)
  #:use-module (utils logging level)
  #:export (log-message-pure
            log-to-port))

;; Pure function: Create log message without side effects
;; Input: level, color, prefix, message, args
;; Output: formatted log message string
(define (log-message-pure level color prefix message args)
  "Pure function to create formatted log message"
  (format-log-message level color prefix message args))

;; Impure function: Write log message to port
;; Input: port, level, color, prefix, message, args
;; Output: unspecified (side effect: writes to port)
(define (log-to-port port level color prefix message args)
  "Write formatted log message to specified port"
  (let ((formatted-message (log-message-pure level color prefix message args)))
    (display formatted-message port)
    (force-output port)))
