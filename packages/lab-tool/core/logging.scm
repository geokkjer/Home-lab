;; lab/core/logging.scm - Logging functionality

(define-module (lab core logging)
  #:use-module (ice-9 format)
  #:export (log-info
            log-debug
            log-success
            log-error
            log-warn))

(define (log-info format-str . args)
  "Log info message"
  (apply format #t (string-append "[INFO] " format-str "~%") args))

(define (log-debug format-str . args)
  "Log debug message"
  (apply format #t (string-append "[DEBUG] " format-str "~%") args))

(define (log-success format-str . args)
  "Log success message"
  (apply format #t (string-append "[SUCCESS] " format-str "~%") args))

(define (log-error format-str . args)
  "Log error message"
  (apply format #t (string-append "[ERROR] " format-str "~%") args))

(define (log-warn format-str . args)
  "Log warning message"
  (apply format #t (string-append "[WARN] " format-str "~%") args))
