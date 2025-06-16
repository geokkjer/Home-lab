;; utils/logging/spinner.scm - Spinner utility for long operations

(define-module (utils logging spinner)
  #:use-module (utils logging core)
  #:export (with-spinner))

;; Pure function: Calculate elapsed time
;; Input: start-time, end-time
;; Output: elapsed seconds
(define (calculate-elapsed start-time end-time)
  "Pure function to calculate elapsed time"
  (- end-time start-time))

;; Impure function: Execute operation with spinner logging
(define (with-spinner message thunk)
  "Execute operation with progress logging"
  (log-info "~a..." message)
  (let ((start-time (current-time)))
    (catch #t
      (lambda ()
        (let ((result (thunk)))
          (let ((elapsed (calculate-elapsed start-time (current-time))))
            (log-success "~a completed in ~as" message elapsed))
          result))
      (lambda (key . args)
        (log-error "~a failed: ~a ~a" message key args)
        (throw key args)))))
