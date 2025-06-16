;; lab/core/utils.scm - Utility functions

(define-module (lab core utils)
  #:use-module (ice-9 format)
  #:export (with-spinner))

(define (with-spinner message proc)
  "Execute procedure with spinner (stub implementation)"
  (display (format #f "~a..." message))
  (let ((result (proc)))
    (display " done.\n")
    result))
