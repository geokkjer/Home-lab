;; utils/ssh/retry.scm - Pure retry logic

(define-module (utils ssh retry)
  #:use-module (utils logging)
  #:use-module (utils ssh remote-command)
  #:export (with-retry
            run-command-with-retry))

;; Pure function: Retry operation with exponential backoff
;; Input: operation (thunk), max-retries number, delay-fn (retry-count -> seconds)
;; Output: result of operation or #f if all retries failed
(define (with-retry operation max-retries . delay-fn)
  "Pure retry logic - operation should return (values success? result)"
  (let ((delay-func (if (null? delay-fn) 
                        (lambda (retry) (* retry 2))  ; Default: exponential backoff
                        (car delay-fn))))
    (let loop ((retries 0))
      (call-with-values operation
        (lambda (success result)
          (if success
              (values #t result)
              (if (< retries max-retries)
                  (begin
                    (sleep (delay-func retries))
                    (loop (+ retries 1)))
                  (values #f result))))))))

;; Impure wrapper: Run command with retry and logging
(define (run-command-with-retry machine-name command max-retries . args)
  "Run command with retry logic (with side effects: logging)"
  (let ((operation (lambda ()
                     (apply run-remote-command machine-name command args))))
    (let loop ((retries 0))
      (call-with-values operation
        (lambda (success output)
          (if success
              (values #t output)
              (if (< retries max-retries)
                  (begin
                    (log-warn "Command failed, retrying (~a/~a)..." (+ retries 1) max-retries)
                    (sleep 2)
                    (loop (+ retries 1)))
                  (begin
                    (log-error "Command failed after ~a retries" max-retries)
                    (values #f output))))))))))
