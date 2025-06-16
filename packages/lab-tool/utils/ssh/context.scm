;; utils/ssh/context.scm - SSH context management

(define-module (utils ssh context)
  #:use-module (ice-9 format)
  #:use-module (utils logging)
  #:use-module (utils ssh connection-test)
  #:export (with-connection-context
            with-ssh-connection))

;; Pure function: Execute operation with connection validation
;; Input: connection-validator (thunk -> boolean), operation (thunk)
;; Output: result of operation or #f if connection invalid
(define (with-connection-context connection-validator operation)
  "Pure function to execute operation with connection context"
  (if (connection-validator)
      (catch #t
        operation
        (lambda (key . args)
          (values #f (format #f "Operation failed: ~a ~a" key args))))
      (values #f "Connection validation failed")))

;; Impure wrapper: Execute with SSH connection context and logging
(define (with-ssh-connection machine-name thunk)
  "Execute operation with SSH connection context (with side effects: logging)"
  (let ((connection-validator (lambda () (test-ssh-connection machine-name))))
    (call-with-values 
        (lambda () (with-connection-context connection-validator thunk))
      (lambda (success result)
        (if success
            result
            (begin
              (log-error "SSH operation failed for ~a: ~a" machine-name result)
              #f))))))
