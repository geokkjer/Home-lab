;; MCP Error Handling and Recovery
;; This module implements comprehensive error handling and recovery mechanisms

(define-module (mcp server error-handling)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (mcp server jsonrpc)
  #:use-module (mcp server validation)
  #:export (make-error-handler
            error-handler?
            handle-error
            recover-from-error
            make-circuit-breaker
            circuit-breaker?
            circuit-breaker-call
            circuit-breaker-state
            create-circuit-breaker
            create-default-error-handler
            create-simple-error-handler
            log-error
            *error-types*
            *recovery-strategies*))

;; Error types
(define *error-types*
  '((protocol-error . "Protocol violation or malformed message")
    (transport-error . "Transport layer failure")
    (method-error . "Method execution failure")
    (validation-error . "Parameter validation failure")
    (timeout-error . "Operation timeout")
    (connection-error . "Connection failure")
    (authentication-error . "Authentication failure")
    (authorization-error . "Authorization failure")
    (resource-error . "Resource unavailable")
    (internal-error . "Internal server error")))

;; Recovery strategies
(define *recovery-strategies*
  '((retry . "Retry the operation")
    (fallback . "Use fallback mechanism")
    (circuit-break . "Open circuit breaker")
    (graceful-degradation . "Reduce functionality")
    (fail-fast . "Fail immediately")
    (ignore . "Ignore the error")))

;; Error handler record type
(define-record-type <error-handler>
  (make-error-handler strategy fallback-handler retry-config circuit-breaker logger)
  error-handler?
  (strategy error-handler-strategy)
  (fallback-handler error-handler-fallback)
  (retry-config error-handler-retry-config)
  (circuit-breaker error-handler-circuit-breaker)
  (logger error-handler-logger))

;; Circuit breaker record type
(define-record-type <circuit-breaker>
  (make-circuit-breaker state failure-count threshold timeout last-failure-time)
  circuit-breaker?
  (state circuit-breaker-state set-circuit-breaker-state!)
  (failure-count circuit-breaker-failure-count set-circuit-breaker-failure-count!)
  (threshold circuit-breaker-threshold)
  (timeout circuit-breaker-timeout)
  (last-failure-time circuit-breaker-last-failure-time set-circuit-breaker-last-failure-time!))

;; Retry configuration record type
(define-record-type <retry-config>
  (make-retry-config max-attempts delay backoff-factor max-delay)
  retry-config?
  (max-attempts retry-config-max-attempts)
  (delay retry-config-delay)
  (backoff-factor retry-config-backoff-factor)
  (max-delay retry-config-max-delay))

;; Main error handling function
(define* (handle-error error-handler error-type error-data #:optional (context #f))
  "Handle an error using the specified error handler"
  (let ((strategy (error-handler-strategy error-handler))
        (logger (error-handler-logger error-handler)))
    
    ;; Log the error
    (when logger
      (log-error logger error-type error-data context))
    
    ;; Apply error handling strategy
    (match strategy
      ('retry
       (handle-retry-error error-handler error-type error-data context))
      ('fallback
       (handle-fallback-error error-handler error-type error-data context))
      ('circuit-break
       (handle-circuit-breaker-error error-handler error-type error-data context))
      ('graceful-degradation
       (handle-graceful-degradation error-handler error-type error-data context))
      ('fail-fast
       (handle-fail-fast-error error-handler error-type error-data context))
      ('ignore
       (handle-ignore-error error-handler error-type error-data context))
      (_
       (handle-default-error error-handler error-type error-data context)))))

;; Retry error handling
(define (handle-retry-error error-handler error-type error-data context)
  "Handle error with retry strategy"
  (let ((retry-config (error-handler-retry-config error-handler)))
    (if retry-config
        (retry-operation retry-config
                         (lambda () (recover-from-error error-type error-data context))
                         error-type)
        (make-jsonrpc-error #f
                            (assoc-ref *jsonrpc-error-codes* 'internal-error)
                            "Retry failed - no retry configuration"
                            error-data))))

(define (retry-operation retry-config operation error-type)
  "Retry an operation according to retry configuration"
  (let loop ((attempts 0)
             (delay (retry-config-delay retry-config)))
    (catch #t
      (lambda ()
        (operation))
      (lambda (key . args)
        (let ((next-attempt (+ attempts 1)))
          (if (>= next-attempt (retry-config-max-attempts retry-config))
              ;; Max attempts reached
              (make-jsonrpc-error #f
                                  (assoc-ref *jsonrpc-error-codes* 'internal-error)
                                  (format #f "Operation failed after ~a attempts" next-attempt)
                                  (list error-type key args))
              ;; Retry with backoff
              (begin
                (usleep (* delay 1000)) ; Convert to microseconds
                (let ((next-delay (min (* delay (retry-config-backoff-factor retry-config))
                                       (retry-config-max-delay retry-config))))
                  (loop next-attempt next-delay)))))))))

;; Fallback error handling
(define (handle-fallback-error error-handler error-type error-data context)
  "Handle error with fallback strategy"
  (let ((fallback-handler (error-handler-fallback error-handler)))
    (if fallback-handler
        (catch #t
          (lambda ()
            (fallback-handler error-type error-data context))
          (lambda (key . args)
            (make-jsonrpc-error #f
                                (assoc-ref *jsonrpc-error-codes* 'internal-error)
                                "Fallback handler failed"
                                (list error-type key args))))
        (make-jsonrpc-error #f
                            (assoc-ref *jsonrpc-error-codes* 'internal-error)
                            "No fallback handler available"
                            error-data))))

;; Circuit breaker error handling
(define (handle-circuit-breaker-error error-handler error-type error-data context)
  "Handle error with circuit breaker strategy"
  (let ((circuit-breaker (error-handler-circuit-breaker error-handler)))
    (if circuit-breaker
        (begin
          (record-circuit-breaker-failure circuit-breaker)
          (make-jsonrpc-error #f
                              (assoc-ref *jsonrpc-error-codes* 'internal-error)
                              "Circuit breaker activated"
                              error-data))
        (make-jsonrpc-error #f
                            (assoc-ref *jsonrpc-error-codes* 'internal-error)
                            "No circuit breaker configured"
                            error-data))))

;; Other error handling strategies
(define (handle-graceful-degradation error-handler error-type error-data context)
  "Handle error with graceful degradation"
  (make-jsonrpc-error #f
                      (assoc-ref *jsonrpc-error-codes* 'internal-error)
                      "Service degraded due to error"
                      error-data))

(define (handle-fail-fast-error error-handler error-type error-data context)
  "Handle error with fail-fast strategy"
  (make-jsonrpc-error #f
                      (assoc-ref *jsonrpc-error-codes* 'internal-error)
                      "Operation failed immediately"
                      error-data))

(define (handle-ignore-error error-handler error-type error-data context)
  "Handle error by ignoring it"
  #f) ; Return nothing for ignored errors

(define (handle-default-error error-handler error-type error-data context)
  "Default error handling"
  (make-jsonrpc-error #f
                      (assoc-ref *jsonrpc-error-codes* 'internal-error)
                      (format #f "Unhandled error: ~a" error-type)
                      error-data))

;; Circuit breaker implementation
(define (create-circuit-breaker threshold timeout)
  "Create a new circuit breaker"
  (make-circuit-breaker 'closed 0 threshold timeout #f))

(define (circuit-breaker-call circuit-breaker operation)
  "Execute operation through circuit breaker"
  (let ((state (circuit-breaker-state circuit-breaker)))
    (match state
      ('open
       (if (circuit-breaker-can-retry? circuit-breaker)
           (begin
             (set-circuit-breaker-state! circuit-breaker 'half-open)
             (circuit-breaker-try-operation circuit-breaker operation))
           (make-jsonrpc-error #f
                               (assoc-ref *jsonrpc-error-codes* 'internal-error)
                               "Circuit breaker is open"
                               #f)))
      ('half-open
       (circuit-breaker-try-operation circuit-breaker operation))
      ('closed
       (circuit-breaker-try-operation circuit-breaker operation)))))

(define (circuit-breaker-try-operation circuit-breaker operation)
  "Try to execute operation and update circuit breaker state"
  (catch #t
    (lambda ()
      (let ((result (operation)))
        ;; Success - reset circuit breaker
        (set-circuit-breaker-failure-count! circuit-breaker 0)
        (set-circuit-breaker-state! circuit-breaker 'closed)
        result))
    (lambda (key . args)
      ;; Failure - update circuit breaker
      (record-circuit-breaker-failure circuit-breaker)
      (throw key args))))

(define (record-circuit-breaker-failure circuit-breaker)
  "Record a failure in the circuit breaker"
  (let ((failure-count (+ (circuit-breaker-failure-count circuit-breaker) 1)))
    (set-circuit-breaker-failure-count! circuit-breaker failure-count)
    (set-circuit-breaker-last-failure-time! circuit-breaker (current-time))
    
    (when (>= failure-count (circuit-breaker-threshold circuit-breaker))
      (set-circuit-breaker-state! circuit-breaker 'open))))

(define (circuit-breaker-can-retry? circuit-breaker)
  "Check if circuit breaker can retry (timeout expired)"
  (let ((last-failure (circuit-breaker-last-failure-time circuit-breaker))
        (timeout (circuit-breaker-timeout circuit-breaker)))
    (and last-failure
         (> (- (current-time) last-failure) timeout))))

;; Recovery functions
(define (recover-from-error error-type error-data context)
  "Attempt to recover from an error"
  (match error-type
    ('connection-error
     (recover-connection-error error-data context))
    ('timeout-error
     (recover-timeout-error error-data context))
    ('validation-error
     (recover-validation-error error-data context))
    (_
     (recover-generic-error error-type error-data context))))

(define (recover-connection-error error-data context)
  "Recover from connection error"
  ;; Attempt to reconnect
  (format (current-error-port) "Attempting to recover from connection error~%")
  #f) ; Placeholder

(define (recover-timeout-error error-data context)
  "Recover from timeout error"
  ;; Reset timeout and try again
  (format (current-error-port) "Attempting to recover from timeout error~%")
  #f) ; Placeholder

(define (recover-validation-error error-data context)
  "Recover from validation error"
  ;; Cannot recover from validation errors
  (throw 'validation-error "Cannot recover from validation error" error-data))

(define (recover-generic-error error-type error-data context)
  "Generic error recovery"
  (format (current-error-port) "Attempting generic recovery for ~a~%" error-type)
  #f) ; Placeholder

;; Logging functions
(define (log-error logger error-type error-data context)
  "Log an error using the specified logger"
  (if logger
      (logger error-type error-data context)
      (default-error-logger error-type error-data context)))

(define (default-error-logger error-type error-data context)
  "Default error logger"
  (let ((timestamp (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time)))))
    (format (current-error-port) 
            "[~a] ERROR: ~a - ~a~%"
            timestamp
            error-type
            error-data)
    (when context
      (format (current-error-port) "Context: ~a~%" context))))

;; Factory functions
(define (create-default-error-handler)
  "Create an error handler with default settings"
  (make-error-handler 'retry
                      #f
                      (make-retry-config 3 1000 2 10000)
                      (create-circuit-breaker 5 30)
                      default-error-logger))

(define (create-simple-error-handler strategy)
  "Create a simple error handler with the specified strategy"
  (make-error-handler strategy #f #f #f default-error-logger))
