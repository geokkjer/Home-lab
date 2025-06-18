;; MCP Request Router and Method Dispatcher
;; This module implements flexible routing and method dispatch for MCP requests

(define-module (mcp server router)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (mcp server jsonrpc)
  #:use-module (mcp server protocol)
  #:export (make-router
            router?
            register-route
            register-simple-route
            unregister-route
            dispatch-request
            create-default-router
            route-exists?
            list-routes
            *mcp-core-methods*))

;; Core MCP methods that are always available
(define *mcp-core-methods*
  '("initialize" "initialized" "shutdown" "ping" "notifications/message"))

;; Router record type
(define-record-type <router>
  (make-router routes middleware error-handler)
  router?
  (routes router-routes set-router-routes!)
  (middleware router-middleware set-router-middleware!)
  (error-handler router-error-handler set-router-error-handler!))

;; Route record type
(define-record-type <route>
  (make-route pattern handler middleware validation)
  route?
  (pattern route-pattern)
  (handler route-handler)
  (middleware route-middleware)
  (validation route-validation))

;; Router operations
(define* (register-route router pattern handler #:key (middleware '()) (validation #f))
  "Register a new route with the router"
  (let* ((current-routes (router-routes router))
         (new-route (make-route pattern handler middleware validation))
         (updated-routes (acons pattern new-route current-routes)))
    (set-router-routes! router updated-routes)))

(define (unregister-route router pattern)
  "Remove a route from the router"
  (let* ((current-routes (router-routes router))
         (updated-routes (assoc-remove! current-routes pattern)))
    (set-router-routes! router updated-routes)))

(define (route-exists? router pattern)
  "Check if a route exists in the router"
  (assoc-ref (router-routes router) pattern))

(define (list-routes router)
  "List all registered routes"
  (map car (router-routes router)))

;; Request dispatching
(define (dispatch-request router server request)
  "Dispatch a request through the router"
  (let* ((method (jsonrpc-request-method request))
         (id (jsonrpc-request-id request))
         (params (jsonrpc-request-params request))
         (routes (router-routes router))
         (route (assoc-ref routes method)))
    
    (cond
     ;; Route found
     (route
      (dispatch-to-route route server request))
     
     ;; Core MCP method - delegate to protocol handler
     ((member method *mcp-core-methods*)
      (handle-mcp-message server request))
     
     ;; Method not found
     (else
      (let ((error-handler (router-error-handler router)))
        (if error-handler
            (error-handler server request 'method-not-found)
            (make-jsonrpc-error id
                                (assoc-ref *jsonrpc-error-codes* 'method-not-found)
                                (format #f "Method not found: ~a" method)
                                #f)))))))

(define (dispatch-to-route route server request)
  "Dispatch a request to a specific route"
  (let* ((handler (route-handler route))
         (middleware (route-middleware route))
         (validation (route-validation route))
         (id (jsonrpc-request-id request))
         (params (jsonrpc-request-params request)))
    
    (catch #t
      (lambda ()
        ;; Validate parameters if validation function provided
        (when validation
          (let ((validation-result (validation params)))
            (when (not validation-result)
              (throw 'validation-error "Parameter validation failed"))))
        
        ;; Apply middleware in order
        (let ((processed-params (apply-middleware middleware server params)))
          ;; Call the handler
          (let ((result (handler server processed-params)))
            (make-jsonrpc-response id result))))
      
      (lambda (key . args)
        (match key
          ('validation-error
           (make-jsonrpc-error id
                               (assoc-ref *jsonrpc-error-codes* 'invalid-params)
                               "Invalid parameters"
                               args))
          (_
           (make-jsonrpc-error id
                               (assoc-ref *jsonrpc-error-codes* 'internal-error)
                               (format #f "Handler error: ~a" key)
                               args)))))))

(define (apply-middleware middleware-list server params)
  "Apply middleware functions to parameters"
  (fold (lambda (middleware-fn acc)
          (middleware-fn server acc))
        params
        middleware-list))

;; Default error handler
(define (default-error-handler server request error-type)
  "Default error handler for the router"
  (let ((id (jsonrpc-request-id request))
        (method (jsonrpc-request-method request)))
    (match error-type
      ('method-not-found
       (make-jsonrpc-error id
                           (assoc-ref *jsonrpc-error-codes* 'method-not-found)
                           (format #f "Method not found: ~a" method)
                           #f))
      ('invalid-params
       (make-jsonrpc-error id
                           (assoc-ref *jsonrpc-error-codes* 'invalid-params)
                           "Invalid parameters"
                           #f))
      (_
       (make-jsonrpc-error id
                           (assoc-ref *jsonrpc-error-codes* 'internal-error)
                           "Internal error"
                           #f)))))

;; Validation helpers
(define (validate-string-param param)
  "Validate that parameter is a string"
  (string? param))

(define (validate-number-param param)
  "Validate that parameter is a number"
  (number? param))

(define (validate-object-param param)
  "Validate that parameter is a hash table (object)"
  (hash-table? param))

(define (validate-array-param param)
  "Validate that parameter is a list (array)"
  (list? param))

(define (validate-required-fields param required-fields)
  "Validate that all required fields are present in parameter object"
  (and (hash-table? param)
       (every (lambda (field)
                (hash-ref param field #f))
              required-fields)))

;; Middleware helpers
(define (logging-middleware server params)
  "Middleware to log request parameters"
  (format (current-error-port) "Request params: ~a~%" params)
  params)

(define (timing-middleware server params)
  "Middleware to add timing information"
  (let ((start-time (current-time)))
    (format (current-error-port) "Request started at: ~a~%" start-time)
    params))

;; Router factory
(define (create-default-router)
  "Create a router with default settings"
  (make-router '() '() default-error-handler))

;; Convenience function for common route patterns
(define (register-simple-route router method handler)
  "Register a simple route without middleware or validation"
  (register-route router method handler))

(define (register-validated-route router method handler validation-fn)
  "Register a route with parameter validation"
  (register-route router method handler #:validation validation-fn))

(define (register-middleware-route router method handler middleware-list)
  "Register a route with middleware"
  (register-route router method handler #:middleware middleware-list))
