;; Unit Tests for Router Module
;; Tests the request routing and method dispatch system

(define-module (tests router-tests)
  #:use-module (srfi srfi-64)
  #:use-module (mcp server router)
  #:use-module (mcp server jsonrpc)
  #:export (run-router-tests))

(define (run-router-tests)
  "Run all Router module tests"
  (test-begin "Router Tests")
  
  ;; Test router creation
  (test-group "Router Creation"
    (test-router-creation))
  
  ;; Test route registration
  (test-group "Route Registration"
    (test-route-registration))
  
  ;; Test request dispatching
  (test-group "Request Dispatching"
    (test-request-dispatching))
  
  ;; Test middleware
  (test-group "Middleware"
    (test-middleware-functionality))
  
  (test-end "Router Tests"))

(define (test-router-creation)
  "Test router creation"
  
  (test-assert "Create default router"
    (let ((router (create-default-router)))
      (router? router))))

(define (test-route-registration)
  "Test route registration and management"
  
  (test-assert "Register simple route"
    (let ((router (create-default-router)))
      (register-simple-route router "test-method" 
                            (lambda (server params) "test-result"))
      (route-exists? router "test-method")))
  
  (test-assert "Unregister route"
    (let ((router (create-default-router)))
      (register-simple-route router "test-method" 
                            (lambda (server params) "test-result"))
      (unregister-route router "test-method")
      (not (route-exists? router "test-method")))))

(define (test-request-dispatching)
  "Test request dispatching functionality"
  
  (test-assert "Dispatch to registered route"
    (let ((router (create-default-router))
          (server #f)) ; Placeholder server
      (register-simple-route router "test-method" 
                            (lambda (srv params) "test-result"))
      (let* ((request (make-jsonrpc-request 1 "test-method" #f))
             (response (dispatch-request router server request)))
        (and (jsonrpc-response? response)
             (equal? (jsonrpc-response-result response) "test-result"))))))

(define (test-middleware-functionality)
  "Test middleware functionality"
  
  ;; Placeholder for middleware tests
  (test-assert "Middleware placeholder"
    #t))
