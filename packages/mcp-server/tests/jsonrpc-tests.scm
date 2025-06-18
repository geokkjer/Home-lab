;; Unit Tests for JSON-RPC 2.0 Module
;; Tests the foundational JSON-RPC protocol implementation

(define-module (tests jsonrpc-tests)
  #:use-module (srfi srfi-64)
  #:use-module (json)
  #:use-module (mcp server jsonrpc)
  #:export (run-jsonrpc-tests))

(define (run-jsonrpc-tests)
  "Run all JSON-RPC module tests"
  (test-begin "JSON-RPC Tests")
  
  ;; Test JSON-RPC request parsing
  (test-group "Request Parsing"
    (test-jsonrpc-request-parsing))
  
  ;; Test JSON-RPC response creation
  (test-group "Response Creation"
    (test-jsonrpc-response-creation))
  
  ;; Test JSON-RPC error handling
  (test-group "Error Handling"
    (test-jsonrpc-error-handling))
  
  ;; Test JSON-RPC notifications
  (test-group "Notifications"
    (test-jsonrpc-notifications))
  
  ;; Test batch requests
  (test-group "Batch Requests"
    (test-jsonrpc-batch-requests))
  
  ;; Test message validation
  (test-group "Message Validation"
    (test-jsonrpc-validation))
  
  (test-end "JSON-RPC Tests"))

(define (test-jsonrpc-request-parsing)
  "Test JSON-RPC request parsing functionality"
  
  ;; Test valid request parsing
  (test-assert "Parse valid JSON-RPC request"
    (let* ((json-request "{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"params\":{\"foo\":\"bar\"},\"id\":1}")
           (parsed (parse-jsonrpc-message json-request)))
      (and (jsonrpc-request? parsed)
           (equal? (jsonrpc-request-method parsed) "test")
           (equal? (jsonrpc-request-id parsed) 1))))
  
  ;; Test request without params
  (test-assert "Parse request without params"
    (let* ((json-request "{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}")
           (parsed (parse-jsonrpc-message json-request)))
      (and (jsonrpc-request? parsed)
           (equal? (jsonrpc-request-method parsed) "test")
           (equal? (jsonrpc-request-params parsed) #f))))
  
  ;; Test request with string ID
  (test-assert "Parse request with string ID"
    (let* ((json-request "{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":\"abc123\"}")
           (parsed (parse-jsonrpc-message json-request)))
      (and (jsonrpc-request? parsed)
           (equal? (jsonrpc-request-id parsed) "abc123"))))
  
  ;; Test invalid version
  (test-assert "Reject invalid JSON-RPC version"
    (let* ((json-request "{\"jsonrpc\":\"1.0\",\"method\":\"test\",\"id\":1}")
           (parsed (parse-jsonrpc-message json-request)))
      (jsonrpc-error? parsed))))

(define (test-jsonrpc-response-creation)
  "Test JSON-RPC response creation functionality"
  
  ;; Test successful response
  (test-assert "Create successful response"
    (let ((response (make-jsonrpc-response 1 "result-data")))
      (and (jsonrpc-response? response)
           (equal? (jsonrpc-response-id response) 1)
           (equal? (jsonrpc-response-result response) "result-data"))))
  
  ;; Test response serialization
  (test-assert "Serialize response to JSON"
    (let* ((response (make-jsonrpc-response 1 "test-result"))
           (json-str (jsonrpc-message->json response))
           (parsed (json-string->scm json-str)))
      (and (list? parsed)
           (equal? (assoc-ref parsed "jsonrpc") "2.0")
           (equal? (assoc-ref parsed "id") 1)
           (equal? (assoc-ref parsed "result") "test-result")))))

(define (test-jsonrpc-error-handling)
  "Test JSON-RPC error handling functionality"
  
  ;; Test error creation
  (test-assert "Create JSON-RPC error"
    (let ((error (make-jsonrpc-error 1 -32600 "Invalid Request" #f)))
      (and (jsonrpc-error? error)
           (equal? (jsonrpc-error-id error) 1)
           (equal? (jsonrpc-error-code error) -32600)
           (equal? (jsonrpc-error-message error) "Invalid Request"))))
  
  ;; Test parse error handling
  (test-assert "Handle parse error"
    (let ((parsed (parse-jsonrpc-message "invalid json")))
      (and (jsonrpc-error? parsed)
           (equal? (jsonrpc-error-code parsed) -32700))))
  
  ;; Test error with data
  (test-assert "Create error with additional data"
    (let ((error (make-jsonrpc-error 1 -32603 "Internal error" '("extra" "data"))))
      (and (jsonrpc-error? error)
           (equal? (jsonrpc-error-data error) '("extra" "data"))))))

(define (test-jsonrpc-notifications)
  "Test JSON-RPC notification functionality"
  
  ;; Test notification parsing
  (test-assert "Parse JSON-RPC notification"
    (let* ((json-notif "{\"jsonrpc\":\"2.0\",\"method\":\"notify\",\"params\":{\"data\":\"value\"}}")
           (parsed (parse-jsonrpc-message json-notif)))
      (and (jsonrpc-notification? parsed)
           (equal? (jsonrpc-notification-method parsed) "notify"))))
  
  ;; Test notification without params
  (test-assert "Parse notification without params"
    (let* ((json-notif "{\"jsonrpc\":\"2.0\",\"method\":\"notify\"}")
           (parsed (parse-jsonrpc-message json-notif)))
      (and (jsonrpc-notification? parsed)
           (equal? (jsonrpc-notification-params parsed) #f)))))

(define (test-jsonrpc-batch-requests)
  "Test JSON-RPC batch request functionality"
  
  ;; Test batch parsing
  (test-assert "Parse batch requests"
    (let* ((batch-msgs '("{\"jsonrpc\":\"2.0\",\"method\":\"test1\",\"id\":1}"
                        "{\"jsonrpc\":\"2.0\",\"method\":\"test2\",\"id\":2}"))
           (results (handle-jsonrpc-batch batch-msgs)))
      (and (list? results)
           (= (length results) 2)
           (jsonrpc-request? (car results))
           (jsonrpc-request? (cadr results))
           (equal? (jsonrpc-request-method (car results)) "test1")
           (equal? (jsonrpc-request-method (cadr results)) "test2"))))
  
  ;; Test empty batch error
  (test-assert "Reject empty batch"
    (let ((result (handle-jsonrpc-batch '())))
      (and (list? result)
           (= (length result) 1)
           (jsonrpc-error? (car result))))))

(define (test-jsonrpc-validation)
  "Test JSON-RPC message validation"
  
  ;; Test valid message validation
  (test-assert "Validate correct message structure"
    (let* ((json-request "{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}")
           (parsed (parse-jsonrpc-message json-request)))
      (jsonrpc-request? parsed)))
  
  ;; Test invalid method name
  (test-assert "Reject invalid method names"
    (let* ((json-request "{\"jsonrpc\":\"2.0\",\"method\":\"rpc.invalid\",\"id\":1}")
           (parsed (parse-jsonrpc-message json-request)))
      (jsonrpc-error? parsed)))
  
  ;; Test missing required fields
  (test-assert "Reject missing jsonrpc field"
    (let* ((json-request "{\"method\":\"test\",\"id\":1}")
           (parsed (parse-jsonrpc-message json-request)))
      (jsonrpc-error? parsed))))

;; Helper functions for testing
(define (create-test-request method params id)
  "Create a test JSON-RPC request"
  (scm->json-string
   `(("jsonrpc" . "2.0")
     ("method" . ,method)
     ,@(if params `(("params" . ,params)) '())
     ("id" . ,id))))

(define (create-test-notification method params)
  "Create a test JSON-RPC notification"
  (scm->json-string
   `(("jsonrpc" . "2.0")
     ("method" . ,method)
     ,@(if params `(("params" . ,params)) '()))))
