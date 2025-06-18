;; Unit Tests for Validation Module
;; Tests the message validation and schema enforcement

(define-module (tests validation-tests)
  #:use-module (srfi srfi-64)
  #:use-module (mcp server validation)
  #:use-module (mcp server jsonrpc)
  #:export (run-validation-tests))

(define (run-validation-tests)
  "Run all Validation module tests"
  (test-begin "Validation Tests")
  
  ;; Test message validation
  (test-group "Message Validation"
    (test-message-validation))
  
  ;; Test schema validation
  (test-group "Schema Validation"
    (test-schema-validation))
  
  ;; Test parameter validation
  (test-group "Parameter Validation"
    (test-parameter-validation))
  
  (test-end "Validation Tests"))

(define (test-message-validation)
  "Test MCP message validation"
  
  (test-assert "Validate valid request"
    (let ((request (make-jsonrpc-request 1 "test-method" #f)))
      (not (validation-error? (validate-mcp-message request)))))
  
  (test-assert "Validate valid response"
    (let ((response (make-jsonrpc-response 1 "result")))
      (not (validation-error? (validate-mcp-message response))))))

(define (test-schema-validation)
  "Test JSON schema validation"
  
  ;; Test object schema validation
  (test-assert "Validate object against schema"
    (let ((data `(("name" . "test")
                 ("version" . "1.0")))
          (schema `(("type" . "object")
                   ("required" . ("name" "version"))
                   ("properties" . (("name" . (("type" . "string")))
                                   ("version" . (("type" . "string"))))))))
      (not (validation-error? (validate-schema data schema '())))))
  
  ;; Test required field validation
  (test-assert "Reject missing required fields"
    (let ((data `(("name" . "test")))
          (schema `(("type" . "object")
                   ("required" . ("name" "version")))))
      (validation-error? (validate-schema data schema '())))))

(define (test-parameter-validation)
  "Test MCP method parameter validation"
  
  ;; Test tool parameter validation
  (test-assert "Validate tool parameters"
    (let ((params `(("name" . "test-tool")
                   ("arguments" . (("arg1" . "value1"))))))
      (not (validation-error? (validate-tool-params params))))))
