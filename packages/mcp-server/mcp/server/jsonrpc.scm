;; JSON-RPC 2.0 Protocol Implementation for MCP
;; This module implements the foundational JSON-RPC 2.0 protocol handling
;; as required by the Model Context Protocol (MCP) specification.

(define-module (mcp server jsonrpc)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (make-jsonrpc-request
            make-jsonrpc-response
            make-jsonrpc-error
            make-jsonrpc-notification
            parse-jsonrpc-message
            validate-jsonrpc-message
            jsonrpc-request?
            jsonrpc-response?
            jsonrpc-notification?
            jsonrpc-error?
            jsonrpc-request-id
            jsonrpc-request-method
            jsonrpc-request-params
            jsonrpc-response-id
            jsonrpc-response-result
            jsonrpc-error-code
            jsonrpc-error-message
            jsonrpc-error-data
            jsonrpc-error-id
            jsonrpc-notification-method
            jsonrpc-notification-params
            handle-jsonrpc-batch
            jsonrpc-message->json
            *jsonrpc-error-codes*))

;; JSON-RPC 2.0 Error Codes
(define *jsonrpc-error-codes*
  '((parse-error . -32700)
    (invalid-request . -32600)
    (method-not-found . -32601)
    (invalid-params . -32602)
    (internal-error . -32603)
    (server-error-start . -32099)
    (server-error-end . -32000)))

;; Record types for JSON-RPC messages
(define-record-type <jsonrpc-request>
  (make-jsonrpc-request id method params)
  jsonrpc-request?
  (id jsonrpc-request-id)
  (method jsonrpc-request-method)
  (params jsonrpc-request-params))

(define-record-type <jsonrpc-response>
  (make-jsonrpc-response id result)
  jsonrpc-response?
  (id jsonrpc-response-id)
  (result jsonrpc-response-result))

(define-record-type <jsonrpc-error>
  (make-jsonrpc-error id code message data)
  jsonrpc-error?
  (id jsonrpc-error-id)
  (code jsonrpc-error-code)
  (message jsonrpc-error-message)
  (data jsonrpc-error-data))

(define-record-type <jsonrpc-notification>
  (make-jsonrpc-notification method params)
  jsonrpc-notification?
  (method jsonrpc-notification-method)
  (params jsonrpc-notification-params))

;; Validation functions
(define (valid-jsonrpc-version? version)
  "Check if the JSON-RPC version is valid (must be '2.0')"
  (and (string? version) (string=? version "2.0")))

(define (valid-method-name? method)
  "Check if the method name is valid (string, not starting with 'rpc.')"
  (and (string? method)
       (not (string-prefix? "rpc." method))))

(define (valid-id? id)
  "Check if the ID is valid (string, number, or null)"
  (or (string? id)
      (number? id)
      (null? id)))

;; Message parsing and validation
(define (parse-jsonrpc-message json-string)
  "Parse a JSON-RPC message from a JSON string"
  (catch 'json-invalid
    (lambda ()
      (let ((parsed (json-string->scm json-string)))
        (validate-and-create-message parsed)))
    (lambda (key . args)
      (make-jsonrpc-error #f
                          (assoc-ref *jsonrpc-error-codes* 'parse-error)
                          "Parse error"
                          #f))))

(define (validate-jsonrpc-message message)
  "Validate a parsed JSON-RPC message structure"
  (cond
   ((not (list? message))
    (make-jsonrpc-error #f
                        (assoc-ref *jsonrpc-error-codes* 'invalid-request)
                        "Invalid Request: message must be an object"
                        #f))
   ((not (valid-jsonrpc-version? (assoc-ref message "jsonrpc")))
    (make-jsonrpc-error (assoc-ref message "id")
                        (assoc-ref *jsonrpc-error-codes* 'invalid-request)
                        "Invalid Request: jsonrpc version must be '2.0'"
                        #f))
   (else #t)))

(define (validate-and-create-message parsed)
  "Validate and create appropriate message type from parsed JSON"
  (let ((validation-result (validate-jsonrpc-message parsed)))
    (if (jsonrpc-error? validation-result)
        validation-result
        (create-message-from-parsed parsed))))

(define (create-message-from-parsed parsed)
  "Create appropriate message type from validated parsed JSON"
  (let ((method (assoc-ref parsed "method"))
        (id (assoc-ref parsed "id"))
        (params (assoc-ref parsed "params"))
        (result (assoc-ref parsed "result"))
        (error (assoc-ref parsed "error")))
    (cond
     ;; Response with result
     ((and (not method) result (not error))
      (if (not id)
          (make-jsonrpc-error #f
                              (assoc-ref *jsonrpc-error-codes* 'invalid-request)
                              "Invalid Request: response must have id"
                              #f)
          (make-jsonrpc-response id result)))
     
     ;; Error response
     ((and (not method) (not result) error)
      (if (not id)
          (make-jsonrpc-error #f
                              (assoc-ref *jsonrpc-error-codes* 'invalid-request)
                              "Invalid Request: error response must have id"
                              #f)
          (let ((error-code (assoc-ref error "code"))
                (error-message (assoc-ref error "message"))
                (error-data (assoc-ref error "data")))
            (make-jsonrpc-error id error-code error-message error-data))))
     
     ;; Request or notification
     ((and method (string? method))
      (if (not (valid-method-name? method))
          (make-jsonrpc-error id
                              (assoc-ref *jsonrpc-error-codes* 'invalid-request)
                              "Invalid Request: invalid method name"
                              #f)
          (if (not id)
              ;; Notification (no id)
              (make-jsonrpc-notification method params)
              ;; Request (has id)
              (if (not (valid-id? id))
                  (make-jsonrpc-error id
                                      (assoc-ref *jsonrpc-error-codes* 'invalid-request)
                                      "Invalid Request: invalid id"
                                      #f)
                  (make-jsonrpc-request id method params)))))
     
     ;; Invalid message
     (else
      (make-jsonrpc-error id
                          (assoc-ref *jsonrpc-error-codes* 'invalid-request)
                          "Invalid Request: message structure is invalid"
                          #f)))))

;; Batch request handling
(define (handle-jsonrpc-batch messages)
  "Handle a batch of JSON-RPC messages"
  (if (and (list? messages) (not (null? messages)))
      (map parse-jsonrpc-message messages)
      (list (make-jsonrpc-error #f
                                (assoc-ref *jsonrpc-error-codes* 'invalid-request)
                                "Invalid Request: batch must be non-empty array"
                                #f))))

;; Message serialization helpers
(define (jsonrpc-message->json message)
  "Convert a JSON-RPC message to JSON string"
  (cond
   ((jsonrpc-request? message)
    (scm->json-string
     `(("jsonrpc" . "2.0")
       ("id" . ,(jsonrpc-request-id message))
       ("method" . ,(jsonrpc-request-method message))
       ,@(if (jsonrpc-request-params message)
             `(("params" . ,(jsonrpc-request-params message)))
             '()))))
   
   ((jsonrpc-response? message)
    (scm->json-string
     `(("jsonrpc" . "2.0")
       ("id" . ,(jsonrpc-response-id message))
       ("result" . ,(jsonrpc-response-result message)))))
   
   ((jsonrpc-error? message)
    (scm->json-string
     `(("jsonrpc" . "2.0")
       ("id" . ,(jsonrpc-error-id message))
       ("error" . (("code" . ,(jsonrpc-error-code message))
                   ("message" . ,(jsonrpc-error-message message))
                   ,@(if (jsonrpc-error-data message)
                         `(("data" . ,(jsonrpc-error-data message)))
                         '()))))))
   
   ((jsonrpc-notification? message)
    (scm->json-string
     `(("jsonrpc" . "2.0")
       ("method" . ,(jsonrpc-notification-method message))
       ,@(if (jsonrpc-notification-params message)
             `(("params" . ,(jsonrpc-notification-params message)))
             '()))))
   
   (else
    (throw 'invalid-message "Unknown message type" message))))
