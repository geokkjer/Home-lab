;; MCP Message Validation and Schema Enforcement
;; This module implements comprehensive validation for MCP messages and schemas

(define-module (mcp server validation)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (mcp server jsonrpc)
  #:export (validate-mcp-message
            validate-mcp-params
            validate-tool-params
            validate-resource-params
            validate-prompt-params
            validate-schema
            make-validator
            validator?
            validation-error?
            validation-error-message
            validation-error-path
            *mcp-schemas*))

;; Validation error record type
(define-record-type <validation-error>
  (make-validation-error message path data)
  validation-error?
  (message validation-error-message)
  (path validation-error-path)
  (data validation-error-data))

;; Validator record type
(define-record-type <validator>
  (make-validator name schema validate-fn)
  validator?
  (name validator-name)
  (schema validator-schema)
  (validate-fn validator-validate-fn))

;; MCP Schema definitions
(define *mcp-schemas*
  `((initialize . (("type" . "object")
                   ("required" . ("protocolVersion" "capabilities" "clientInfo"))
                   ("properties" . (("protocolVersion" . (("type" . "string")))
                                    ("capabilities" . (("type" . "object")))
                                    ("clientInfo" . (("type" . "object")
                                                     ("required" . ("name" "version"))
                                                     ("properties" . (("name" . (("type" . "string")))
                                                                      ("version" . (("type" . "string")))))))))))
    
    (tools/list . (("type" . "object")
                   ("properties" . (("cursor" . (("type" . "string")))))))
    
    (tools/call . (("type" . "object")
                   ("required" . ("name"))
                   ("properties" . (("name" . (("type" . "string")))
                                    ("arguments" . (("type" . "object")))))))
    
    (resources/list . (("type" . "object")
                       ("properties" . (("cursor" . (("type" . "string")))))))
    
    (resources/read . (("type" . "object")
                       ("required" . ("uri"))
                       ("properties" . (("uri" . (("type" . "string")))))))
    
    (prompts/list . (("type" . "object")
                     ("properties" . (("cursor" . (("type" . "string")))))))
    
    (prompts/get . (("type" . "object")
                    ("required" . ("name"))
                    ("properties" . (("name" . (("type" . "string")))
                                     ("arguments" . (("type" . "object"))))))))) 

;; Core validation functions
(define (validate-mcp-message message)
  "Validate an MCP message structure"
  (cond
   ((jsonrpc-request? message)
    (validate-mcp-request message))
   ((jsonrpc-response? message)
    (validate-mcp-response message))
   ((jsonrpc-notification? message)
    (validate-mcp-notification message))
   ((jsonrpc-error? message)
    (validate-mcp-error message))
   (else
    (make-validation-error "Invalid message type" '() message))))

(define (validate-mcp-request request)
  "Validate an MCP request message"
  (let ((method (jsonrpc-request-method request))
        (params (jsonrpc-request-params request))
        (id (jsonrpc-request-id request)))
    
    ;; Validate method name
    (cond
     ((not (string? method))
      (make-validation-error "Method must be a string" '(method) method))
     
     ((string-null? method)
      (make-validation-error "Method cannot be empty" '(method) method))
     
     ;; Validate method-specific parameters
     (else
      (validate-mcp-params method params)))))

(define (validate-mcp-response response)
  "Validate an MCP response message"
  (let ((id (jsonrpc-response-id response))
        (result (jsonrpc-response-result response)))
    
    ;; Basic response validation
    (if (not (or (string? id) (number? id) (null? id)))
        (make-validation-error "Response ID must be string, number, or null" '(id) id)
        #t)))

(define (validate-mcp-notification notification)
  "Validate an MCP notification message"
  (let ((method (jsonrpc-notification-method notification))
        (params (jsonrpc-notification-params notification)))
    
    ;; Validate method name
    (cond
     ((not (string? method))
      (make-validation-error "Method must be a string" '(method) method))
     
     ((string-null? method)
      (make-validation-error "Method cannot be empty" '(method) method))
     
     ;; Validate method-specific parameters
     (else
      (validate-mcp-params method params)))))

(define (validate-mcp-error error)
  "Validate an MCP error message"
  (let ((id (jsonrpc-error-id error))
        (code (jsonrpc-error-code error))
        (message (jsonrpc-error-message error)))
    
    (cond
     ((not (number? code))
      (make-validation-error "Error code must be a number" '(error code) code))
     
     ((not (string? message))
      (make-validation-error "Error message must be a string" '(error message) message))
     
     (else #t))))

;; Parameter validation
(define (validate-mcp-params method params)
  "Validate parameters for a specific MCP method"
  (let ((schema (assoc-ref *mcp-schemas* (string->symbol method))))
    (if schema
        (validate-schema params schema (list method))
        ;; No schema defined - basic validation
        (if (and params (not (and (list? params) (every pair? params))) (not (list? params)))
            (make-validation-error "Parameters must be object or array" '(params) params)
            #t))))

;; Schema validation engine
(define (validate-schema data schema path)
  "Validate data against a JSON schema"
  (let ((schema-type (assoc-ref schema "type")))
    (match schema-type
      ("object"
       (validate-object-schema data schema path))
      ("array"
       (validate-array-schema data schema path))
      ("string"
       (validate-string-schema data schema path))
      ("number"
       (validate-number-schema data schema path))
      ("integer"
       (validate-integer-schema data schema path))
      ("boolean"
       (validate-boolean-schema data schema path))
      ("null"
       (validate-null-schema data schema path))
      (_
       (make-validation-error "Unknown schema type" path schema-type)))))

(define (validate-object-schema data schema path)
  "Validate object against object schema"
  (cond
   ((not (and (list? data) (every pair? data)))
    (make-validation-error "Expected object" path data))
   
   (else
    (let ((required (assoc-ref schema "required"))
          (properties (assoc-ref schema "properties")))
      
      ;; Check required fields
      (if required
          (let ((missing-fields (filter (lambda (field)
                                          (not (assoc-ref data field)))
                                        required)))
            (if (not (null? missing-fields))
                (make-validation-error 
                 (format #f "Missing required fields: ~a" missing-fields)
                 path
                 missing-fields)
                ;; Validate properties
                (validate-object-properties data properties path)))
          ;; No required fields - validate properties
          (validate-object-properties data properties path))))))

(define (validate-object-properties data properties path)
  "Validate object properties against schema"
  (if (not properties)
      #t
      (let loop ((props (if (and (list? properties) (every pair? properties))
                            properties
                            '())))
        (if (null? props)
            #t
            (let* ((prop (car props))
                   (prop-name (car prop))
                   (prop-schema (cdr prop))
                   (prop-value (assoc-ref data prop-name))
                   (prop-path (append path (list prop-name))))
              
              (if prop-value
                  (let ((validation-result (validate-schema prop-value prop-schema prop-path)))
                    (if (validation-error? validation-result)
                        validation-result
                        (loop (cdr props))))
                  (loop (cdr props))))))))

(define (validate-array-schema data schema path)
  "Validate array against array schema"
  (cond
   ((not (list? data))
    (make-validation-error "Expected array" path data))
   
   (else
    (let ((items-schema (assoc-ref schema "items"))
          (min-items (assoc-ref schema "minItems"))
          (max-items (assoc-ref schema "maxItems")))
      
      ;; Check length constraints
      (let ((length (length data)))
        (cond
         ((and min-items (< length min-items))
          (make-validation-error 
           (format #f "Array too short: ~a < ~a" length min-items)
           path data))
         
         ((and max-items (> length max-items))
          (make-validation-error 
           (format #f "Array too long: ~a > ~a" length max-items)
           path data))
         
         ;; Validate items
         (items-schema
          (validate-array-items data items-schema path))
         
         (else #t)))))))

(define (validate-array-items data items-schema path)
  "Validate array items against schema"
  (let loop ((items data)
             (index 0))
    (if (null? items)
        #t
        (let* ((item (car items))
               (item-path (append path (list index)))
               (validation-result (validate-schema item items-schema item-path)))
          (if (validation-error? validation-result)
              validation-result
              (loop (cdr items) (+ index 1)))))))

(define (validate-string-schema data schema path)
  "Validate string against string schema"
  (cond
   ((not (string? data))
    (make-validation-error "Expected string" path data))
   
   (else
    (let ((min-length (assoc-ref schema "minLength"))
          (max-length (assoc-ref schema "maxLength"))
          (pattern (assoc-ref schema "pattern")))
      
      (let ((length (string-length data)))
        (cond
         ((and min-length (< length min-length))
          (make-validation-error 
           (format #f "String too short: ~a < ~a" length min-length)
           path data))
         
         ((and max-length (> length max-length))
          (make-validation-error 
           (format #f "String too long: ~a > ~a" length max-length)
           path data))
         
         ;; Pattern validation would require regex support
         (else #t)))))))

(define (validate-number-schema data schema path)
  "Validate number against number schema"
  (if (not (number? data))
      (make-validation-error "Expected number" path data)
      #t))

(define (validate-integer-schema data schema path)
  "Validate integer against integer schema"
  (if (not (and (number? data) (integer? data)))
      (make-validation-error "Expected integer" path data)
      #t))

(define (validate-boolean-schema data schema path)
  "Validate boolean against boolean schema"
  (if (not (boolean? data))
      (make-validation-error "Expected boolean" path data)
      #t))

(define (validate-null-schema data schema path)
  "Validate null against null schema"
  (if (not (null? data))
      (make-validation-error "Expected null" path data)
      #t))

;; Specific MCP method validators
(define (validate-tool-params params)
  "Validate tool method parameters"
  (validate-mcp-params "tools/call" params))

(define (validate-resource-params params)
  "Validate resource method parameters"
  (validate-mcp-params "resources/read" params))

(define (validate-prompt-params params)
  "Validate prompt method parameters"
  (validate-mcp-params "prompts/get" params))
