;; MCP Protocol Core Implementation
;; This module implements the core Model Context Protocol (MCP) server functionality
;; building on the JSON-RPC foundation.

(define-module (mcp server protocol)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (mcp server jsonrpc)
  #:export (make-mcp-server
            mcp-server?
            mcp-server-name
            mcp-server-version
            mcp-server-capabilities
            mcp-server-handlers
            mcp-server-initialized?
            register-mcp-handler
            handle-mcp-message
            mcp-initialize
            mcp-initialized
            mcp-shutdown
            create-mcp-server
            *mcp-protocol-version*
            *mcp-server-capabilities*))

;; MCP Protocol version
(define *mcp-protocol-version* "2024-11-05")

;; Default server capabilities
(define *mcp-server-capabilities*
  '((tools . #t)
    (resources . #t)
    (prompts . #t)
    (logging . #t)))

;; MCP Server record type
(define-record-type <mcp-server>
  (make-mcp-server name version capabilities handlers initialized?)
  mcp-server?
  (name mcp-server-name)
  (version mcp-server-version)
  (capabilities mcp-server-capabilities)
  (handlers mcp-server-handlers set-mcp-server-handlers!)
  (initialized? mcp-server-initialized? set-mcp-server-initialized!))

;; Register a handler for a specific MCP method
(define (register-mcp-handler server method handler)
  "Register a handler function for a specific MCP method"
  (let ((current-handlers (mcp-server-handlers server)))
    (set-mcp-server-handlers! server
                              (assoc-set! current-handlers method handler))))

;; Main message handler
(define (handle-mcp-message server message)
  "Handle an MCP message (request or notification)"
  (cond
   ((jsonrpc-request? message)
    (handle-mcp-request server message))
   ((jsonrpc-notification? message)
    (handle-mcp-notification server message))
   (else
    (make-jsonrpc-error #f
                        (assoc-ref *jsonrpc-error-codes* 'invalid-request)
                        "Invalid message format"
                        #f))))

(define (handle-mcp-request server request)
  "Handle an MCP request message"
  (let* ((id (jsonrpc-request-id request))
         (method (jsonrpc-request-method request))
         (params (jsonrpc-request-params request))
         (handlers (mcp-server-handlers server))
         (handler (assoc-ref handlers method)))
    
    (cond
     ;; Core protocol methods
     ((string=? method "initialize")
      (mcp-initialize server id params))
     
     ((string=? method "shutdown")
      (mcp-shutdown server id))
     
     ;; Custom handler
     (handler
      (catch #t
        (lambda ()
          (let ((result (handler server params)))
            (make-jsonrpc-response id result)))
        (lambda (key . args)
          (make-jsonrpc-error id
                              (assoc-ref *jsonrpc-error-codes* 'internal-error)
                              (format #f "Handler error: ~a" key)
                              args))))
     
     ;; Method not found
     (else
      (make-jsonrpc-error id
                          (assoc-ref *jsonrpc-error-codes* 'method-not-found)
                          (format #f "Method not found: ~a" method)
                          #f)))))

(define (handle-mcp-notification server notification)
  "Handle an MCP notification message"
  (let* ((method (jsonrpc-notification-method notification))
         (params (jsonrpc-notification-params notification))
         (handlers (mcp-server-handlers server))
         (handler (assoc-ref handlers method)))
    
    (cond
     ;; Core protocol notifications
     ((string=? method "initialized")
      (mcp-initialized server params))
     
     ;; Custom handler
     (handler
      (catch #t
        (lambda ()
          (handler server params)
          #t) ; Notifications don't return responses
        (lambda (key . args)
          ;; Log error but don't send response for notifications
          (format (current-error-port) "Notification handler error: ~a ~a~%" key args)
          #f)))
     
     ;; Unknown notification - ignore silently per JSON-RPC spec
     (else #t))))

;; Core MCP protocol methods
(define (mcp-initialize server id params)
  "Handle MCP initialize request"
  (let* ((client-info (assoc-ref params "clientInfo"))
         (protocol-version (assoc-ref params "protocolVersion"))
         (capabilities (assoc-ref params "capabilities")))
    
    ;; Validate protocol version
    (if (and protocol-version (not (string=? protocol-version *mcp-protocol-version*)))
        (make-jsonrpc-error id
                            (assoc-ref *jsonrpc-error-codes* 'invalid-params)
                            (format #f "Unsupported protocol version: ~a" protocol-version)
                            #f)
        
        ;; Return initialization response
        (make-jsonrpc-response id
                               `(("protocolVersion" . ,*mcp-protocol-version*)
                                 ("capabilities" . ,(mcp-server-capabilities server))
                                 ("serverInfo" . (("name" . ,(mcp-server-name server))
                                                  ("version" . ,(mcp-server-version server)))))))))

(define (mcp-initialized server params)
  "Handle MCP initialized notification"
  (set-mcp-server-initialized! server #t)
  #t)

(define (mcp-shutdown server id)
  "Handle MCP shutdown request"
  (set-mcp-server-initialized! server #f)
  (make-jsonrpc-response id '()))

;; Convenience function to create a basic MCP server
(define* (create-mcp-server name version #:optional (capabilities *mcp-server-capabilities*))
  "Create a new MCP server with default settings"
  (make-mcp-server name version capabilities '() #f))
