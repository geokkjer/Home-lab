;; MCP Transport Layer Implementation
;; This module implements the transport layer for MCP communication
;; supporting stdio, HTTP, and WebSocket protocols.

(define-module (mcp server transport)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (mcp server jsonrpc)
  #:use-module (mcp server protocol)
  #:export (make-transport
            transport?
            transport-type
            transport-active?
            start-transport
            stop-transport
            send-message
            receive-message
            stdio-transport
            http-transport
            websocket-transport
            run-mcp-server))

;; Transport record type
(define-record-type <transport>
  (make-transport type active? send-proc receive-proc start-proc stop-proc)
  transport?
  (type transport-type)
  (active? transport-active? set-transport-active!)
  (send-proc transport-send-proc)
  (receive-proc transport-receive-proc)
  (start-proc transport-start-proc)
  (stop-proc transport-stop-proc))

;; Transport operations
(define (start-transport transport)
  "Start the transport"
  ((transport-start-proc transport) transport))

(define (stop-transport transport)
  "Stop the transport"
  ((transport-stop-proc transport) transport))

(define (send-message transport message)
  "Send a message through the transport"
  ((transport-send-proc transport) message))

(define (receive-message transport)
  "Receive a message from the transport"
  ((transport-receive-proc transport)))

;; Stdio Transport Implementation
(define (stdio-send-message message)
  "Send a message via stdio"
  (let ((json-str (jsonrpc-message->json message)))
    (format #t "~a~%" json-str)
    (force-output)))

(define (stdio-receive-message)
  "Receive a message via stdio"
  (let ((line (read-line)))
    (if (eof-object? line)
        #f
        (parse-jsonrpc-message line))))

(define (stdio-start transport)
  "Start stdio transport"
  (set-transport-active! transport #t)
  #t)

(define (stdio-stop transport)
  "Stop stdio transport"
  (set-transport-active! transport #f)
  #t)

(define (stdio-transport)
  "Create a stdio transport"
  (make-transport 'stdio #f
                  stdio-send-message
                  stdio-receive-message
                  stdio-start
                  stdio-stop))

;; HTTP Transport Implementation
(define (http-send-message message)
  "Send a message via HTTP (for responses)"
  ;; HTTP responses are handled by the request handler
  (jsonrpc-message->json message))

(define (http-receive-message request)
  "Receive a message via HTTP request"
  (let ((body (utf8->string (request-body request))))
    (if (string-null? body)
        #f
        (parse-jsonrpc-message body))))

(define (http-handler server)
  "Create HTTP handler for MCP server"
  (lambda (request request-body)
    (match (request-method request)
      ('POST
       (let* ((message (http-receive-message request))
              (response-message (if message
                                    (handle-mcp-message server message)
                                    (make-jsonrpc-error #f
                                                        (assoc-ref *jsonrpc-error-codes* 'parse-error)
                                                        "Invalid request body"
                                                        #f)))
              (response-json (http-send-message response-message)))
         (values (build-response #:code 200
                                 #:headers '((content-type . (application/json))))
                 response-json)))
      (_
       (values (build-response #:code 405
                               #:headers '((content-type . (text/plain))))
               "Method Not Allowed")))))

(define (http-start transport server port)
  "Start HTTP transport"
  (set-transport-active! transport #t)
  (run-server (http-handler server) 'http `(#:port ,port))
  #t)

(define (http-stop transport)
  "Stop HTTP transport"
  (set-transport-active! transport #f)
  ;; Note: Stopping the HTTP server requires more complex lifecycle management
  #t)

(define (http-transport port)
  "Create an HTTP transport"
  (make-transport 'http #f
                  http-send-message
                  (lambda () #f) ; HTTP is request-response, not continuous receive
                  (lambda (transport) (http-start transport #f port))
                  http-stop))

;; WebSocket Transport Implementation (Basic stub)
;; Note: Full WebSocket implementation would require additional dependencies
(define (websocket-send-message message)
  "Send a message via WebSocket"
  ;; Placeholder for WebSocket implementation
  (format (current-error-port) "WebSocket send not implemented: ~a~%" message))

(define (websocket-receive-message)
  "Receive a message via WebSocket"
  ;; Placeholder for WebSocket implementation
  #f)

(define (websocket-start transport)
  "Start WebSocket transport"
  (format (current-error-port) "WebSocket transport not fully implemented~%")
  (set-transport-active! transport #f)
  #f)

(define (websocket-stop transport)
  "Stop WebSocket transport"
  (set-transport-active! transport #f)
  #t)

(define (websocket-transport port)
  "Create a WebSocket transport (placeholder)"
  (make-transport 'websocket #f
                  websocket-send-message
                  websocket-receive-message
                  websocket-start
                  websocket-stop))

;; Main server runner
(define (run-mcp-server server transport)
  "Run the MCP server with the specified transport"
  (start-transport transport)
  
  (cond
   ;; Stdio transport - event loop
   ((eq? (transport-type transport) 'stdio)
    (let loop ()
      (when (transport-active? transport)
        (let ((message (receive-message transport)))
          (when message
            (let ((response (handle-mcp-message server message)))
              (when (and response (not (jsonrpc-notification? message)))
                (send-message transport response)))))
        (loop))))
   
   ;; HTTP transport - handled by web server
   ((eq? (transport-type transport) 'http)
    (format (current-error-port) "HTTP server started~%")
    ;; The HTTP server runs in its own event loop
    #t)
   
   ;; WebSocket transport - placeholder
   ((eq? (transport-type transport) 'websocket)
    (format (current-error-port) "WebSocket transport not implemented~%")
    #f)
   
   (else
    (format (current-error-port) "Unknown transport type: ~a~%" (transport-type transport))
    #f))
  
  (stop-transport transport))
