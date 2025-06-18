;; Unit Tests for MCP Protocol Module
;; Tests the core MCP protocol implementation

(define-module (tests protocol-tests)
  #:use-module (srfi srfi-64)
  #:use-module (mcp server protocol)
  #:use-module (mcp server jsonrpc)
  #:export (run-protocol-tests))

(define (run-protocol-tests)
  "Run all MCP Protocol module tests"
  (test-begin "MCP Protocol Tests")
  
  ;; Test MCP server creation
  (test-group "Server Creation"
    (test-mcp-server-creation))
  
  ;; Test initialization handshake
  (test-group "Initialization"
    (test-mcp-initialization))
  
  ;; Test capability negotiation
  (test-group "Capabilities"
    (test-capability-negotiation))
  
  ;; Test handler registration
  (test-group "Handler Registration"
    (test-handler-registration))
  
  ;; Test message handling
  (test-group "Message Handling"
    (test-message-handling))
  
  ;; Test MCP lifecycle
  (test-group "Lifecycle"
    (test-mcp-lifecycle))
  
  (test-end "MCP Protocol Tests"))

(define (test-mcp-server-creation)
  "Test MCP server creation and configuration"
  
  (test-assert "Create MCP server"
    (let ((server (create-mcp-server "test-server" "1.0.0")))
      (and (mcp-server? server)
           (equal? (mcp-server-name server) "test-server")
           (equal? (mcp-server-version server) "1.0.0"))))
  
  ;; Test server with custom capabilities
  (test-assert "Create server with custom capabilities"
    (let* ((custom-caps '((tools . #t) (custom . #t)))
           (server (create-mcp-server "test-server" "1.0.0" custom-caps)))
      (and (mcp-server? server)
           (equal? (mcp-server-capabilities server) custom-caps))))
  
  ;; Test server initial state
  (test-assert "Server starts with empty handlers"
    (let ((server (create-mcp-server "test-server" "1.0.0")))
      (null? (mcp-server-handlers server)))))

(define (test-mcp-initialization)
  "Test MCP initialization process"
  
  (test-assert "Handle initialize request"
    (let* ((server (create-mcp-server "test-server" "1.0.0"))
           (params `(("protocolVersion" . ,*mcp-protocol-version*)
                    ("capabilities" . (("tools" . #t)))
                    ("clientInfo" . (("name" . "test-client")
                                    ("version" . "1.0.0")))))
           (request (make-jsonrpc-request 1 "initialize" params))
           (response (handle-mcp-message server request)))
      (jsonrpc-response? response))))

(define (test-capability-negotiation)
  "Test capability negotiation"
  
  (test-assert "Server has default capabilities"
    (let ((server (create-mcp-server "test-server" "1.0.0")))
      (list? (mcp-server-capabilities server))))
  
  ;; Test specific capabilities
  (test-assert "Server supports tools capability"
    (let ((server (create-mcp-server "test-server" "1.0.0")))
      (assoc-ref (mcp-server-capabilities server) 'tools)))
  
  ;; Test capability modification
  (test-assert "Can create server with modified capabilities"
    (let* ((custom-caps '((tools . #f) (resources . #t)))
           (server (create-mcp-server "test-server" "1.0.0" custom-caps)))
      (and (not (assoc-ref (mcp-server-capabilities server) 'tools))
           (assoc-ref (mcp-server-capabilities server) 'resources))))))

(define (test-handler-registration)
  "Test handler registration and management"
  
  (test-assert "Register custom handler"
    (let ((server (create-mcp-server "test-server" "1.0.0")))
      (register-mcp-handler server "test-method" 
                           (lambda (srv params) "test-result"))
      (assoc-ref (mcp-server-handlers server) "test-method")))
  
  ;; Test multiple handlers
  (test-assert "Register multiple handlers"
    (let ((server (create-mcp-server "test-server" "1.0.0")))
      (register-mcp-handler server "method1" (lambda (srv p) "result1"))
      (register-mcp-handler server "method2" (lambda (srv p) "result2"))
      (and (assoc-ref (mcp-server-handlers server) "method1")
           (assoc-ref (mcp-server-handlers server) "method2"))))
  
  ;; Test handler replacement
  (test-assert "Replace existing handler"
    (let ((server (create-mcp-server "test-server" "1.0.0")))
      (register-mcp-handler server "test-method" (lambda (srv p) "old"))
      (register-mcp-handler server "test-method" (lambda (srv p) "new"))
      (let ((handler (assoc-ref (mcp-server-handlers server) "test-method")))
        (equal? (handler server '()) "new")))))

(define (test-message-handling)
  "Test MCP message handling functionality"
  
  ;; Test request handling
  (test-assert "Handle JSON-RPC request"
    (let* ((server (create-mcp-server "test-server" "1.0.0"))
           (request (make-jsonrpc-request 1 "initialize" 
                                        `(("protocolVersion" . ,*mcp-protocol-version*)
                                         ("capabilities" . (("tools" . #t)))
                                         ("clientInfo" . (("name" . "test-client")
                                                         ("version" . "1.0.0"))))))
           (response (handle-mcp-message server request)))
      (jsonrpc-response? response)))
  
  ;; Test notification handling
  (test-assert "Handle JSON-RPC notification"
    (let* ((server (create-mcp-server "test-server" "1.0.0"))
           (notification (make-jsonrpc-notification "initialized" '()))
           (result (handle-mcp-message server notification)))
      ;; Notifications should return #t for success or an error
      (or (eq? result #t) (jsonrpc-error? result))))
  
  ;; Test unknown method
  (test-assert "Handle unknown method"
    (let* ((server (create-mcp-server "test-server" "1.0.0"))
           (request (make-jsonrpc-request 1 "unknown-method" '()))
           (response (handle-mcp-message server request)))
      (jsonrpc-error? response)))
  
  ;; Test custom handler
  (test-assert "Call custom handler"
    (let* ((server (create-mcp-server "test-server" "1.0.0"))
           (handler (lambda (srv params) "custom-result")))
      (register-mcp-handler server "custom-method" handler)
      (let* ((request (make-jsonrpc-request 1 "custom-method" '()))
             (response (handle-mcp-message server request)))
        (and (jsonrpc-response? response)
             (equal? (jsonrpc-response-result response) "custom-result"))))))

(define (test-mcp-lifecycle)
  "Test MCP server lifecycle management"
  
  ;; Test initialization state
  (test-assert "Server starts uninitialized"
    (let ((server (create-mcp-server "test-server" "1.0.0")))
      (not (mcp-server-initialized? server))))
  
  ;; Test initialization process
  (test-assert "Initialize server properly"
    (let* ((server (create-mcp-server "test-server" "1.0.0"))
           (params `(("protocolVersion" . ,*mcp-protocol-version*)
                    ("capabilities" . (("tools" . #t)))
                    ("clientInfo" . (("name" . "test-client")
                                    ("version" . "1.0.0")))))
           (response (mcp-initialize server 1 params)))
      (and (jsonrpc-response? response)
           (mcp-server-initialized? server))))
  
  ;; Test shutdown
  (test-assert "Handle shutdown request"
    (let* ((server (create-mcp-server "test-server" "1.0.0"))
           (response (mcp-shutdown server 1)))
      (and (jsonrpc-response? response)
           (not (mcp-server-initialized? server)))))
  
  ;; Test protocol version
  (test-assert "Protocol version is correct"
    (string=? *mcp-protocol-version* "2024-11-05"))
  
  ;; Test default capabilities
  (test-assert "Default capabilities include required items"
    (let ((caps *mcp-server-capabilities*))
      (and (assoc-ref caps 'tools)
           (assoc-ref caps 'resources)
           (assoc-ref caps 'prompts)))))
