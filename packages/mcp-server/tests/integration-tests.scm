;; Integration Tests for MCP Server
;; Tests the complete server functionality and component interactions

(define-module (tests integration-tests)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 receive)
  #:use-module (mcp server integration)
  #:use-module (mcp server protocol)
  #:use-module (mcp server transport)
  #:use-module (mcp server router)
  #:use-module (mcp server error-handling)
  #:use-module (mcp server jsonrpc)
  #:export (run-server-integration-tests))

(define (run-server-integration-tests)
  "Run all server integration tests"
  (test-begin "Server Integration Tests")
  
  ;; Test full server setup
  (test-group "Server Setup"
    (test-server-setup))
  
  ;; Test end-to-end communication
  (test-group "End-to-End Communication"
    (test-e2e-communication))
  
  ;; Test lab tool integration
  (test-group "Lab Tool Integration"
    (test-lab-tool-integration))
  
  (test-end "Server Integration Tests"))

(define (test-server-setup)
  "Test complete server setup and configuration"
  
  (test-assert "Setup integrated MCP server"
    (receive (server transport router error-handler)
        (setup-mcp-server #:transport-type 'stdio)
      (and (mcp-server? server)
           (transport? transport)
           (router? router)
           (error-handler? error-handler)))))

(define (test-e2e-communication)
  "Test end-to-end communication flow"
  
  ;; Test initialization handshake
  (test-assert "Complete initialization handshake"
    (let ((server (create-integrated-mcp-server)))
      (let* ((init-params `(("protocolVersion" . "2024-11-05")
                           ("capabilities" . (("tools" . #t)))
                           ("clientInfo" . (("name" . "test-client")
                                          ("version" . "1.0.0")))))
             (init-request (make-jsonrpc-request 1 "initialize" init-params))
             (init-response (handle-mcp-message server init-request)))
        (jsonrpc-response? init-response))))
  
  ;; Test tool calls
  (test-assert "Handle tool call requests"
    (let ((server (create-integrated-mcp-server)))
      ;; First initialize
      (let* ((init-params `(("protocolVersion" . "2024-11-05")
                           ("capabilities" . (("tools" . #t)))
                           ("clientInfo" . (("name" . "test-client")
                                          ("version" . "1.0.0")))))
             (init-request (make-jsonrpc-request 1 "initialize" init-params))
             (init-response (handle-mcp-message server init-request)))
        
        ;; Send initialized notification
        (let ((init-notif (make-jsonrpc-notification "initialized" #f)))
          (handle-mcp-message server init-notif)
          
          ;; Now test a tool call
          (let* ((tool-params `(("machine" . "test-machine")))
                 (tool-request (make-jsonrpc-request 2 "tools/machine/status" tool-params))
                 (tool-response (handle-mcp-message server tool-request)))
            (or (jsonrpc-response? tool-response)
                (jsonrpc-error? tool-response))))))))

(define (test-lab-tool-integration)
  "Test home lab tool integration"
  
  ;; Test machine management tools
  (test-assert "Machine management tools available"
    (let ((server (create-integrated-mcp-server)))
      ;; Check if machine management handlers are registered
      (assoc-ref (mcp-server-handlers server) "tools/machine/list")))
  
  ;; Test service management tools
  (test-assert "Service management tools available"
    (let ((server (create-integrated-mcp-server)))
      ;; Check if service management handlers are registered
      (assoc-ref (mcp-server-handlers server) "tools/service/status")))
  
  ;; Test configuration access
  (test-assert "Configuration access available"
    (let ((server (create-integrated-mcp-server)))
      ;; Check if configuration handlers are registered
      (assoc-ref (mcp-server-handlers server) "resources/config/nixos"))))
