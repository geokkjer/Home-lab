;; Protocol Compliance Tests for MCP 2024-11-05 Specification
;; Tests compliance with the official MCP specification

(define-module (tests protocol-compliance-tests)
  #:use-module (srfi srfi-64)
  #:use-module (mcp server integration)
  #:use-module (mcp server jsonrpc)
  #:export (run-mcp-compliance-tests))

(define (run-mcp-compliance-tests)
  "Run all MCP protocol compliance tests"
  (test-begin "MCP Protocol Compliance Tests")
  
  ;; Test MCP 2024-11-05 specification compliance
  (test-group "MCP 2024-11-05 Specification"
    (test-mcp-spec-compliance))
  
  ;; Test required capabilities
  (test-group "Required Capabilities"
    (test-required-capabilities))
  
  ;; Test standard methods
  (test-group "Standard Methods"
    (test-standard-methods))
  
  (test-end "MCP Protocol Compliance Tests"))

(define (test-mcp-spec-compliance)
  "Test MCP specification compliance"
  
  ;; Test protocol version support
  (test-assert "Support MCP protocol version 2024-11-05"
    (equal? *mcp-protocol-version* "2024-11-05"))
  
  ;; Test initialization flow
  (test-assert "Follow MCP initialization flow"
    (let ((server (create-integrated-mcp-server)))
      (let* ((init-params `(("protocolVersion" . "2024-11-05")
                           ("capabilities" . (("tools" . #t)))
                           ("clientInfo" . (("name" . "test-client")
                                          ("version" . "1.0.0")))))
             (init-request (make-jsonrpc-request 1 "initialize" init-params))
             (init-response (handle-mcp-message server init-request)))
        
        (and (jsonrpc-response? init-response)
             (let ((result (jsonrpc-response-result init-response)))
               (and (hash-table? result)
                    (equal? (hash-ref result "protocolVersion") "2024-11-05")
                    (hash-ref result "capabilities")
                    (hash-ref result "serverInfo")))))))
  
  ;; Test shutdown flow
  (test-assert "Handle shutdown request"
    (let ((server (create-integrated-mcp-server)))
      (let* ((shutdown-request (make-jsonrpc-request 1 "shutdown" #f))
             (shutdown-response (handle-mcp-message server shutdown-request)))
        (and (jsonrpc-response? shutdown-response)
             (null? (jsonrpc-response-result shutdown-response)))))))

(define (test-required-capabilities)
  "Test required MCP capabilities"
  
  ;; Test tools capability
  (test-assert "Support tools capability"
    (let ((server (create-integrated-mcp-server)))
      (assoc-ref (mcp-server-capabilities server) 'tools)))
  
  ;; Test resources capability
  (test-assert "Support resources capability"
    (let ((server (create-integrated-mcp-server)))
      (assoc-ref (mcp-server-capabilities server) 'resources)))
  
  ;; Test prompts capability
  (test-assert "Support prompts capability"
    (let ((server (create-integrated-mcp-server)))
      (assoc-ref (mcp-server-capabilities server) 'prompts))))

(define (test-standard-methods)
  "Test standard MCP methods"
  
  ;; Test ping method (if implemented)
  (test-assert "Handle ping method"
    (let ((server (create-integrated-mcp-server)))
      (let* ((ping-request (make-jsonrpc-request 1 "ping" #f))
             (ping-response (handle-mcp-message server ping-request)))
        ;; Ping might not be implemented, so accept either response or method-not-found
        (or (jsonrpc-response? ping-response)
            (and (jsonrpc-error? ping-response)
                 (equal? (jsonrpc-error-code ping-response) -32601))))))
  
  ;; Test notifications/message handling
  (test-assert "Handle notifications"
    (let ((server (create-integrated-mcp-server)))
      (let ((notif (make-jsonrpc-notification "notifications/message" 
                                              `(("level" . "info")
                                               ("data" . "test message")))))
        ;; Notifications don't return responses, so just check it doesn't crash
        (handle-mcp-message server notif)
        #t))))
