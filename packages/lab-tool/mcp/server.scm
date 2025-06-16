;; mcp/server.scm - MCP server stub (impure)

(define-module (mcp server)
  #:use-module (utils logging)
  #:export (start-mcp-server))

;; Impure function: Start MCP server
(define (start-mcp-server)
  "Start MCP server (impure - has side effects)"
  (log-info "Starting MCP server...")
  (log-warn "MCP server not yet implemented")
  #f)
