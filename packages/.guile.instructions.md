# Guile Scheme Coding Instructions for Home Lab Tool

## Functional Programming Principles

**Core Philosophy**: Functional programming is about actions, data, and computation - compose small, pure functions to build complex behaviors.

### 1. Pure Functions First
- Functions should be deterministic and side-effect free when possible
- Separate pure computation from I/O operations
- Use immutable data structures as default

```scheme
;; Good: Pure function
(define (calculate-deployment-hash config)
  (sha256 (scm->json-string config)))

;; Better: Separate pure logic from I/O
(define (deployment-ready? machine-config current-state)
  (and (eq? (assoc-ref machine-config 'status) 'configured)
       (eq? (assoc-ref current-state 'connectivity) 'online)))

;; I/O operations separate
(define (check-machine-deployment machine)
  (let ((config (load-machine-config machine))
        (state (probe-machine-state machine)))
    (deployment-ready? config state)))
```

### 2. Data-Driven Design
- Represent configurations and state as data structures
- Use association lists (alists) and vectors for structured data
- Leverage Guile's homoiconicity (code as data)

```scheme
;; Machine configuration as data
(define machine-specs
  `((grey-area 
     (services (ollama jellyfin forgejo))
     (deployment-method deploy-rs)
     (backup-schedule weekly))
    (sleeper-service
     (services (nfs zfs monitoring))
     (deployment-method hybrid-update)
     (backup-schedule daily))))

;; Operations on data
(define (get-machine-services machine)
  (assoc-ref (assoc-ref machine-specs machine) 'services))

(define (machines-with-service service)
  (filter (lambda (machine-spec)
            (member service (get-machine-services (car machine-spec))))
          machine-specs))
```

## Guile-Specific Idioms

### 3. Module Organization
- Use meaningful module hierarchies
- Export only necessary public interfaces
- Group related functionality together

```scheme
;; File: modules/lab/machines.scm
(define-module (lab machines)
  #:use-module (srfi srfi-1)    ; List processing
  #:use-module (srfi srfi-26)   ; Cut/cute
  #:use-module (ice-9 match)    ; Pattern matching
  #:use-module (ssh session)
  #:export (machine-status
            deploy-machine
            list-machines
            machine-services))

;; File: modules/lab/deployment.scm
(define-module (lab deployment)
  #:use-module (lab machines)
  #:use-module (json)
  #:export (deploy-rs
            hybrid-update
            rollback-deployment))
```

### 4. Error Handling the Scheme Way
- Use exceptions for exceptional conditions
- Return #f or special values for expected failures
- Provide meaningful error context

```scheme
;; Use exceptions for programming errors
(define (deploy-machine machine method)
  (unless (member machine (list-machines))
    (throw 'invalid-machine "Unknown machine" machine))
  (unless (member method '(deploy-rs hybrid-update legacy))
    (throw 'invalid-method "Unknown deployment method" method))
  ;; ... deployment logic)

;; Return #f for expected failures
(define (machine-reachable? machine)
  (catch #t
    (lambda ()
      (ssh-connect machine)
      #t)
    (lambda (key . args)
      #f)))

;; Provide context with failure info
(define (deployment-result success? machine method details)
  `((success . ,success?)
    (machine . ,machine)
    (method . ,method)
    (timestamp . ,(current-time))
    (details . ,details)))
```

### 5. Higher-Order Functions and Composition
- Use map, filter, fold for list processing
- Compose functions to build complex operations
- Leverage SRFI-1 for advanced list operations

```scheme
(use-modules (srfi srfi-1))

;; Functional composition
(define (healthy-machines machines)
  (filter machine-reachable?
          (filter (lambda (m) (not (maintenance-mode? m)))
                  machines)))

;; Map operations across machines
(define (update-all-machines)
  (map (lambda (machine)
         (cons machine (update-machine machine)))
       (healthy-machines (list-machines))))

;; Fold for aggregation
(define (deployment-summary results)
  (fold (lambda (result acc)
          (if (assoc-ref result 'success)
              (cons 'successful (1+ (assoc-ref acc 'successful)))
              (cons 'failed (1+ (assoc-ref acc 'failed)))))
        '((successful . 0) (failed . 0))
        results))
```

### 6. Pattern Matching for Control Flow
- Use `match` for destructuring and dispatch
- Pattern match on data structures
- Cleaner than nested if/cond statements

```scheme
(use-modules (ice-9 match))

(define (handle-deployment-event event)
  (match event
    (('start machine method)
     (log-info "Starting deployment of ~a using ~a" machine method))
    
    (('progress machine percent)
     (update-progress-bar machine percent))
    
    (('success machine result)
     (log-success "Deployment completed: ~a" machine)
     (notify-success machine result))
    
    (('error machine error-msg)
     (log-error "Deployment failed: ~a - ~a" machine error-msg)
     (initiate-rollback machine))
    
    (_ (log-warning "Unknown event: ~a" event))))

;; Pattern matching for configuration parsing
(define (parse-machine-config config-sexp)
  (match config-sexp
    (('machine name ('services services ...) ('options options ...))
     `((name . ,name)
       (services . ,services)
       (options . ,(alist->hash-table options))))
    
    (_ (throw 'invalid-config "Malformed machine config" config-sexp))))
```

### 7. REPL-Driven Development
- Design for interactive development
- Provide introspection functions
- Make state queryable and modifiable

```scheme
;; REPL helpers for development
(define (debug-machine-state machine)
  "Display comprehensive machine state for debugging"
  (format #t "Machine: ~a~%" machine)
  (format #t "Status: ~a~%" (machine-status machine))
  (format #t "Services: ~a~%" (machine-services machine))
  (format #t "Last deployment: ~a~%" (last-deployment machine))
  (format #t "Reachable: ~a~%" (machine-reachable? machine)))

;; Interactive deployment with confirmation
(define (interactive-deploy machine)
  (let ((current-config (get-machine-config machine)))
    (display-config current-config)
    (when (yes-or-no? "Proceed with deployment?")
      (deploy-machine machine 'deploy-rs))))

;; State introspection
(define (lab-status)
  `((total-machines . ,(length (list-machines)))
    (reachable . ,(length (filter machine-reachable? (list-machines))))
    (services-running . ,(total-running-services))
    (pending-deployments . ,(length (pending-deployments)))))
```

### 8. Concurrency with Fibers
- Use fibers for concurrent operations
- Non-blocking I/O for better performance
- Coordinate parallel deployments safely

```scheme
(use-modules (fibers) (fibers channels))

;; Concurrent machine checking
(define (check-all-machines-concurrent machines)
  (run-fibers
    (lambda ()
      (let ((results-channel (make-channel)))
        ;; Spawn fiber for each machine
        (for-each (lambda (machine)
                    (spawn-fiber
                      (lambda ()
                        (let ((status (check-machine-status machine)))
                          (put-message results-channel 
                                     (cons machine status))))))
                  machines)
        
        ;; Collect results
        (let loop ((remaining (length machines))
                   (results '()))
          (if (zero? remaining)
              results
              (loop (1- remaining)
                    (cons (get-message results-channel) results))))))))

;; Parallel deployment with coordination
(define (deploy-machines-parallel machines)
  (run-fibers
    (lambda ()
      (let ((deployment-channel (make-channel))
            (coordinator (spawn-fiber (deployment-coordinator deployment-channel))))
        (par-map (lambda (machine)
                   (deploy-with-coordination machine deployment-channel))
                 machines)))))
```

### 9. MCP Server Implementation Patterns
- Structured message handling
- Capability-based tool organization
- Resource management with caching

```scheme
;; MCP message dispatch
(define (handle-mcp-request request)
  (match (json-ref request "method")
    ("tools/list" 
     (mcp-tools-list))
    
    ("tools/call"
     (let ((tool (json-ref request "params" "name"))
           (args (json-ref request "params" "arguments")))
       (call-lab-tool tool args)))
    
    ("resources/list"
     (mcp-resources-list))
    
    ("resources/read"
     (let ((uri (json-ref request "params" "uri")))
       (read-lab-resource uri)))
    
    (method
     (mcp-error -32601 "Method not found" method))))

;; Tool capability definition
(define lab-tools
  `((deploy-machine
     (description . "Deploy configuration to a specific machine")
     (inputSchema . ,(json-schema
                      `((type . "object")
                        (properties . ((machine (type . "string"))
                                     (method (type . "string") 
                                            (enum . ("deploy-rs" "hybrid-update")))))
                        (required . ("machine")))))
     (handler . ,deploy-machine-tool))
    
    (check-status
     (description . "Check machine status and connectivity")
     (inputSchema . ,(json-schema
                      `((type . "object")
                        (properties . ((machines (type . "array")
                                                (items (type . "string"))))))))
     (handler . ,check-status-tool))))
```

### 10. Configuration and Environment
- Use parameters for configuration
- Environment-aware defaults
- Validate configuration on startup

```scheme
;; Configuration parameters
(define lab-config-dir 
  (make-parameter (or (getenv "LAB_CONFIG_DIR") 
                      "/etc/lab-tool")))

(define deployment-timeout
  (make-parameter (string->number (or (getenv "DEPLOYMENT_TIMEOUT") "300"))))

(define ssh-key-path
  (make-parameter (or (getenv "LAB_SSH_KEY")
                      (string-append (getenv "HOME") "/.ssh/lab_key"))))

;; Configuration validation
(define (validate-lab-config)
  (unless (file-exists? (lab-config-dir))
    (throw 'config-error "Lab config directory not found" (lab-config-dir)))
  
  (unless (file-exists? (ssh-key-path))
    (throw 'config-error "SSH key not found" (ssh-key-path)))
  
  (unless (> (deployment-timeout) 0)
    (throw 'config-error "Invalid deployment timeout" (deployment-timeout))))

;; Initialize with validation
(define (init-lab-tool)
  (validate-lab-config)
  (load-machine-configurations)
  (initialize-ssh-agent)
  (setup-logging))
```

## Code Style Guidelines

### 11. Naming Conventions
- Use kebab-case for variables and functions
- Predicates end with `?`
- Mutating procedures end with `!`
- Constants in ALL-CAPS with hyphens

```scheme
;; Good naming
(define DEFAULT-SSH-PORT 22)
(define machine-deployment-status ...)
(define (machine-reachable? machine) ...)
(define (update-machine-config! machine config) ...)

;; Avoid
(define defaultSSHPort 22)           ; camelCase
(define machine_status ...)          ; snake_case
(define (is-machine-reachable ...) ; unnecessary 'is-'
```

### 12. Documentation and Comments
- Document module purposes and exports
- Use docstrings for complex functions
- Comment the "why", not the "what"

```scheme
(define (deploy-machine machine method)
  "Deploy configuration to MACHINE using METHOD.
   
   Returns a deployment result alist with success status, timing,
   and any error messages. May throw exceptions for invalid inputs."
  
  ;; Validate inputs early to fail fast
  (validate-machine machine)
  (validate-deployment-method method)
  
  ;; Use atomic operations to prevent partial deployments
  (call-with-deployment-lock machine
    (lambda ()
      (let ((start-time (current-time)))
        ;; ... deployment logic
        ))))
```

### 13. Testing Approach
- Write tests for pure functions first
- Mock I/O operations
- Use SRFI-64 testing framework

```scheme
(use-modules (srfi srfi-64))

(test-begin "machine-configuration")

(test-equal "machine services extraction"
  '(ollama jellyfin forgejo)
  (get-machine-services 'grey-area))

(test-assert "deployment readiness check"
  (deployment-ready? 
    '((status . configured) (health . good))
    '((connectivity . online) (load . normal))))

(test-error "invalid machine throws exception"
  'invalid-machine
  (deploy-machine 'non-existent-machine 'deploy-rs))

(test-end "machine-configuration")
```

## Project Structure Best Practices

### 14. Module Organization
```
modules/
├── lab/
│   ├── core.scm          ; Core data structures and utilities
│   ├── machines.scm      ; Machine management
│   ├── deployment.scm    ; Deployment strategies
│   ├── monitoring.scm    ; Status checking and metrics
│   └── config.scm        ; Configuration handling
├── mcp/
│   ├── server.scm        ; MCP server implementation
│   ├── tools.scm         ; MCP tool definitions
│   └── resources.scm     ; MCP resource handlers
└── utils/
    ├── ssh.scm           ; SSH utilities
    ├── json.scm          ; JSON helpers
    └── logging.scm       ; Logging facilities
```

### 15. Build and Development Workflow
- Use Guile's module compilation
- Leverage REPL for iterative development
- Provide development/production configurations

```scheme
;; Development helpers in separate module
(define-module (lab dev)
  #:use-module (lab core)
  #:export (reload-config
            reset-state
            dev-deploy))

;; Hot-reload for development
(define (reload-config)
  (reload-module (resolve-module '(lab config)))
  (init-lab-tool))

;; Safe deployment for development
(define (dev-deploy machine)
  (if (eq? (current-environment) 'development)
      (deploy-machine machine 'deploy-rs)
      (error "dev-deploy only available in development mode")))
```

## VS Code and GitHub Copilot Integration

### 16. MCP Client Integration with VS Code
- Implement MCP client in VS Code extension
- Bridge home lab context to Copilot
- Provide real-time infrastructure state

```typescript
// VS Code extension structure for MCP integration
// File: vscode-extension/src/extension.ts
import * as vscode from 'vscode';
import { MCPClient } from './mcp-client';

export function activate(context: vscode.ExtensionContext) {
    const mcpClient = new MCPClient('stdio', {
        command: 'guile',
        args: ['-c', '(use-modules (mcp server)) (run-mcp-server)']
    });

    // Register commands for home lab operations
    const deployCommand = vscode.commands.registerCommand(
        'homelab.deploy', 
        async (machine: string) => {
            const result = await mcpClient.callTool('deploy-machine', {
                machine: machine,
                method: 'deploy-rs'
            });
            vscode.window.showInformationMessage(
                `Deployment ${result.success ? 'succeeded' : 'failed'}`
            );
        }
    );

    // Provide context to Copilot through workspace state
    const statusProvider = new HomeLab StatusProvider(mcpClient);
    context.subscriptions.push(
        vscode.workspace.registerTextDocumentContentProvider(
            'homelab', statusProvider
        )
    );

    context.subscriptions.push(deployCommand);
}

class HomeLabStatusProvider implements vscode.TextDocumentContentProvider {
    constructor(private mcpClient: MCPClient) {}

    async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
        // Fetch current lab state for Copilot context
        const resources = await this.mcpClient.listResources();
        const status = await this.mcpClient.readResource('machines://status/all');
        
        return `# Home Lab Status
Current Infrastructure State:
${JSON.stringify(status, null, 2)}

Available Resources:
${resources.map(r => `- ${r.uri}: ${r.description}`).join('\n')}
`;
    }
}
```

### 17. MCP Server Configuration for IDE Integration
- Provide IDE-specific tools and resources
- Format responses for developer consumption
- Include code suggestions and snippets

```scheme
;; IDE-specific MCP tools
(define ide-tools
  `((generate-nix-config
     (description . "Generate NixOS configuration for new machine")
     (inputSchema . ,(json-schema
                      `((type . "object")
                        (properties . ((machine-name (type . "string"))
                                     (services (type . "array")
                                              (items (type . "string")))
                                     (hardware-profile (type . "string"))))
                        (required . ("machine-name")))))
     (handler . ,generate-nix-config-tool))
    
    (suggest-deployment-strategy
     (description . "Suggest optimal deployment strategy for changes")
     (inputSchema . ,(json-schema
                      `((type . "object")
                        (properties . ((changed-files (type . "array")
                                                     (items (type . "string")))
                                     (target-machines (type . "array")
                                                    (items (type . "string")))))
                        (required . ("changed-files")))))
     (handler . ,suggest-deployment-strategy-tool))
    
    (validate-config
     (description . "Validate NixOS configuration syntax and dependencies")
     (inputSchema . ,(json-schema
                      `((type . "object")
                        (properties . ((config-path (type . "string"))
                                     (machine (type . "string"))))
                        (required . ("config-path")))))
     (handler . ,validate-config-tool))))

;; IDE-specific resources
(define ide-resources
  `(("homelab://templates/machine-config"
     (description . "Template for new machine configuration")
     (mimeType . "application/x-nix"))
    
    ("homelab://examples/service-configs"
     (description . "Example service configurations")
     (mimeType . "application/x-nix"))
    
    ("homelab://docs/deployment-guide"
     (description . "Step-by-step deployment procedures")
     (mimeType . "text/markdown"))
    
    ("homelab://status/real-time"
     (description . "Real-time infrastructure status for context")
     (mimeType . "application/json"))))

;; Generate contextual code suggestions
(define (generate-nix-config-tool args)
  (let ((machine-name (assoc-ref args "machine-name"))
        (services (assoc-ref args "services"))
        (hardware-profile (assoc-ref args "hardware-profile")))
    
    `((content . ,(format #f "# Generated configuration for ~a
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ~/args
  ];

  # Machine-specific configuration
  networking.hostName = \"~a\";
  
  # Services configuration
~a

  # System packages
  environment.systemPackages = with pkgs; [
    # Add your packages here
  ];

  system.stateVersion = \"24.05\";
}"
                        machine-name
                        machine-name
                        (if services
                            (string-join 
                              (map (lambda (service)
                                     (format #f "  services.~a.enable = true;" service))
                                   services)
                              "\n")
                            "  # No services specified")))
      (isError . #f))))
```

### 18. Copilot Context Enhancement
- Provide infrastructure context to improve suggestions
- Include deployment patterns and best practices
- Real-time system state for informed recommendations

```scheme
;; Context provider for Copilot integration
(define (provide-copilot-context)
  `((infrastructure-state . ,(get-current-infrastructure-state))
    (deployment-patterns . ,(get-common-deployment-patterns))
    (service-configurations . ,(get-service-config-templates))
    (best-practices . ,(get-deployment-best-practices))
    (current-issues . ,(get-active-alerts))))

(define (get-current-infrastructure-state)
  `((machines . ,(map (lambda (machine)
                        `((name . ,machine)
                          (status . ,(machine-status machine))
                          (services . ,(machine-services machine))
                          (last-deployment . ,(last-deployment-time machine))))
                      (list-machines)))
    (network-topology . ,(get-network-topology))
    (resource-usage . ,(get-resource-utilization))))

(define (get-common-deployment-patterns)
  `((safe-deployment . "Use deploy-rs for production, hybrid-update for development")
    (rollback-strategy . "Always test deployments in staging first")
    (service-dependencies . "Ensure database services start before applications")
    (backup-before-deploy . "Create snapshots before major configuration changes")))

;; Format context for IDE consumption
(define (format-ide-context context)
  (scm->json-string context #:pretty #t))
```

### 19. VS Code Extension Development
- Create extension for seamless MCP integration
- Provide commands, views, and context
- Enable real-time collaboration with infrastructure

```typescript
// package.json for VS Code extension
{
  "name": "homelab-mcp-integration",
  "displayName": "Home Lab MCP Integration",
  "description": "Integrate home lab infrastructure with VS Code through MCP",
  "version": "0.1.0",
  "engines": {
    "vscode": "^1.74.0"
  },
  "categories": ["Other"],
  "activationEvents": [
    "onCommand:homelab.connect",
    "workspaceContains:**/flake.nix"
  ],
  "main": "./out/extension.js",
  "contributes": {
    "commands": [
      {
        "command": "homelab.deploy",
        "title": "Deploy Machine",
        "category": "Home Lab"
      },
      {
        "command": "homelab.status",
        "title": "Check Status",
        "category": "Home Lab"
      },
      {
        "command": "homelab.generateConfig",
        "title": "Generate Config",
        "category": "Home Lab"
      }
    ],
    "views": {
      "explorer": [
        {
          "id": "homelabStatus",
          "name": "Home Lab Status",
          "when": "homelab:connected"
        }
      ]
    },
    "viewsContainers": {
      "activitybar": [
        {
          "id": "homelab",
          "title": "Home Lab",
          "icon": "$(server-environment)"
        }
      ]
    }
  }
}

// MCP Client implementation
class MCPClient {
    private transport: MCPTransport;
    private capabilities: MCPCapabilities;

    constructor(transportType: 'stdio' | 'websocket', config: any) {
        this.transport = this.createTransport(transportType, config);
        this.initialize();
    }

    async callTool(name: string, arguments: any): Promise<any> {
        return this.transport.request('tools/call', {
            name: name,
            arguments: arguments
        });
    }

    async listResources(): Promise<MCPResource[]> {
        const response = await this.transport.request('resources/list', {});
        return response.resources;
    }

    async readResource(uri: string): Promise<any> {
        return this.transport.request('resources/read', { uri });
    }

    // Integration with Copilot context
    async getCopilotContext(): Promise<string> {
        const context = await this.readResource('homelab://context/copilot');
        return context.content;
    }
}
```

### 20. GitHub Copilot Workspace Integration
- Configure workspace for optimal Copilot suggestions
- Provide infrastructure context files
- Set up context patterns for deployment scenarios

```json
// .vscode/settings.json
{
  "github.copilot.enable": {
    "*": true,
    "yaml": true,
    "nix": true,
    "scheme": true
  },
  "github.copilot.advanced": {
    "length": 500,
    "temperature": 0.2
  },
  "homelab.mcpServer": {
    "command": "guile",
    "args": ["-L", "modules", "-c", "(use-modules (mcp server)) (run-mcp-server)"],
    "autoStart": true
  },
  "files.associations": {
    "*.scm": "scheme",
    "flake.lock": "json"
  }
}

// .copilot/context.md for workspace context
```markdown
# Home Lab Infrastructure Context

## Current Architecture
- NixOS-based infrastructure with multiple machines
- Deploy-rs for safe deployments
- Services: Ollama, Jellyfin, Forgejo, NFS, ZFS
- Network topology: reverse-proxy, grey-area, sleeper-service, congenital-optimist

## Common Patterns
- Use `deploy-rs` for production deployments
- Test with `hybrid-update` in development
- Always backup before major changes
- Follow NixOS module structure in `/modules/`

## Configuration Standards
- Machine configs in `/machines/{hostname}/`
- Shared modules in `/modules/`
- Service-specific configs in `services/` subdirectories
```

### 21. Real-time Context Updates
- Stream infrastructure changes to VS Code
- Update Copilot context automatically
- Provide deployment feedback in editor

```scheme
;; Real-time context streaming
(define (start-context-stream port)
  "Stream infrastructure changes to connected IDE clients"
  (let ((clients (make-hash-table)))
    (spawn-fiber
      (lambda ()
        (let loop ()
          (let ((update (get-infrastructure-update)))
            (hash-for-each 
              (lambda (client-id websocket)
                (catch #t
                  (lambda ()
                    (websocket-send websocket 
                                  (scm->json-string update)))
                  (lambda (key . args)
                    (hash-remove! clients client-id))))
              clients)
            (sleep 5)
            (loop)))))
    
    ;; WebSocket server for IDE connections
    (run-websocket-server
      (lambda (ws)
        (let ((client-id (generate-client-id)))
          (hash-set! clients client-id ws)
          (websocket-send ws 
                        (scm->json-string 
                          `((type . "welcome")
                            (context . ,(get-current-context)))))
          (handle-client-messages ws client-id clients)))
      #:port port)))

;; Integration with file watchers
(define (watch-config-changes)
  "Watch for configuration file changes and update context"
  (file-system-watcher 
    (list "/home/geir/Home-lab/machines"
          "/home/geir/Home-lab/modules")
    (lambda (event)
      (match event
        (('modify path)
         (when (string-suffix? ".nix" path)
           (update-copilot-context path)))
        (_ #f)))))
```