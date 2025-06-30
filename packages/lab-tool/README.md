# Lab Package - Home Lab Infrastructure Management in Guile Scheme

A comprehensive home lab management tool implemented in GNU Guile Scheme, providing infrastructure monitoring, deployment automation, and health checking capabilities for NixOS-based systems.

## Table of Contents

- [Overview](#overview)
- [Background on GNU Guile](#background-on-gnu-guile)
- [Code Architecture](#code-architecture)
- [Core Modules](#core-modules)
- [Installation & Setup](#installation--setup)
- [Usage Examples](#usage-examples)
- [API Reference](#api-reference)
- [Development](#development)
- [Integration](#integration)

## Overview

The Lab package is a sophisticated infrastructure management tool designed for home lab environments running NixOS. It provides:

- **Infrastructure Status Monitoring**: Real-time status checking across multiple machines
- **Deployment Automation**: Safe NixOS configuration deployment with multiple strategies
- **Health Checking**: Comprehensive system health validation
- **SSH-based Operations**: Secure remote command execution and file operations
- **Error Handling**: Robust error reporting and recovery mechanisms
- **MCP Integration**: Model Context Protocol server for IDE integration

## Background on GNU Guile

### What is GNU Guile?

GNU Guile is the official extension language for the GNU Project and is an implementation of the Scheme programming language. Scheme is a minimalist dialect of Lisp, designed for clarity and elegance.

### Why Guile for Infrastructure Management?

**Advantages over traditional shell scripting:**

1. **Rich Data Structures**: Native support for complex data manipulation with lists, association lists, and records
2. **Error Handling**: Sophisticated condition system for graceful error recovery
3. **Modularity**: Built-in module system for organizing large codebases
4. **Functional Programming**: Immutable data structures and pure functions reduce bugs
5. **REPL-Driven Development**: Interactive development and debugging capabilities
6. **Extensibility**: Easy integration with C libraries and external tools

**Scheme Language Features:**

- **S-expressions**: Code as data, enabling powerful metaprogramming
- **First-class functions**: Functions can be passed as arguments and returned as values
- **Pattern matching**: Elegant control flow with the `match` macro
- **Tail call optimization**: Efficient recursive algorithms
- **Hygienic macros**: Safe code generation and transformation

**Example of Scheme expressiveness:**

```scheme
;; Traditional imperative approach (pseudo-code)
machines = get_machines()
results = []
for machine in machines:
    status = check_machine(machine)
    results.append((machine, status))

;; Functional Scheme approach
(map (lambda (machine)
       `(,machine . ,(check-machine machine)))
     (get-machines))
```

## Code Architecture

The lab package follows a modular architecture with clear separation of concerns:

```
packages/lab/
├── core.scm              # Core infrastructure operations
├── deployment.scm        # Deployment strategies and execution
├── machines.scm          # Machine management and discovery
├── monitoring.scm        # Health checking and status monitoring
└── utils/
    ├── config.scm        # Configuration management
    ├── logging.scm       # Structured logging system
    └── ssh.scm           # SSH operations and connectivity
```

### Design Principles

1. **Functional Core, Imperative Shell**: Pure functions for business logic, side effects at boundaries
2. **Data-Driven Design**: Configuration and machine definitions as data structures
3. **Composable Operations**: Small, focused functions that can be combined
4. **Error Boundaries**: Comprehensive error handling with informative messages
5. **Testable Components**: Functions designed for easy unit testing

## Core Modules

### core.scm - Main Infrastructure Operations

The core module provides the primary interface for infrastructure management:

```scheme
(define-module (lab core)
  #:export (get-infrastructure-status
            check-system-health
            update-flake
            validate-environment
            execute-nixos-rebuild))
```

#### Key Functions

**`get-infrastructure-status`**

- Retrieves comprehensive status of all machines or a specific machine
- Returns structured data including connectivity, services, and system metrics
- Supports both local and remote machine checking

**`check-system-health`**

- Performs comprehensive health checks including:
  - SSH connectivity testing
  - Disk space validation (< 90% usage)
  - System load monitoring (< 5.0 load average)
  - Critical service status (sshd, etc.)
  - Network connectivity verification

**`execute-nixos-rebuild`**

- Safe NixOS deployment with comprehensive error handling
- Supports multiple deployment modes (switch, boot, test)
- Handles both local and remote deployments
- Includes dry-run capabilities for testing

#### Code Analysis Example

```scheme
;; Infrastructure status checking with error handling
(define (get-infrastructure-status . args)
  "Get status of all machines or specific machine if provided"
  (let ((target-machine (if (null? args) #f (car args)))
        (machines (if (null? args) 
                     (get-all-machines) 
                     (list (car args)))))
    
    (log-info "Checking infrastructure status...")
    
    (map (lambda (machine-name)
           (let ((start-time (current-time)))
             (log-debug "Checking ~a..." machine-name)
             
             ;; Gather machine information with error isolation
             (let* ((ssh-config (get-ssh-config machine-name))
                    (is-local (and ssh-config (assoc-ref ssh-config 'is-local)))
                    (connection-status (test-ssh-connection machine-name))
                    (services-status (if connection-status
                                       (get-machine-services-status machine-name)
                                       '()))
                    (system-info (if connection-status
                                   (get-machine-system-info machine-name)
                                   #f))
                    (elapsed (- (current-time) start-time)))
               
               ;; Return structured status data
               `((machine . ,machine-name)
                 (type . ,(if is-local 'local 'remote))
                 (connection . ,(if connection-status 'online 'offline))
                 (services . ,services-status)
                 (system . ,system-info)
                 (check-time . ,elapsed)))))
         machines)))
```

This code demonstrates several Scheme/Guile idioms:

1. **Optional Arguments**: Using `(args)` with `null?` check for flexible function signatures
2. **Let Bindings**: Structured variable binding with `let*` for dependent calculations
3. **Conditional Expressions**: Using `if` expressions for control flow
4. **Association Lists**: Structured data representation with `assoc-ref`
5. **Quasiquote/Unquote**: Building structured data with `` `((key . ,value))` syntax

### utils/ssh.scm - SSH Operations

Handles all SSH-related operations with comprehensive error handling:

```scheme
(define (test-ssh-connection machine-name)
  (let ((ssh-config (get-ssh-config machine-name)))
    (if (not ssh-config)
        (begin
          (log-error "No SSH configuration found for ~a" machine-name)
          #f)
        (if (assoc-ref ssh-config 'is-local)
            (begin
              (log-debug "Machine ~a is local, skipping SSH test" machine-name)
              #t)
            ;; Remote SSH testing logic
            (let ((hostname (assoc-ref ssh-config 'hostname))
                  (ssh-alias (assoc-ref ssh-config 'ssh-alias)))
              (catch #t
                (lambda ()
                  (let* ((test-cmd (if ssh-alias
                                      (format #f "ssh -o ConnectTimeout=5 -o BatchMode=yes ~a echo OK" ssh-alias)
                                      (format #f "ssh -o ConnectTimeout=5 -o BatchMode=yes ~a echo OK" hostname)))
                         (port (open-pipe* OPEN_READ "/bin/sh" "-c" test-cmd))
                         (output (get-string-all port))
                         (status (close-pipe port)))
                    (zero? status)))
                (lambda (key . args)
                  (log-warn "SSH connection test failed for ~a: ~a" machine-name key)
                  #f)))))))
```

### utils/config.scm - Configuration Management

Provides structured configuration handling:

```scheme
(define default-config
  `((homelab-root . "/home/geir/Home-lab")
    (machines . ((congenital-optimist 
                  (type . local)
                  (hostname . "localhost")
                  (services . (workstation development)))
                 (sleeper-service
                  (type . remote)
                  (hostname . "sleeper-service.tail807ea.ts.net")
                  (ssh-alias . "admin-sleeper")
                  (services . (nfs zfs storage)))
                 (grey-area
                  (type . remote)
                  (hostname . "grey-area.tail807ea.ts.net")
                  (ssh-alias . "admin-grey")
                  (services . (ollama forgejo git)))
                 (reverse-proxy
                  (type . remote)
                  (hostname . "reverse-proxy.tail807ea.ts.net")
                  (ssh-alias . "admin-reverse")
                  (services . (nginx proxy ssl)))
                 (little-rascal
                  (type . remote)
                  (hostname . "little-rascal.tail807ea.ts.net")
                  (ssh-alias . "little-rascal")
                  (services . (development niri desktop ai-tools)))
                 ;; Additional machines...
                 ))))
```

### utils/logging.scm - Structured Logging

Comprehensive logging system with color coding and log levels:

```scheme
;; ANSI color codes for terminal output
(define color-codes
  '((reset . "\x1b[0m")
    (bold . "\x1b[1m")
    (red . "\x1b[31m")
    (green . "\x1b[32m")
    (yellow . "\x1b[33m")
    (blue . "\x1b[34m")
    (magenta . "\x1b[35m")
    (cyan . "\x1b[36m")))

;; Log level hierarchy
(define log-levels
  '((debug . 0)
    (info . 1)
    (warn . 2)
    (error . 3)))
```

## Installation & Setup

### Prerequisites

1. **GNU Guile**: Version 3.0 or later
2. **NixOS System**: Target machines running NixOS
3. **SSH Access**: Configured SSH keys for remote machines
4. **Required Guile Libraries**:
   - `guile-ssh` - SSH operations
   - `guile-json` - JSON data handling
   - `srfi-1`, `srfi-19` - Standard Scheme libraries

### NixOS Integration

Add to your NixOS configuration:

```nix
environment.systemPackages = with pkgs; [
  guile
  guile-ssh
  guile-json-4
];
```

### Environment Setup

```bash
# Set environment variables
export HOMELAB_ROOT="/path/to/your/home-lab"
export GUILE_LOAD_PATH="/path/to/packages:$GUILE_LOAD_PATH"

# Test installation
guile -c "(use-modules (lab core)) (display \"Lab package loaded successfully\\n\")"
```

## Usage Examples

### Basic Infrastructure Status

```scheme
#!/usr/bin/env guile
!#

(use-modules (lab core))

;; Check all machines
(define all-status (get-infrastructure-status))
(display all-status)

;; Check specific machine
(define machine-status (get-infrastructure-status "sleeper-service"))
(display machine-status)
```

### Health Checking

```scheme
(use-modules (lab core))

;; Comprehensive health check
(define health-report (check-system-health "grey-area"))
(for-each (lambda (check)
            (let ((name (car check))
                  (result (cdr check)))
              (format #t "~a: ~a~%" name 
                      (assoc-ref result 'status))))
          health-report)
```

### Deployment Operations

```scheme
(use-modules (lab core))

;; Validate environment before deployment
(if (validate-environment)
    (begin
      ;; Update flake inputs
      (update-flake '((dry-run . #f)))
      
      ;; Deploy to development laptop
      (execute-nixos-rebuild "little-rascal" "switch" 
                           '((dry-run . #f)))
      
      ;; Deploy to storage server
      (execute-nixos-rebuild "sleeper-service" "boot" 
                           '((dry-run . #f))))
    (display "Environment validation failed\n"))
```

### Interactive REPL Usage

```sh
;; Start Guile REPL
$ guile -L packages

;; Load modules interactively
scheme@(guile-user)> (use-modules (lab core) (utils logging))

;; Set debug logging
scheme@(guile-user)> (set-log-level! 'debug)

;; Check machine status interactively
scheme@(guile-user)> (get-infrastructure-status "little-rascal")

;; Test health checks for development machine
scheme@(guile-user)> (check-system-health "little-rascal")

;; Test SSH connectivity to laptop
scheme@(guile-user)> (test-ssh-connection "little-rascal")

```

## API Reference

### Infrastructure Management

#### `(get-infrastructure-status [machine-name])`

Returns structured status information for all machines or specific machine.

**Parameters:**

- `machine-name` (optional): Specific machine to check

**Returns:**
Association list with machine status including:

- `machine`: Machine name
- `type`: 'local or 'remote
- `connection`: 'online or 'offline
- `services`: List of service statuses
- `system`: System information (uptime, load, memory, disk)
- `check-time`: Time taken for status check

#### `(check-system-health machine-name)`

Performs comprehensive health validation.

**Parameters:**

- `machine-name`: Target machine name

**Returns:**
List of health check results with status ('pass, 'fail, 'error) and details.

#### `(execute-nixos-rebuild machine-name mode options)`

Executes NixOS rebuild with error handling.

**Parameters:**

- `machine-name`: Target machine
- `mode`: Rebuild mode ("switch", "boot", "test")
- `options`: Configuration options (dry-run, etc.)

**Returns:**
Boolean indicating success/failure.

### Utility Functions

#### `(validate-environment)`

Validates home lab environment configuration.

#### `(update-flake options)`

Updates Nix flake inputs with optional dry-run.

### Configuration Access

#### `(get-machine-config machine-name)`

Retrieves machine-specific configuration.

#### `(get-all-machines)`

Returns list of all configured machines.

#### `(get-ssh-config machine-name)`

Gets SSH configuration for machine.

## Development

### Code Style Guidelines

1. **Naming Conventions**:
   - Use kebab-case for functions and variables
   - Predicates end with `?` (e.g., `machine-online?`)
   - Mutating procedures end with `!` (e.g., `set-log-level!`)

2. **Module Organization**:
   - One module per file
   - Clear export lists
   - Minimal external dependencies

3. **Error Handling**:
   - Use `catch` for exception handling
   - Provide informative error messages
   - Log errors with appropriate levels

4. **Documentation**:
   - Docstrings for all public functions
   - Inline comments for complex logic
   - Example usage in comments

### Testing

```scheme
;; Example test structure
(use-modules (srfi srfi-64)    ; Testing framework
             (lab core))

(test-begin "infrastructure-tests")

(test-assert "validate-environment"
  (validate-environment))

(test-assert "machine-connectivity"
  (test-ssh-connection "congenital-optimist"))

(test-end "infrastructure-tests")
```

### REPL-Driven Development

Guile's strength lies in interactive development:

```scheme
;; Reload modules during development
scheme@(guile-user)> (reload-module (resolve-module '(lab core)))

;; Test functions interactively
scheme@(guile-user)> (get-machine-config "sleeper-service")

;; Debug with modified log levels
scheme@(guile-user)> (set-log-level! 'debug)
scheme@(guile-user)> (test-ssh-connection "grey-area")
```

## Integration

### Model Context Protocol (MCP) Server

The lab package includes MCP server capabilities for IDE integration, providing:

- **Tools**: Infrastructure management operations
- **Resources**: Machine configurations and status
- **Prompts**: Common operational workflows

### VS Code Integration

Through the MCP server, the lab package integrates with VS Code to provide:

- Real-time infrastructure status
- Contextual deployment suggestions
- Configuration validation
- Automated documentation generation

### Command Line Interface

The package provides a CLI wrapper for common operations:

```bash
# Using the Guile-based CLI
./home-lab-tool.scm status
./home-lab-tool.scm deploy sleeper-service
./home-lab-tool.scm health-check grey-area
```

### Future Enhancements

1. **Web Dashboard**: Real-time monitoring interface
2. **Metrics Collection**: Prometheus integration
3. **Automated Recovery**: Self-healing capabilities
4. **Configuration Validation**: Pre-deployment checks
5. **Rollback Automation**: Automatic failure recovery

---

## Contributing

1. Follow the established code style
2. Add tests for new functionality
3. Update documentation
4. Test with multiple machine configurations

## License

This project follows the same license as the Home Lab repository.

---

**Note**: This tool represents a migration from traditional Bash scripting to a more robust, functional programming approach using GNU Guile Scheme. The benefits include better error handling, structured data management, and more maintainable code for complex infrastructure management tasks.
