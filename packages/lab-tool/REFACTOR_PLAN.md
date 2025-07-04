# Lab Tool Refactoring Plan - Functional Programming Approach

## Current Problems
- Deep nested parentheses causing syntax errors
- Functions doing too many things
- Hard to debug and maintain
- Mixed pure/impure logic

## New Structure

```
lab-tool/
├── core/
│   ├── config.scm          # Pure config access
│   ├── machine.scm         # Pure machine data structures
│   └── commands.scm        # Pure command building
├── io/
│   ├── ssh.scm             # Pure SSH command building
│   ├── rsync.scm           # Pure rsync command building
│   └── shell.scm           # Impure shell execution
├── deploy/
│   ├── ssh-strategy.scm    # Pure deployment strategy
│   ├── deploy-rs-strategy.scm # Pure deploy-rs strategy
│   └── executor.scm        # Impure deployment execution
├── health/
│   ├── checks.scm          # Pure health check logic
│   └── monitor.scm         # Impure health monitoring
└── main/
    ├── cli.scm             # Pure CLI parsing
    ├── dispatcher.scm      # Pure command dispatch
    └── runner.scm          # Impure main execution
```

## Functional Principles

### 1. Single Responsibility Functions
```scheme
;; Instead of one complex function doing everything:
(define (deploy-complex machine options) ...)

;; Break into focused functions:
(define (build-rsync-command source dest ssh-config) ...)
(define (build-nixos-rebuild-command flake-path machine mode) ...)
(define (execute-command command) ...)
(define (compose-ssh-deployment rsync-cmd rebuild-cmd) ...)
```

### 2. Pure vs Impure Separation
```scheme
;; Pure: No side effects, testable
(define (make-ssh-target user hostname) 
  (format #f "~a@~a" user hostname))

;; Impure: Clear side effects
(define (execute-ssh-command ssh-config command)
  (system (build-ssh-command ssh-config command)))
```

### 3. Function Composition
```scheme
;; Instead of deep nesting:
(let ((config (get-ssh-config machine))
      (command (build-command ...))
      (result (execute (format ...))))
  (if (success? result) ...))

;; Use composition:
(-> machine
    get-ssh-config
    (build-deployment-commands flake-path)
    execute-deployment
    handle-result)
```

### 4. Error Handling as Values
```scheme
;; Instead of exceptions in nested calls:
(catch #t (lambda () (complex-nested-operation)) error-handler)

;; Return result types:
(define (safe-ssh-connect machine)
  (if (valid-config? machine)
      `(success . ,(make-connection machine))
      `(error . "Invalid SSH config")))
```

## Implementation Steps

1. **Extract SSH utilities** (no more parentheses hell)
2. **Create pure command builders**
3. **Separate execution layer**
4. **Build composable deployment strategies**
5. **Clean CLI interface**

This will make debugging much easier - each small function can be tested independently!