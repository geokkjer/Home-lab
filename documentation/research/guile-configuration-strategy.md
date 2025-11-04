# Guile-Based Programmatic Configuration Strategy

## Overview
Research into implementing a robust, programmatic configuration system using Guile's strengths for the lab tool, moving beyond simple YAML to leverage Scheme's expressiveness and composability.

## Why Guile for Configuration?

### Advantages Over YAML
- **Programmable**: Logic, conditionals, functions in configuration
- **Composable**: Reusable configuration snippets and inheritance
- **Type Safety**: Scheme's type system prevents configuration errors
- **Extensible**: Custom DSL capabilities for lab-specific concepts
- **Dynamic**: Runtime configuration generation and validation
- **Functional**: Pure functions for configuration transformation

### Guile-Specific Benefits
- **S-expressions**: Natural data structure representation
- **Modules**: Clean separation of configuration concerns
- **Macros**: Custom syntax for common patterns
- **GOOPS**: Object-oriented configuration when needed
- **Records**: Structured data with validation

## Configuration Architecture

### Hierarchical Structure
```
config/
├── machines/          # Machine-specific configurations
│   ├── sleeper-service.scm
│   ├── grey-area.scm
│   ├── reverse-proxy.scm
│   └── orchestrator.scm
├── groups/           # Machine group definitions
│   ├── infrastructure.scm
│   ├── services.scm
│   └── development.scm
├── environments/     # Environment-specific configs
│   ├── production.scm
│   ├── staging.scm
│   └── development.scm
├── templates/        # Reusable configuration templates
│   ├── web-server.scm
│   ├── database.scm
│   └── monitoring.scm
└── base.scm         # Core configuration framework
```

## Implementation Plan

### 1. Configuration Framework Module
```scheme
;; config/base.scm - Core configuration framework
(define-module (config base)
  #:use-module (srfi srfi-9)    ; Records
  #:use-module (srfi srfi-1)    ; Lists
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (define-machine
            define-group
            define-environment
            machine?
            group?
            get-machine-config
            get-group-machines
            validate-config
            merge-configs
            resolve-inheritance))

;; Machine record type with validation
(define-record-type <machine>
  (make-machine name hostname user services groups environment metadata)
  machine?
  (name         machine-name)
  (hostname     machine-hostname)
  (user         machine-user)
  (services     machine-services)
  (groups       machine-groups)
  (environment  machine-environment)
  (metadata     machine-metadata))

;; Group record type
(define-record-type <group>
  (make-group name machines deployment-order dependencies metadata)
  group?
  (name             group-name)
  (machines         group-machines)
  (deployment-order group-deployment-order)
  (dependencies     group-dependencies)
  (metadata         group-metadata))

;; Environment record type
(define-record-type <environment>
  (make-environment name settings overrides)
  environment?
  (name      environment-name)
  (settings  environment-settings)
  (overrides environment-overrides))

;; Configuration DSL macros
(define-syntax define-machine
  (syntax-rules ()
    ((_ name hostname config ...)
     (make-machine 'name hostname
                   (parse-machine-config (list config ...))))))

(define-syntax define-group
  (syntax-rules ()
    ((_ name machines ...)
     (make-group 'name (list machines ...)
                 (parse-group-config (list machines ...))))))

;; Pure function: Parse machine configuration
(define (parse-machine-config config-list)
  "Parse machine configuration from keyword-value pairs"
  (let loop ((config config-list)
             (result '()))
    (match config
      (() result)
      ((#:user user . rest)
       (loop rest (cons `(user . ,user) result)))
      ((#:services services . rest)
       (loop rest (cons `(services . ,services) result)))
      ((#:groups groups . rest)
       (loop rest (cons `(groups . ,groups) result)))
      ((#:environment env . rest)
       (loop rest (cons `(environment . ,env) result)))
      ((key value . rest)
       (loop rest (cons `(,key . ,value) result))))))

;; Configuration inheritance resolver
(define (resolve-inheritance machine-config template-configs)
  "Resolve configuration inheritance from templates"
  (fold merge-configs machine-config template-configs))

;; Configuration merger
(define (merge-configs base-config override-config)
  "Merge two configurations, with override taking precedence"
  (append override-config
          (filter (lambda (item)
                    (not (assoc (car item) override-config)))
                  base-config)))

;; Configuration validator
(define (validate-config config)
  "Validate configuration completeness and consistency"
  (and (assoc 'hostname config)
       (assoc 'user config)
       (string? (assoc-ref config 'hostname))
       (string? (assoc-ref config 'user))))
```

### 2. Machine Configuration Examples
```scheme
;; config/machines/sleeper-service.scm
(define-module (config machines sleeper-service)
  #:use-module (config base)
  #:use-module (config templates web-server)
  #:export (sleeper-service-config))

(define sleeper-service-config
  (define-machine sleeper-service "sleeper-service.local"
    #:user "root"
    #:services '(nginx postgresql redis)
    #:groups '(infrastructure database)
    #:environment 'production
    #:ssh-port 22
    #:deploy-strategy 'rolling
    #:health-checks '((http "http://localhost:80/health")
                      (tcp 5432)
                      (tcp 6379))
    #:dependencies '()
    #:reboot-delay 0  ; First to reboot
    #:backup-required #t
    #:monitoring-enabled #t
    #:metadata `((description . "Main application server")
                 (maintainer . "geir")
                 (criticality . high))))

;; config/machines/grey-area.scm  
(define-module (config machines grey-area)
  #:use-module (config base)
  #:use-module (config templates monitoring)
  #:export (grey-area-config))

(define grey-area-config
  (define-machine grey-area "grey-area.local"
    #:user "root"
    #:services '(prometheus grafana alertmanager)
    #:groups '(infrastructure monitoring)
    #:environment 'production
    #:ssh-port 22
    #:deploy-strategy 'blue-green
    #:health-checks '((http "http://localhost:3000/health")
                      (http "http://localhost:9090/-/healthy"))
    #:dependencies '(sleeper-service)
    #:reboot-delay 600  ; 10 minutes after sleeper-service
    #:backup-required #f
    #:monitoring-enabled #t
    #:metadata `((description . "Monitoring and observability")
                 (maintainer . "geir")
                 (criticality . medium))))

;; config/machines/reverse-proxy.scm
(define-module (config machines reverse-proxy)
  #:use-module (config base)
  #:use-module (config templates proxy)
  #:export (reverse-proxy-config))

(define reverse-proxy-config
  (define-machine reverse-proxy "reverse-proxy.local"
    #:user "root"
    #:services '(nginx traefik)
    #:groups '(infrastructure edge)
    #:environment 'production
    #:ssh-port 22
    #:deploy-strategy 'rolling
    #:health-checks '((http "http://localhost:80/health")
                      (tcp 443))
    #:dependencies '(sleeper-service grey-area)
    #:reboot-delay 1200  ; 20 minutes after sleeper-service
    #:backup-required #f
    #:monitoring-enabled #t
    #:public-facing #t
    #:ssl-certificates '("homelab.local" "*.homelab.local")
    #:metadata `((description . "Edge proxy and load balancer")
                 (maintainer . "geir")
                 (criticality . high))))
```

### 3. Group Configuration
```scheme
;; config/groups/infrastructure.scm
(define-module (config groups infrastructure)
  #:use-module (config base)
  #:export (infrastructure-group))

(define infrastructure-group
  (define-group infrastructure
    #:machines '(sleeper-service grey-area reverse-proxy)
    #:deployment-order '(sleeper-service grey-area reverse-proxy)
    #:reboot-sequence '((sleeper-service . 0)
                        (grey-area . 600)
                        (reverse-proxy . 1200))
    #:update-strategy 'staggered
    #:rollback-strategy 'reverse-order
    #:health-check-required #t
    #:maintenance-window '(02:00 . 06:00)
    #:notification-channels '(email discord)
    #:metadata `((description . "Core infrastructure services")
                 (owner . "platform-team")
                 (sla . "99.9%"))))

;; config/groups/services.scm
(define-module (config groups services)
  #:use-module (config base)
  #:export (services-group))

(define services-group
  (define-group services
    #:machines '(app-server-01 app-server-02 worker-01)
    #:deployment-order '(worker-01 app-server-01 app-server-02)
    #:update-strategy 'rolling
    #:canary-percentage 25
    #:health-check-required #t
    #:dependencies '(infrastructure)
    #:metadata `((description . "Application services")
                 (owner . "application-team"))))
```

### 4. Template System
```scheme
;; config/templates/web-server.scm
(define-module (config templates web-server)
  #:use-module (config base)
  #:export (web-server-template))

(define web-server-template
  '((services . (nginx))
    (ports . (80 443))
    (health-checks . ((http "http://localhost:80/health")))
    (deploy-strategy . rolling)
    (backup-required . #f)
    (monitoring-enabled . #t)
    (firewall-rules . ((allow 80 tcp)
                       (allow 443 tcp)))))

;; config/templates/database.scm
(define-module (config templates database)
  #:use-module (config base)
  #:export (database-template))

(define database-template
  '((services . (postgresql))
    (ports . (5432))
    (health-checks . ((tcp 5432)
                      (pg-isready)))
    (deploy-strategy . blue-green)
    (backup-required . #t)
    (backup-schedule . "0 2 * * *")
    (monitoring-enabled . #t)
    (replication-enabled . #f)
    (firewall-rules . ((allow 5432 tcp internal)))))
```

### 5. Configuration Loader Integration
```scheme
;; lab/config-loader.scm - Integration with existing lab tool
(define-module (lab config-loader)
  #:use-module (config base)
  #:use-module (config machines sleeper-service)
  #:use-module (config machines grey-area)
  #:use-module (config machines reverse-proxy)
  #:use-module (config groups infrastructure)
  #:use-module (utils logging)
  #:export (load-lab-config
            get-all-machines
            get-machine-info
            get-reboot-sequence
            get-deployment-order))

;; Global configuration registry
(define *lab-config* 
  `((machines . ,(list sleeper-service-config
                       grey-area-config
                       reverse-proxy-config))
    (groups . ,(list infrastructure-group))
    (environments . ())))

;; Pure function: Get all machine configurations
(define (get-all-machines-from-config)
  "Get all machine configurations"
  (assoc-ref *lab-config* 'machines))

;; Pure function: Find machine by name
(define (find-machine-by-name name machines)
  "Find machine configuration by name"
  (find (lambda (machine)
          (eq? (machine-name machine) name))
        machines))

;; Integration function: Get machine info for existing lab tool
(define (get-machine-info machine-name)
  "Get machine information in format expected by existing lab tool"
  (let* ((machines (get-all-machines-from-config))
         (machine (find-machine-by-name machine-name machines)))
    (if machine
        `((hostname . ,(machine-hostname machine))
          (user . ,(machine-user machine))
          (ssh-port . ,(assoc-ref (machine-metadata machine) 'ssh-port))
          (is-local . ,(string=? (machine-hostname machine) "localhost")))
        #f)))

;; Get reboot sequence for orchestrator
(define (get-reboot-sequence)
  "Get the ordered reboot sequence with delays"
  (let ((infra-group (car (assoc-ref *lab-config* 'groups))))
    (assoc-ref (group-metadata infra-group) 'reboot-sequence)))

;; Get deployment order
(define (get-deployment-order group-name)
  "Get deployment order for a group"
  (let* ((groups (assoc-ref *lab-config* 'groups))
         (group (find (lambda (g) (eq? (group-name g) group-name)) groups)))
    (if group
        (group-deployment-order group)
        '())))
```

### 6. Integration with Existing Lab Tool
```scheme
;; Update lab/machines.scm to use new config system
(define-module (lab machines)
  #:use-module (lab config-loader)
  ;; ...existing modules...
  #:export (;; ...existing exports...
            get-ssh-config
            validate-machine-name
            list-machines))

;; Update existing functions to use new config system
(define (get-ssh-config machine-name)
  "Get SSH configuration for machine - updated to use new config"
  (get-machine-info machine-name))

(define (validate-machine-name machine-name)
  "Validate machine name exists in configuration"
  (let ((machine-info (get-machine-info machine-name)))
    (not (eq? machine-info #f))))

(define (list-machines)
  "List all configured machines"
  (map machine-name (get-all-machines-from-config)))

;; New function: Get machines by group
(define (get-machines-in-group group-name)
  "Get all machines in a specific group"
  (let ((deployment-order (get-deployment-order group-name)))
    (if deployment-order
        deployment-order
        '())))
```

## Advanced Configuration Features

### 1. Environment-Specific Overrides
```scheme
;; config/environments/production.scm
(define production-environment
  (make-environment 'production
    ;; Base settings
    '((log-level . info)
      (debug-mode . #f)
      (monitoring-enabled . #t)
      (backup-enabled . #t))
    ;; Machine-specific overrides
    '((sleeper-service . ((log-level . warn)
                          (max-connections . 1000)))
      (grey-area . ((retention-days . 90))))))

;; config/environments/development.scm  
(define development-environment
  (make-environment 'development
    '((log-level . debug)
      (debug-mode . #t)
      (monitoring-enabled . #f)
      (backup-enabled . #f))
    '()))
```

### 2. Dynamic Configuration Generation
```scheme
;; config/generators/auto-scaling.scm
(define (generate-web-server-configs count)
  "Dynamically generate web server configurations"
  (map (lambda (i)
         (define-machine (string->symbol (format #f "web-~2,'0d" i))
                        (format #f "web-~2,'0d.local" i)
           #:user "root"
           #:services '(nginx)
           #:groups '(web-servers)
           #:template web-server-template))
       (iota count 1)))

;; Usage in configuration
(define web-servers (generate-web-server-configs 3))
```

### 3. Configuration Validation
```scheme
;; config/validation.scm
(define-module (config validation)
  #:use-module (config base)
  #:export (validate-lab-config
            check-dependencies
            validate-network-topology))

(define (validate-lab-config config)
  "Comprehensive configuration validation"
  (and (validate-machine-configs config)
       (validate-group-dependencies config)
       (validate-network-topology config)
       (validate-reboot-sequence config)))

(define (validate-machine-configs config)
  "Validate all machine configurations"
  (every validate-config 
         (map machine-metadata 
              (assoc-ref config 'machines))))

(define (validate-reboot-sequence config)
  "Validate reboot sequence dependencies"
  (let ((sequence (get-reboot-sequence)))
    (check-dependency-order sequence)))
```

## Migration Strategy

### Phase 1: Parallel Configuration System
1. Implement new config modules alongside existing YAML
2. Add config-loader integration layer
3. Update lab tool to optionally use new system
4. Validate equivalent behavior

### Phase 2: Feature Enhancement
1. Add dynamic configuration capabilities
2. Implement validation and error checking
3. Add environment-specific overrides
4. Enhance orchestrator with new features

### Phase 3: Full Migration
1. Migrate all existing configurations
2. Remove YAML dependency
3. Add advanced features (templates, inheritance)
4. Optimize performance

## Benefits of This Approach

### Developer Experience
- **Rich Configuration**: Logic and computation in config
- **Type Safety**: Catch errors at config load time
- **Reusability**: Templates and inheritance reduce duplication
- **Composability**: Mix and match configuration components
- **Validation**: Comprehensive consistency checking

### Operational Benefits
- **Dynamic Scaling**: Generate configurations programmatically
- **Environment Management**: Seamless dev/staging/prod handling
- **Dependency Tracking**: Automatic dependency resolution
- **Extensibility**: Easy to add new machine types and features

### Integration Advantages
- **Native Guile**: No external dependencies or parsers
- **Performance**: Compiled configuration, fast access
- **Debugging**: Full Guile debugging tools available
- **Flexibility**: Can mix declarative and imperative approaches

## File Structure Summary
```
/home/geir/Home-lab/
├── packages/lab-tool/
│   ├── config/
│   │   ├── base.scm              # Configuration framework
│   │   ├── machines/             # Machine definitions
│   │   ├── groups/               # Group definitions  
│   │   ├── environments/         # Environment configs
│   │   ├── templates/            # Reusable templates
│   │   └── validation.scm        # Configuration validation
│   ├── lab/
│   │   ├── config-loader.scm     # Integration layer
│   │   ├── machines.scm          # Updated machine management
│   │   └── ...existing modules...
│   └── main.scm                  # Updated main entry point
```

This approach leverages Guile's strengths to create a powerful, flexible configuration system that grows with your homelab while maintaining the K.I.S.S principles of your current tool.