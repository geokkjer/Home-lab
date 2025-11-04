# Leveraging NixOS Configuration vs Custom Implementation

## Current Situation Analysis

We're at risk of reimplementing significant functionality that NixOS already provides:

### What NixOS Already Handles
- **Machine Configuration**: Complete system configuration as code
- **Service Management**: Declarative service definitions
- **Deployment**: `nixos-rebuild` with atomic updates
- **Validation**: Configuration validation at build time
- **Dependencies**: Service dependency management
- **Environments**: Multiple configurations per machine
- **Templates**: NixOS modules for reusable configuration
- **Type Safety**: Nix language type system
- **Inheritance**: Module imports and overrides

### What We're Duplicating
- Machine metadata and properties
- Service definitions and health checks
- Deployment strategies and validation
- Configuration inheritance and composition
- Environment-specific overrides

## Better Approach: NixOS-Native Strategy

### Core Principle
**Let NixOS handle configuration, let lab tool handle orchestration**

### Revised Architecture

#### 1. NixOS Handles Configuration
```nix
# hosts/sleeper-service/configuration.nix
{ config, lib, pkgs, ... }:
{
  # NixOS handles all the configuration
  services.nginx.enable = true;
  services.postgresql.enable = true;
  
  # Lab-specific metadata as NixOS options
  lab.machine = {
    role = "application-server";
    groups = [ "infrastructure" "database" ];
    rebootOrder = 1;
    dependencies = [ ];
    healthChecks = [
      { type = "http"; url = "http://localhost:80/health"; }
      { type = "tcp"; port = 5432; }
    ];
    orchestration = {
      deployStrategy = "rolling";
      rebootDelay = 0;
      criticalityLevel = "high";
    };
  };
}
```

#### 2. Lab Tool Handles Orchestration
```scheme
;; lab tool queries NixOS configuration, doesn't define it
(define (get-machine-metadata machine-name)
  "Extract lab metadata from NixOS configuration"
  (let ((config-path (format #f "hosts/~a/configuration.nix" machine-name)))
    (extract-lab-metadata-from-nix-config config-path)))

(define (get-reboot-sequence)
  "Get reboot sequence from NixOS configurations"
  (let ((machines (get-all-machines)))
    (sort machines 
          (lambda (a b) 
            (< (get-reboot-order a) (get-reboot-order b))))))
```

### Implementation Strategy

#### 1. Create NixOS Lab Module
```nix
# nix/modules/lab-machine.nix
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.lab.machine;
in
{
  options.lab.machine = {
    role = mkOption {
      type = types.str;
      description = "Machine role in the lab";
      example = "web-server";
    };
    
    groups = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Groups this machine belongs to";
    };
    
    rebootOrder = mkOption {
      type = types.int;
      description = "Order in reboot sequence (lower = earlier)";
    };
    
    dependencies = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Machines this depends on";
    };
    
    healthChecks = mkOption {
      type = types.listOf (types.submodule {
        options = {
          type = mkOption {
            type = types.enum [ "http" "tcp" "command" ];
            description = "Type of health check";
          };
          url = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "URL for HTTP health checks";
          };
          port = mkOption {
            type = types.nullOr types.int;
            default = null;
            description = "Port for TCP health checks";
          };
          command = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Command for command-based health checks";
          };
        };
      });
      default = [];
      description = "Health check configurations";
    };
    
    orchestration = mkOption {
      type = types.submodule {
        options = {
          deployStrategy = mkOption {
            type = types.enum [ "rolling" "blue-green" "recreate" ];
            default = "rolling";
            description = "Deployment strategy";
          };
          
          rebootDelay = mkOption {
            type = types.int;
            default = 600; # 10 minutes
            description = "Delay in seconds before this machine reboots";
          };
          
          criticalityLevel = mkOption {
            type = types.enum [ "low" "medium" "high" "critical" ];
            default = "medium";
            description = "Service criticality level";
          };
        };
      };
      default = {};
      description = "Orchestration configuration";
    };
  };

  config = {
    # Generate machine metadata file for lab tool consumption
    environment.etc."lab-machine-metadata.json".text = builtins.toJSON {
      inherit (cfg) role groups rebootOrder dependencies healthChecks orchestration;
      hostname = config.networking.hostName;
      services = builtins.attrNames (lib.filterAttrs (n: v: v.enable or false) config.services);
    };
  };
}
```

#### 2. Simplified Lab Tool
```scheme
;; lab/nix-integration.scm - NixOS integration module
(define-module (lab nix-integration)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (json)
  #:export (get-machine-metadata-from-nix
            get-all-nix-machines
            get-reboot-sequence-from-nix
            build-nix-config
            evaluate-nix-expr))

(define (evaluate-nix-expr expr)
  "Evaluate a Nix expression and return the result"
  (let* ((cmd (format #f "nix eval --json --expr '~a'" expr))
         (port (open-input-pipe cmd))
         (output (read-string port)))
    (close-pipe port)
    (if (string-null? output)
        #f
        (json-string->scm output))))

(define (get-machine-metadata-from-nix machine-name)
  "Get machine metadata from NixOS configuration"
  (let* ((expr (format #f 
                 "(import ./hosts/~a/configuration.nix {}).lab.machine // { hostname = \"~a\"; }"
                 machine-name machine-name))
         (metadata (evaluate-nix-expr expr)))
    metadata))

(define (get-all-nix-machines)
  "Get all machines by scanning hosts directory"
  (let* ((hosts-expr "(builtins.attrNames (builtins.readDir ./hosts))")
         (hosts (evaluate-nix-expr hosts-expr)))
    (if hosts hosts '())))

(define (get-reboot-sequence-from-nix)
  "Get reboot sequence from NixOS configurations"
  (let* ((machines (get-all-nix-machines))
         (machine-data (map (lambda (machine)
                             (cons machine (get-machine-metadata-from-nix machine)))
                           machines)))
    (sort machine-data
          (lambda (a b)
            (< (assoc-ref (cdr a) 'rebootOrder)
               (assoc-ref (cdr b) 'rebootOrder))))))
```

#### 3. Updated Machine Configurations
```nix
# hosts/sleeper-service/configuration.nix
{ config, lib, pkgs, ... }:
{
  imports = [
    ../../nix/modules/lab-machine.nix
    # ... other imports
  ];

  # Standard NixOS configuration
  services.nginx = {
    enable = true;
    # ... nginx config
  };
  
  services.postgresql = {
    enable = true;
    # ... postgresql config
  };

  # Lab orchestration metadata
  lab.machine = {
    role = "application-server";
    groups = [ "infrastructure" "backend" ];
    rebootOrder = 1;
    dependencies = [ ];
    healthChecks = [
      {
        type = "http";
        url = "http://localhost:80/health";
      }
      {
        type = "tcp";
        port = 5432;
      }
    ];
    orchestration = {
      deployStrategy = "rolling";
      rebootDelay = 0;
      criticalityLevel = "high";
    };
  };
}
```

#### 4. Lab Tool Integration
```scheme
;; Update main.scm to use NixOS integration
(use-modules ;; ...existing modules...
             (lab nix-integration))

(define (cmd-machines)
  "List all configured machines from NixOS"
  (log-info "Listing machines from NixOS configurations...")
  (let ((machines (get-all-nix-machines)))
    (format #t "Configured Machines (from NixOS):\n")
    (for-each (lambda (machine)
                (let ((metadata (get-machine-metadata-from-nix machine)))
                  (format #t "  ~a (~a) - ~a\n" 
                          machine
                          (assoc-ref metadata 'role)
                          (string-join (assoc-ref metadata 'groups) ", "))))
              machines)))

(define (cmd-orchestrator-sequence)
  "Show the orchestrated reboot sequence"
  (log-info "Getting reboot sequence from NixOS configurations...")
  (let ((sequence (get-reboot-sequence-from-nix)))
    (format #t "Reboot Sequence:\n")
    (for-each (lambda (machine-data)
                (let ((machine (car machine-data))
                      (metadata (cdr machine-data)))
                  (format #t "  ~a. ~a (delay: ~a seconds)\n"
                          (assoc-ref metadata 'rebootOrder)
                          machine
                          (assoc-ref metadata 'orchestration 'rebootDelay))))
              sequence)))
```

### Benefits of This Approach

#### 1. Leverage NixOS Strengths
- **Configuration Management**: NixOS handles all system configuration
- **Validation**: Nix language validates configuration at build time
- **Atomic Updates**: `nixos-rebuild` provides atomic system updates
- **Rollbacks**: Nix generations for automatic rollback
- **Reproducibility**: Identical configurations across environments

#### 2. Lab Tool Focus
- **Orchestration**: Coordinate updates across multiple machines
- **Sequencing**: Handle reboot ordering and dependencies
- **Monitoring**: Health checks and status reporting
- **Communication**: SSH coordination and logging

#### 3. Reduced Complexity
- **No Duplication**: Don't reimplement what NixOS provides
- **Native Integration**: Work with NixOS's natural patterns
- **Maintainability**: Less custom code to maintain
- **Ecosystem**: Leverage existing NixOS modules and community

### Migration Strategy

#### Phase 1: Add Lab Module to NixOS
1. Create `lab-machine.nix` module
2. Add to each machine configuration
3. Test metadata extraction

#### Phase 2: Update Lab Tool
1. Replace custom config with NixOS integration
2. Update commands to read from NixOS configs
3. Test orchestration with new metadata

#### Phase 3: Enhanced Features
1. Add more sophisticated orchestration
2. Integrate with NixOS deployment tools
3. Add monitoring and alerting

### Example: Simplified Orchestrator
```nix
# The orchestrator service becomes much simpler
systemd.services.lab-orchestrator = {
  script = ''
    # Update flake
    nix flake update
    
    # Get reboot sequence from NixOS configs
    SEQUENCE=$(lab get-reboot-sequence)
    
    # Deploy to all machines
    lab deploy-all
    
    # Execute reboot sequence
    for machine_delay in $SEQUENCE; do
      machine=$(echo $machine_delay | cut -d: -f1)
      delay=$(echo $machine_delay | cut -d: -f2)
      
      sleep $delay
      lab reboot $machine
    done
  '';
};
```

## Conclusion

By leveraging NixOS's existing configuration system instead of reinventing it, we get:

- **Less code to maintain**
- **Better integration with the Nix ecosystem**
- **Validation and type safety from Nix**
- **Standard NixOS patterns and practices**
- **Focus on actual orchestration needs**

The lab tool becomes a **coordination layer** rather than a configuration management system, which is exactly what you need for homelab orchestration.