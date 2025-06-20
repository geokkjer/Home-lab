# Lab-Wide Auto-Update Service with Staggered Reboots

## Overview
A NixOS service that runs on this machine (orchestrator) to update the entire homelab using existing lab tool commands, then perform staggered reboots to ensure you wake up to a freshly updated lab every morning.

## Service Architecture

### Central Orchestrator Approach
- Runs on this machine (the controller)
- Uses existing `lab update` and `lab deploy-all` commands
- Orchestrates staggered reboots: sleeper-service → grey-area → reverse-proxy → self
- 10-minute delays between each machine reboot

## Implementation

### 1. Nix Service Module
```nix
# /home/geir/Home-lab/nix/modules/lab-orchestrator.nix
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.lab-orchestrator;
  
  labPath = "/home/geir/Home-lab";
  
  # Machine reboot order with delays
  rebootSequence = [
    { machine = "sleeper-service"; delay = 0; }
    { machine = "grey-area"; delay = 600; }  # 10 minutes
    { machine = "reverse-proxy"; delay = 1200; }  # 20 minutes total
    { machine = "self"; delay = 1800; }  # 30 minutes total
  ];
  
  orchestratorScript = pkgs.writeShellScript "lab-orchestrator" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    LOG_FILE="/var/log/lab-orchestrator.log"
    LAB_TOOL="${labPath}/result/bin/lab"
    
    log() {
      echo "$(date '+%Y-%m-%d %H:%M:%S'): $1" | tee -a "$LOG_FILE"
    }
    
    # Ensure lab tool is available
    if [[ ! -x "$LAB_TOOL" ]]; then
      log "ERROR: Lab tool not found at $LAB_TOOL"
      log "Building lab tool first..."
      cd "${labPath}"
      if ! nix build .#lab-tool; then
        log "ERROR: Failed to build lab tool"
        exit 1
      fi
    fi
    
    log "=== Starting Lab-Wide Update Orchestration ==="
    
    # Step 1: Update flake inputs
    log "Updating flake inputs..."
    cd "${labPath}"
    if ! $LAB_TOOL update; then
      log "ERROR: Failed to update flake inputs"
      exit 1
    fi
    log "Flake inputs updated successfully"
    
    # Step 2: Deploy to all machines
    log "Deploying to all machines..."
    if ! $LAB_TOOL deploy-all; then
      log "ERROR: Failed to deploy to all machines"
      exit 1
    fi
    log "Deployment completed successfully"
    
    # Step 3: Staggered reboots
    log "Starting staggered reboot sequence..."
    
    # Reboot sleeper-service immediately
    log "Rebooting sleeper-service..."
    if $LAB_TOOL reboot sleeper-service; then
      log "✓ sleeper-service reboot initiated"
    else
      log "WARNING: Failed to reboot sleeper-service"
    fi
    
    # Wait 10 minutes, then reboot grey-area
    log "Waiting 10 minutes before rebooting grey-area..."
    sleep 600
    log "Rebooting grey-area..."
    if $LAB_TOOL reboot grey-area; then
      log "✓ grey-area reboot initiated"
    else
      log "WARNING: Failed to reboot grey-area"
    fi
    
    # Wait 10 minutes, then reboot reverse-proxy
    log "Waiting 10 minutes before rebooting reverse-proxy..."
    sleep 600
    log "Rebooting reverse-proxy..."
    if $LAB_TOOL reboot reverse-proxy; then
      log "✓ reverse-proxy reboot initiated"
    else
      log "WARNING: Failed to reboot reverse-proxy"
    fi
    
    # Wait 10 minutes, then reboot self
    log "Waiting 10 minutes before rebooting self..."
    sleep 600
    log "Rebooting this machine (orchestrator)..."
    log "=== Lab Update Orchestration Completed ==="
    
    # Reboot this machine
    systemctl reboot
  '';
  
in
{
  options.services.lab-orchestrator = {
    enable = mkEnableOption "Lab orchestrator auto-update service";
    
    schedule = mkOption {
      type = types.str;
      default = "02:00";
      description = "Time to start lab update (HH:MM format)";
    };
    
    user = mkOption {
      type = types.str;
      default = "geir";
      description = "User to run the lab tool as";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.lab-orchestrator = {
      description = "Lab-Wide Update Orchestrator";
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = "users";
        WorkingDirectory = labPath;
        ExecStart = "${orchestratorScript}";
        # Give it plenty of time (2 hours)
        TimeoutStartSec = 7200;
      };
      # Ensure network is ready
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
    };

    systemd.timers.lab-orchestrator = {
      description = "Lab-Wide Update Orchestrator Timer";
      timerConfig = {
        OnCalendar = "*-*-* ${cfg.schedule}:00";
        Persistent = true;
        # No randomization - we want predictable timing
      };
      wantedBy = [ "timers.target" ];
    };

    # Ensure log directory and file exist with proper permissions
    systemd.tmpfiles.rules = [
      "f /var/log/lab-orchestrator.log 0644 ${cfg.user} users -"
    ];
  };
}
```

### 2. Lab Tool Reboot Command Extension
Add reboot capability to the existing Guile lab tool:

```scheme
;; lab/reboot.scm - New module for machine reboots
(define-module (lab reboot)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (utils logging)
  #:use-module (lab machines)
  #:export (reboot-machine))

(define (execute-ssh-command hostname command)
  "Execute command on remote machine via SSH"
  (let* ((ssh-cmd (format #f "ssh root@~a '~a'" hostname command))
         (port (open-input-pipe ssh-cmd))
         (output (read-string port)))
    (close-pipe port)
    output))

(define (reboot-machine machine-name)
  "Reboot a specific machine via SSH"
  (log-info "Attempting to reboot machine: ~a" machine-name)
  
  (if (validate-machine-name machine-name)
      (let* ((ssh-config (get-ssh-config machine-name))
             (hostname (if ssh-config 
                          (assoc-ref ssh-config 'hostname)
                          machine-name))
             (is-local (if ssh-config 
                          (assoc-ref ssh-config 'is-local) 
                          #f)))
        
        (cond
         (is-local
          (log-info "Rebooting local machine...")
          (system "sudo systemctl reboot")
          #t)
         
         (hostname
          (log-info "Rebooting ~a via SSH..." hostname)
          (catch #t
            (lambda ()
              ;; Send reboot command - connection will drop
              (execute-ssh-command hostname "sudo systemctl reboot")
              (log-success "Reboot command sent to ~a" machine-name)
              #t)
            (lambda (key . args)
              ;; SSH connection drop is expected during reboot
              (if (string-contains (format #f "~a" args) "Connection")
                  (begin
                    (log-info "Connection dropped (expected during reboot)")
                    #t)
                  (begin
                    (log-error "Failed to reboot ~a: ~a" machine-name args)
                    #f)))))
         
         (else
          (log-error "No hostname found for machine: ~a" machine-name)
          #f)))
      
      (begin
        (log-error "Invalid machine name: ~a" machine-name)
        #f)))
```

### 3. CLI Integration
Update the main.scm dispatcher to include reboot command:

```scheme
;; main.scm (additions to command dispatcher)
(use-modules ;; ...existing modules...
             (lab reboot))

;; Add to dispatch-command function
(define (dispatch-command command args)
  "Dispatch command with appropriate handler"
  (match command
    ;; ...existing cases...
    
    ('reboot
     (if (null? args)
         (begin
           (log-error "reboot command requires machine name")
           (format #t "Usage: lab reboot <machine>\n"))
         (let ((result (reboot-machine (car args))))
           (if result
               (log-success "Reboot initiated")
               (log-error "Reboot failed")))))
    
    ;; ...rest of existing cases...
    ))

;; Update help text to include reboot command
(define (get-help-text)
  "Pure function returning help text"
  "Home Lab Tool - K.I.S.S Refactored Edition

USAGE: lab <command> [args...]

COMMANDS:
  status              Show infrastructure status
  machines            List all machines  
  deploy <machine>    Deploy configuration to machine
  deploy-all          Deploy to all machines
  update              Update flake inputs
  health [machine]    Check machine health (all if no machine specified)
  ssh <machine>       SSH to machine
  reboot <machine>    Reboot machine via SSH
  test-modules        Test modular implementation
  help                Show this help

EXAMPLES:
  lab status
  lab machines
  lab deploy congenital-optimist
  lab deploy-all
  lab update
  lab health
  lab health sleeper-service
  lab ssh sleeper-service
  lab reboot sleeper-service
  lab test-modules
")

### 4. Configuration
Enable the service on this machine (the orchestrator):

```nix
# hosts/this-machine/configuration.nix
{
  imports = [
    ../../nix/modules/lab-orchestrator.nix
  ];

  services.lab-orchestrator = {
    enable = true;
    schedule = "02:00";  # 2 AM start
    user = "geir";
  };
}
```

## Timeline Breakdown

### Nightly Execution (Starting 2:00 AM)
```
02:00 - Start orchestration
02:00-02:15 - Update flake inputs (lab update)
02:15-02:45 - Deploy to all machines (lab deploy-all)  
02:45 - Reboot sleeper-service
02:55 - Reboot grey-area (10 min later)
03:05 - Reboot reverse-proxy (10 min later)
03:15 - Reboot orchestrator machine (10 min later)
03:20 - All machines back online and updated
```

### Total Duration: ~1 hour 20 minutes
- Deployment: ~30 minutes
- Staggered reboots: ~50 minutes
- Everything done by 3:20 AM

## Safety Features

### Logging and Monitoring
```bash
# Check orchestrator logs
sudo journalctl -u lab-orchestrator.service -f

# Check orchestrator log file
tail -f /var/log/lab-orchestrator.log

# Check timer status
systemctl status lab-orchestrator.timer
```

### Manual Controls
```bash
# Start update manually
sudo systemctl start lab-orchestrator.service

# Disable automatic updates
sudo systemctl disable lab-orchestrator.timer

# Check when next run is scheduled
systemctl list-timers lab-orchestrator.timer
```

### Recovery Options
```bash
# If orchestration fails, machines can be individually managed
lab deploy sleeper-service
lab deploy grey-area
lab deploy reverse-proxy

# Emergency reboot sequence
lab reboot sleeper-service
sleep 600
lab reboot grey-area
sleep 600
lab reboot reverse-proxy
```

## Machine Configuration Requirements

### SSH Key Setup
Ensure this machine can SSH to all target machines:
```bash
# Test connectivity
ssh root@sleeper-service "echo 'Connection OK'"
ssh root@grey-area "echo 'Connection OK'"  
ssh root@reverse-proxy "echo 'Connection OK'"
```

### Lab Tool Configuration
Ensure lab.yaml includes all machines:
```yaml
machines:
  sleeper-service:
    host: sleeper-service.local
    user: root
  grey-area:
    host: grey-area.local
    user: root
  reverse-proxy:
    host: reverse-proxy.local
    user: root
```

## Deployment Steps

### 1. Create the Service Module
Add the Nix module file and import it

### 2. Extend Lab Tool
Add reboot command functionality

### 3. Test Components
```bash
# Build the lab tool first
cd /home/geir/Home-lab
nix build .#lab-tool

# Test lab commands work
./result/bin/lab update
./result/bin/lab deploy-all
./result/bin/lab machines
./result/bin/lab reboot sleeper-service  # Test reboot (be careful!)
```

### 4. Enable Service
```bash
# Add to configuration and rebuild
nixos-rebuild switch

# Verify timer is active
systemctl status lab-orchestrator.timer
```

### 5. Monitor First Run
```bash
# Watch the logs during first execution
sudo journalctl -u lab-orchestrator.service -f
```

## Benefits

### Morning Routine
- Wake up to fully updated homelab
- All services running latest versions
- No manual intervention needed
- Predictable update schedule

### Reliability
- Uses existing, tested lab tool commands
- Proper error handling and logging
- Graceful degradation if individual reboots fail
- Easy to disable or modify timing

### Visibility
- Comprehensive logging of entire process
- Clear timestamps for each phase
- Easy troubleshooting if issues occur

This gives you the "wake up to fresh lab" experience with minimal complexity, leveraging your existing infrastructure!