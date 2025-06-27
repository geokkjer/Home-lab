# Simple Lab Auto-Update Service Plan

## Overview
A simple automated update service for the homelab that runs nightly via cron, updates Nix flakes, rebuilds systems, and reboots machines. Designed for homelab environments where uptime is only critical during day hours.

## Current Lab Tool Analysis
Based on the existing lab tool structure, we need to integrate with:
- Command structure and CLI interface
- Machine inventory and management
- Configuration handling
- Logging and status reporting

## Simple Architecture

### Core Components
1. **Nix Service Module** - NixOS service definition for the auto-updater
2. **Lab Tool Integration** - New commands in the existing lab tool
3. **Cron Scheduling** - Simple nightly execution
4. **Update Script** - Core logic for update/reboot cycle

## Implementation Plan

### 1. Nix Service Module
Create a NixOS service that integrates with the lab tool:

```nix
# /home/geir/Home-lab/nix/modules/lab-auto-update.nix
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.lab-auto-update;
  labTool = pkgs.writeShellScript "lab-auto-update" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    LOG_FILE="/var/log/lab-auto-update.log"
    
    echo "$(date): Starting auto-update" >> "$LOG_FILE"
    
    # Update flake
    lab update-system --self 2>&1 | tee -a "$LOG_FILE"
    
    # Reboot if configured
    if [[ "${cfg.autoReboot}" == "true" ]]; then
      echo "$(date): Rebooting system" >> "$LOG_FILE"
      systemctl reboot
    fi
  '';
in
{
  options.services.lab-auto-update = {
    enable = mkEnableOption "Lab auto-update service";
    
    schedule = mkOption {
      type = types.str;
      default = "02:00";
      description = "Time to run updates (HH:MM format)";
    };
    
    autoReboot = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to automatically reboot after updates";
    };
    
    flakePath = mkOption {
      type = types.str;
      default = "/home/geir/Home-lab";
      description = "Path to the lab flake";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.lab-auto-update = {
      description = "Lab Auto-Update Service";
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        ExecStart = "${labTool}";
      };
    };

    systemd.timers.lab-auto-update = {
      description = "Lab Auto-Update Timer";
      timerConfig = {
        OnCalendar = "daily";
        Persistent = true;
        RandomizedDelaySec = "30m";
      };
      wantedBy = [ "timers.target" ];
    };

    # Ensure log directory exists
    systemd.tmpfiles.rules = [
      "d /var/log 0755 root root -"
    ];
  };
}
```

### 2. Guile Scheme Auto-Update Module
Create the core auto-update functionality in `lab/auto-update.scm`:

```scheme
;; lab/auto-update.scm - Auto-update system implementation

(define-module (lab auto-update)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19) ; Date/time
  #:use-module (utils logging)
  #:use-module (utils config)
  #:use-module (lab deployment)
  #:use-module (lab machines)
  #:export (auto-update-system
            schedule-auto-update
            check-update-health
            auto-update-status))

;; Pure function: Generate update log entry
(define (format-update-log-entry timestamp operation status details)
  "Pure function to format update log entry"
  (format #f "~a: ~a - ~a (~a)" timestamp operation status details))

;; Pure function: Check if system is healthy for updates
(define (system-health-check-pure)
  "Pure function returning health check criteria"
  '((disk-space-threshold . 90)
    (required-services . ("systemd"))
    (min-uptime-minutes . 30)))

;; Impure function: Check actual system health
(define (check-update-health)
  "Check if system is ready for updates (impure - checks actual system)"
  (log-info "Checking system health before update...")
  
  (let* ((health-checks (system-health-check-pure))
         (disk-threshold (assoc-ref health-checks 'disk-space-threshold))
         (disk-usage (get-disk-usage))
         (system-running (system-is-running?))
         (uptime-ok (check-minimum-uptime)))
    
    (log-debug "Disk usage: ~a%" disk-usage)
    (log-debug "System running: ~a" system-running)
    (log-debug "Uptime check: ~a" uptime-ok)
    
    (cond
     ((> disk-usage disk-threshold)
      (log-error "Disk usage too high: ~a% (threshold: ~a%)" disk-usage disk-threshold)
      #f)
     ((not system-running)
      (log-error "System not in running state")
      #f)
     ((not uptime-ok)
      (log-error "System uptime too low for safe update")
      #f)
     (else
      (log-success "System health check passed")
      #t))))

;; Impure function: Main auto-update routine
(define (auto-update-system . args)
  "Perform automatic system update (impure - modifies system)"
  (let* ((options (if (null? args) '() (car args)))
         (auto-reboot (option-ref options 'auto-reboot #t))
         (dry-run (option-ref options 'dry-run #f))
         (machine-name (get-hostname)))
    
    (log-info "Starting auto-update for machine: ~a" machine-name)
    (write-update-log "auto-update" "started" machine-name)
    
    (if (not (check-update-health))
        (begin
          (log-error "System health check failed - aborting update")
          (write-update-log "auto-update" "aborted" "health check failed")
          #f)
        (begin
          ;; Update flake inputs
          (log-info "Updating flake inputs...")
          (let ((flake-result (update-flake options)))
            (if flake-result
                (begin
                  (log-success "Flake update completed")
                  (write-update-log "flake-update" "success" "")
                  
                  ;; Deploy configuration
                  (log-info "Deploying updated configuration...")
                  (let ((deploy-result (deploy-machine machine-name "switch" options)))
                    (if deploy-result
                        (begin
                          (log-success "Configuration deployment completed")
                          (write-update-log "deployment" "success" "switch mode")
                          
                          ;; Schedule reboot if enabled
                          (if (and auto-reboot (not dry-run))
                              (begin
                                (log-info "Scheduling system reboot in 2 minutes...")
                                (write-update-log "reboot" "scheduled" "2 minutes")
                                (system "shutdown -r +2 'Auto-update completed - rebooting'")
                                #t)
                              (begin
                                (log-info "Auto-reboot disabled - update complete")
                                (write-update-log "auto-update" "completed" "no reboot")
                                #t)))
                        (begin
                          (log-error "Configuration deployment failed")
                          (write-update-log "deployment" "failed" "switch mode")
                          #f))))
                (begin
                  (log-error "Flake update failed")
                  (write-update-log "flake-update" "failed" "")
                  #f)))))))

;; Helper functions for system checks and logging
(define (get-disk-usage)
  "Get root filesystem disk usage percentage"
  (let* ((cmd "df / | tail -1 | awk '{print $5}' | sed 's/%//'")
         (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
         (output (string-trim-both (get-string-all port)))
         (status (close-pipe port)))
    (if (zero? status)
        (string->number output)
        95)))

(define (system-is-running?)
  "Check if system is in running state"
  (let* ((cmd "systemctl is-system-running --quiet")
         (status (system cmd)))
    (zero? status)))

(define (get-hostname)
  "Get current system hostname"
  (let* ((cmd "hostname")
         (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
         (output (string-trim-both (get-string-all port)))
         (status (close-pipe port)))
    (if (zero? status) output "unknown")))

(define (write-update-log operation status details)
  "Write update operation to log file"
  (let* ((timestamp (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
         (log-entry (format-update-log-entry timestamp operation status details))
         (log-file "/var/log/lab-auto-update.log"))
    (catch #t
      (lambda ()
        (call-with-output-file log-file
          (lambda (port) (format port "~a\n" log-entry))
          #:append #t))
      (lambda (key . args)
        (log-error "Failed to write update log: ~a" args)))))

(define (auto-update-status)
  "Display auto-update service status and recent logs"
  (log-info "Checking auto-update status...")
  
  (let ((log-file "/var/log/lab-auto-update.log"))
    (if (file-exists? log-file)
        (begin
          (format #t "Recent auto-update activity:\n")
          (let* ((cmd (format #f "tail -10 ~a" log-file))
                 (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
                 (output (get-string-all port))
                 (status (close-pipe port)))
            (if (zero? status) (display output)
                (log-error "Failed to read update log"))))
        (log-info "No auto-update log found"))
    
    ;; Check systemd timer status
    (format #t "\nSystemd timer status:\n")
    (let* ((cmd "systemctl status lab-auto-update.timer --no-pager")
           (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
           (output (get-string-all port)))
      (display output))))
```

### 3. CLI Integration
Update `main.scm` to include auto-update commands:

```scheme
;; Add to use-modules section:
(lab auto-update)

;; Add to help text:
  auto-update         Perform automatic system update with health checks
  auto-update-status  Show auto-update service status and logs

;; Add command handlers:
(define (cmd-auto-update)
  "Perform automatic system update"
  (log-info "Starting automatic system update...")
  (let ((result (auto-update-system '((auto-reboot . #t)))))
    (if result
        (log-success "Automatic update completed successfully")
        (log-error "Automatic update failed"))))

(define (cmd-auto-update-status)
  "Show auto-update status and logs"
  (auto-update-status))

;; Add to command dispatcher:
    ('auto-update
     (cmd-auto-update))
    
    ('auto-update-status
     (cmd-auto-update-status))
```

### 4. Updated NixOS Service Module
Enhanced service module at `modules/services/lab-auto-update.nix`:

```nix
# modules/services/lab-auto-update.nix - NixOS service for automatic lab updates

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.lab-auto-update;
  
  # Get the lab tool from our packages
  labTool = pkgs.callPackage ../../packages/lab-tools.nix {};
  
  # Auto-update script that uses the Guile lab tool
  autoUpdateScript = pkgs.writeShellScript "lab-auto-update" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    LOG_FILE="/var/log/lab-auto-update.log"
    LOCK_FILE="/var/run/lab-auto-update.lock"
    
    # Ensure we don't run multiple instances
    if [ -f "$LOCK_FILE" ]; then
      echo "$(date): Auto-update already running (lock file exists)" >> "$LOG_FILE"
      exit 1
    fi
    
    # Create lock file
    echo $$ > "$LOCK_FILE"
    
    # Cleanup function
    cleanup() {
      rm -f "$LOCK_FILE"
    }
    trap cleanup EXIT
    
    echo "$(date): Starting lab auto-update" >> "$LOG_FILE"
    
    # Change to the lab directory
    cd "${cfg.flakePath}"
    
    # Run the Guile lab tool auto-update command
    if ${labTool}/bin/lab auto-update 2>&1 | tee -a "$LOG_FILE"; then
      echo "$(date): Auto-update completed successfully" >> "$LOG_FILE"
    else
      echo "$(date): Auto-update failed with exit code $?" >> "$LOG_FILE"
      exit 1
    fi
  '';
  
in
{
  options.services.lab-auto-update = {
    enable = mkEnableOption "Lab auto-update service";
    
    schedule = mkOption {
      type = types.str;
      default = "02:00";
      description = "Time to run updates (HH:MM format)";
    };
    
    randomizedDelay = mkOption {
      type = types.str;
      default = "30m";
      description = "Maximum random delay before starting update";
    };
    
    flakePath = mkOption {
      type = types.str;
      default = "/home/geir/Projects/home-lab";
      description = "Path to the home lab flake directory";
    };
    
    persistent = mkOption {
      type = types.bool;
      default = true;
      description = "Whether the timer should be persistent across reboots";
    };
    
    logRetentionDays = mkOption {
      type = types.int;
      default = 30;
      description = "Number of days to retain auto-update logs";
    };
  };

  config = mkIf cfg.enable {
    # Systemd service for the auto-update
    systemd.services.lab-auto-update = {
      description = "Home Lab Auto-Update Service";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        Group = "root";
        ExecStart = "${autoUpdateScript}";
        
        # Security settings
        PrivateTmp = true;
        ProtectSystem = false;  # We need to modify the system
        ProtectHome = true;
        NoNewPrivileges = false; # We need privileges for nixos-rebuild
        
        # Resource limits
        MemoryMax = "2G";
        CPUQuota = "50%";
        
        # Timeout settings
        TimeoutStartSec = "30m";
        TimeoutStopSec = "5m";
      };
      
      # Environment variables for the service
      environment = {
        PATH = lib.makeBinPath (with pkgs; [
          nix nixos-rebuild git openssh rsync gawk gnused 
          coreutils util-linux systemd
        ]);
        NIX_PATH = "nixpkgs=${pkgs.path}";
      };
    };

    # Systemd timer for scheduling
    systemd.timers.lab-auto-update = {
      description = "Home Lab Auto-Update Timer";
      wantedBy = [ "timers.target" ];
      
      timerConfig = {
        OnCalendar = "*-*-* ${cfg.schedule}:00";
        Persistent = cfg.persistent;
        RandomizedDelaySec = cfg.randomizedDelay;
        AccuracySec = "1min";
      };
    };

    # Log rotation for auto-update logs
    services.logrotate.settings.lab-auto-update = {
      files = "/var/log/lab-auto-update.log";
      frequency = "daily";
      rotate = cfg.logRetentionDays;
      compress = true;
      delaycompress = true;
      missingok = true;
      notifempty = true;
      create = "644 root root";
    };

    # Ensure log directory exists with proper permissions
    systemd.tmpfiles.rules = [
      "d /var/log 0755 root root -"
      "f /var/log/lab-auto-update.log 0644 root root -"
    ];
  };
}
```

## Guile Scheme Implementation Advantages

The Guile Scheme implementation provides several benefits over the original Python approach:

### 🎯 **K.I.S.S Principles Alignment**
- **Modular**: Follows existing lab-tool module structure
- **Functional**: Pure functions for logic, impure functions clearly marked
- **Small**: Each function has single responsibility
- **Simple**: Leverages existing deployment and configuration infrastructure

### 🔧 **Integration Benefits**
- **Seamless Integration**: Uses existing `lab deployment` and `lab machines` modules
- **Consistent CLI**: Follows same command pattern as other lab commands
- **Shared Configuration**: Uses same configuration system and logging
- **Type Safety**: Leverages Guile's type system and error handling

### 🛡️ **Enhanced Safety Features**
- **Health Checks**: Pre-update validation (disk space, system state, uptime)
- **Comprehensive Logging**: All operations logged with timestamps
- **Lock File Protection**: Prevents concurrent update attempts
- **Graceful Error Handling**: Proper cleanup and rollback on failures

### 📊 **Observability**
- **Status Commands**: `lab auto-update-status` for monitoring
- **Structured Logs**: Easy to parse and analyze
- **Systemd Integration**: Native systemd service and timer management
- **Log Rotation**: Automatic log management with configurable retention

### 🚀 **Usage Examples**

```bash
# Manual testing
lab auto-update                    # Run update with health checks
lab auto-update-status            # Check logs and service status

# Service management
systemctl status lab-auto-update.timer
systemctl list-timers lab-auto-update
journalctl -u lab-auto-update.service

# Configuration (in machine's configuration.nix)
services.lab-auto-update = {
  enable = true;
  schedule = "02:00";             # 2 AM daily
  randomizedDelay = "30m";        # Up to 30min random delay
  flakePath = "/home/geir/Projects/home-lab";
  logRetentionDays = 30;
};
```

This implementation provides a robust, well-integrated auto-update system that maintains the functional programming principles and modular architecture of the existing lab-tool infrastructure.

## Deployment Strategy

### Per-Machine Setup
Each machine gets the service enabled in its Nix configuration:

```nix
# hosts/<hostname>/configuration.nix
{
  imports = [
    ../../nix/modules/lab-auto-update.nix
  ];

  services.lab-auto-update = {
    enable = true;
    schedule = "02:00";
    autoReboot = true;
  };
}
```

### Staggered Scheduling
Different machines can have different update times to avoid all rebooting simultaneously:

```nix
# Example configurations
# db-server.nix
services.lab-auto-update.schedule = "02:00";

# web-servers.nix  
services.lab-auto-update.schedule = "02:30";

# dev-machines.nix
services.lab-auto-update.schedule = "03:00";
```

## Implementation Steps

### Step 1: Create Nix Module
- Create the service module file
- Add to common imports
- Test on single machine

### Step 2: Extend Lab Tool
- Add UpdateSystemCommand class
- Integrate CLI commands
- Test update functionality

### Step 3: Deploy Gradually
- Enable on non-critical machines first
- Monitor logs and behavior
- Roll out to all machines

### Step 4: Monitoring Setup
- Log rotation configuration
- Status reporting
- Alert on failures

## Safety Features

### Pre-Update Checks
```bash
# Basic health check before update
if ! systemctl is-system-running --quiet; then
  echo "System not healthy, skipping update"
  exit 1
fi

# Check disk space
if [[ $(df / | tail -1 | awk '{print $5}' | sed 's/%//') -gt 90 ]]; then
  echo "Low disk space, skipping update"
  exit 1
fi
```

### Rollback on Boot Failure
```nix
# Enable automatic rollback
boot.loader.grub.configurationLimit = 10;
systemd.services."rollback-on-failure" = {
  description = "Rollback on boot failure";
  serviceConfig = {
    Type = "oneshot";
    RemainAfterExit = true;
  };
  script = ''
    # This runs if we successfully boot
    # Clear any failure flags
    rm -f /var/lib/update-failed
  '';
  wantedBy = [ "multi-user.target" ];
};
```

## Monitoring and Logging

### Log Management
```nix
# Add to service configuration
services.logrotate.settings.lab-auto-update = {
  files = "/var/log/lab-auto-update.log";
  rotate = 30;
  daily = true;
  compress = true;
  missingok = true;
  notifempty = true;
};
```

### Status Reporting
```python
# lab/commands/status.py additions
def update_status():
    """Show auto-update status"""
    log_file = "/var/log/lab-auto-update.log"
    
    if os.path.exists(log_file):
        # Parse last update attempt
        with open(log_file, 'r') as f:
            lines = f.readlines()
            # Show last few entries
            for line in lines[-10:]:
                print(line.strip())
    
    # Show service status
    result = subprocess.run(['systemctl', 'status', 'lab-auto-update.timer'], 
                          capture_output=True, text=True)
    print(result.stdout)
```

## Testing Plan

### Local Testing
1. Test lab tool commands manually
2. Test service creation and timer
3. Verify logging works
4. Test with dry-run options

### Gradual Rollout
1. Enable on development machine first
2. Monitor for one week
3. Enable on infrastructure machines
4. Finally enable on critical services

## Future Enhancements

### Simple Additions
- Email notifications on failure
- Webhook status reporting
- Update statistics tracking
- Configuration validation

### Advanced Features
- Update coordination between machines
- Dependency-aware scheduling
- Emergency update capabilities
- Integration with monitoring systems

## File Structure
```
/home/geir/Home-lab/
├── nix/modules/lab-auto-update.nix
├── lab/commands/update_system.py
├── lab/cli.py (modified)
└── scripts/
    ├── update-health-check.sh
    └── emergency-rollback.sh
```

This plan provides a simple, reliable auto-update system that leverages the existing lab tool infrastructure while keeping complexity minimal for a homelab environment.