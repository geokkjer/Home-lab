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

### 2. Lab Tool Commands
Add new commands to the existing lab tool:

```python
# lab/commands/update_system.py
class UpdateSystemCommand:
    def __init__(self, lab_config):
        self.lab_config = lab_config
        self.flake_path = lab_config.get('flake_path', '/home/geir/Home-lab')
    
    def update_self(self):
        """Update the current system using Nix flake"""
        try:
            # Update flake inputs
            self._run_command(['nix', 'flake', 'update'], cwd=self.flake_path)
            
            # Rebuild system
            hostname = self._get_hostname()
            self._run_command([
                'nixos-rebuild', 'switch', 
                '--flake', f'{self.flake_path}#{hostname}'
            ])
            
            print("System updated successfully")
            return True
            
        except Exception as e:
            print(f"Update failed: {e}")
            return False
    
    def schedule_reboot(self, delay_minutes=1):
        """Schedule a system reboot"""
        self._run_command(['shutdown', '-r', f'+{delay_minutes}'])
        
    def _get_hostname(self):
        import socket
        return socket.gethostname()
        
    def _run_command(self, cmd, cwd=None):
        import subprocess
        result = subprocess.run(cmd, cwd=cwd, check=True, 
                              capture_output=True, text=True)
        return result.stdout
```

### 3. CLI Integration
Extend the main lab tool CLI:

```python
# lab/cli.py (additions)
@cli.group()
def update():
    """System update commands"""
    pass

@update.command('system')
@click.option('--self', 'update_self', is_flag=True, 
              help='Update the current system')
@click.option('--reboot', is_flag=True, 
              help='Reboot after update')
def update_system(update_self, reboot):
    """Update system using Nix flake"""
    if update_self:
        updater = UpdateSystemCommand(config)
        success = updater.update_self()
        
        if success and reboot:
            updater.schedule_reboot()
```

### 4. Simple Configuration
Add update settings to lab configuration:

```yaml
# lab.yaml (additions)
auto_update:
  enabled: true
  schedule: "02:00"
  auto_reboot: true
  flake_path: "/home/geir/Home-lab"
  log_retention_days: 30
```

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