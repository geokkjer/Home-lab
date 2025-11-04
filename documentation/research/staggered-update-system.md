# Staggered Machine Update and Reboot System Research

## Overview

Research into implementing an automated system for updating and rebooting all lab machines in a staggered fashion using Nix, cronjobs, and our existing lab tool infrastructure.

## Goals

- Minimize downtime by updating machines in waves
- Ensure system stability with gradual rollouts
- Leverage Nix's atomic updates and rollback capabilities
- Integrate with existing lab tool for orchestration
- Provide monitoring and failure recovery

## Architecture Components

### 1. Update Controller

- Central orchestrator running on management node
- Maintains machine groups and update schedules
- Coordinates staggered execution
- Monitors update progress and health

### 2. Machine Groups

```
Group 1: Non-critical services (dev environments, testing)
Group 2: Infrastructure services (monitoring, logging)
Group 3: Critical services (databases, core applications)
Group 4: Management nodes (controllers, orchestrators)
```

### 3. Nix Integration

- Use `nixos-rebuild switch` for atomic updates
- Leverage Nix generations for rollback capability
- Update channels/flakes before rebuilding
- Validate configuration before applying

### 4. Lab Tool Integration

- Extend lab tool with update management commands
- Machine inventory and grouping
- Health check integration
- Status reporting and logging

## Implementation Strategy

### Phase 1: Basic Staggered Updates

```bash
# Example workflow per machine group
lab update prepare --group=dev
lab update execute --group=dev --wait-for-completion
lab update verify --group=dev
lab update prepare --group=infrastructure
# Continue with next group...
```

### Phase 2: Enhanced Orchestration

- Dependency-aware scheduling
- Health checks before proceeding to next group
- Automatic rollback on failures
- Notification system

### Phase 3: Advanced Features

- Blue-green deployments for critical services
- Canary releases
- Integration with monitoring systems
- Custom update policies per service

## Cronjob Design

### Master Cron Schedule

```cron
# Weekly full system update - Sundays at 2 AM
0 2 * * 0 /home/geir/Home-lab/scripts/staggered-update.sh

# Daily security updates for critical machines
0 3 * * * /home/geir/Home-lab/scripts/security-update.sh --group=critical

# Health check and cleanup
0 1 * * * /home/geir/Home-lab/scripts/update-health-check.sh
```

### Update Script Structure

```bash
#!/usr/bin/env bash
# staggered-update.sh

set -euo pipefail

# Configuration
GROUPS=("dev" "infrastructure" "critical" "management")
STAGGER_DELAY=30m
MAX_PARALLEL=3

# Log setup
LOG_DIR="/var/log/lab-updates"
LOG_FILE="$LOG_DIR/update-$(date +%Y%m%d-%H%M%S).log"

exec > >(tee -a "$LOG_FILE") 2>&1

for group in "${GROUPS[@]}"; do
    echo "Starting update for group: $group"
    
    # Pre-update checks
    lab health-check --group="$group" || {
        echo "Health check failed for $group, skipping"
        continue
    }
    
    # Update Nix channels/flakes
    lab update prepare --group="$group"
    
    # Execute updates with parallelism control
    lab update execute --group="$group" --parallel="$MAX_PARALLEL"
    
    # Verify updates
    lab update verify --group="$group" || {
        echo "Verification failed for $group, initiating rollback"
        lab update rollback --group="$group"
        # Send alert
        lab notify --level=error --message="Update failed for $group, rolled back"
        exit 1
    }
    
    echo "Group $group updated successfully, waiting $STAGGER_DELAY"
    sleep "$STAGGER_DELAY"
done

echo "All groups updated successfully"
lab notify --level=info --message="Staggered update completed successfully"
```

## Nix Configuration Management

### Centralized Configuration

```nix
# /home/geir/Home-lab/nix/update-config.nix
{
  updateGroups = {
    dev = {
      machines = [ "dev-01" "dev-02" "test-env" ];
      updatePolicy = "aggressive";
      maintenanceWindow = "02:00-06:00";
      allowReboot = true;
    };
    
    infrastructure = {
      machines = [ "monitor-01" "log-server" "backup-01" ];
      updatePolicy = "conservative";
      maintenanceWindow = "03:00-05:00";
      allowReboot = true;
      dependencies = [ "dev" ];
    };
    
    critical = {
      machines = [ "db-primary" "web-01" "web-02" ];
      updatePolicy = "manual-approval";
      maintenanceWindow = "04:00-05:00";
      allowReboot = false;  # Requires manual reboot
      dependencies = [ "infrastructure" ];
    };
  };
  
  updateSettings = {
    maxParallel = 3;
    healthCheckTimeout = 300;
    rollbackOnFailure = true;
    notificationChannels = [ "email" "discord" ];
  };
}
```

### Machine-Specific Update Configurations

```nix
# On each machine: /etc/nixos/update-config.nix
{
  services.lab-updater = {
    enable = true;
    group = "infrastructure";
    preUpdateScript = ''
      # Stop non-critical services
      systemctl stop some-service
    '';
    postUpdateScript = ''
      # Restart services and verify
      systemctl start some-service
      curl -f http://localhost:8080/health
    '';
    rollbackScript = ''
      # Custom rollback procedures
      systemctl stop some-service
      nixos-rebuild switch --rollback
      systemctl start some-service
    '';
  };
}
```

## Lab Tool Extensions

### New Commands

```bash
# Update management
lab update prepare [--group=GROUP] [--machine=MACHINE]
lab update execute [--group=GROUP] [--parallel=N] [--dry-run]
lab update verify [--group=GROUP]
lab update rollback [--group=GROUP] [--to-generation=N]
lab update status [--group=GROUP]

# Health and monitoring
lab health-check [--group=GROUP] [--timeout=SECONDS]
lab update-history [--group=GROUP] [--days=N]
lab notify [--level=LEVEL] [--message=MSG] [--channel=CHANNEL]

# Configuration
lab update-config show [--group=GROUP]
lab update-config set [--group=GROUP] [--key=KEY] [--value=VALUE]
```

### Integration Points

```python
# lab/commands/update.py
class UpdateCommand:
    def prepare(self, group=None, machine=None):
        """Prepare machines for updates"""
        # Update Nix channels/flakes
        # Pre-update health checks
        # Download packages
        
    def execute(self, group=None, parallel=1, dry_run=False):
        """Execute updates on machines"""
        # Run nixos-rebuild switch
        # Monitor progress
        # Handle failures
        
    def verify(self, group=None):
        """Verify updates completed successfully"""
        # Check system health
        # Verify services
        # Compare generations
```

## Monitoring and Alerting

### Health Checks

- Service availability checks
- Resource usage monitoring
- System log analysis
- Network connectivity tests

### Alerting Triggers

- Update failures
- Health check failures
- Rollback events
- Long-running updates

### Notification Channels

- Email notifications
- Discord/Slack integration
- Dashboard updates
- Log aggregation

## Safety Mechanisms

### Pre-Update Validation

- Configuration syntax checking
- Dependency verification
- Resource availability checks
- Backup verification

### During Update

- Progress monitoring
- Timeout handling
- Partial failure recovery
- Emergency stop capability

### Post-Update

- Service verification
- Performance monitoring
- Automatic rollback triggers
- Success confirmation

## Rollback Strategy

### Automatic Rollback Triggers

- Health check failures
- Service startup failures
- Critical error detection
- Timeout exceeded

### Manual Rollback

```bash
# Quick rollback to previous generation
lab update rollback --group=critical --immediate

# Rollback to specific generation
lab update rollback --group=infrastructure --to-generation=150

# Selective rollback (specific machines)
lab update rollback --machine=db-primary,web-01
```

## Testing Strategy

### Development Environment

- Test updates in isolated environment
- Validate scripts and configurations
- Performance testing
- Failure scenario testing

### Staging Rollout

- Deploy to staging group first
- Automated testing suite
- Manual verification
- Production deployment

## Security Considerations

- Secure communication channels
- Authentication for update commands
- Audit logging
- Access control for update scripts
- Encrypted configuration storage

## Future Enhancements

### Advanced Scheduling

- Maintenance window management
- Business hour awareness
- Holiday scheduling
- Emergency update capabilities

### Intelligence Features

- Machine learning for optimal timing
- Predictive failure detection
- Automatic dependency discovery
- Performance impact analysis

### Integration Expansions

- CI/CD pipeline integration
- Cloud provider APIs
- Container orchestration
- Configuration management systems

## Implementation Roadmap

### Phase 1 (2 weeks)

- Basic staggered update script
- Simple group management
- Nix integration
- Basic health checks

### Phase 2 (4 weeks)

- Lab tool integration
- Advanced scheduling
- Monitoring and alerting
- Rollback mechanisms

### Phase 3 (6 weeks)

- Advanced features
- Performance optimization
- Extended integrations
- Documentation and training

## Resources and References

- NixOS Manual: System Administration
- Cron Best Practices
- Blue-Green Deployment Patterns
- Infrastructure as Code Principles
- Monitoring and Observability Patterns

## Conclusion

A well-designed staggered update system will significantly improve lab maintenance efficiency while reducing risk. The combination of Nix's atomic updates, careful orchestration, and comprehensive monitoring provides a robust foundation for automated infrastructure management.
