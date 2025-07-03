# SSH Deployment Strategy - Unified sma User Approach

## Overview

This document outlines the updated SSH deployment strategy for the home lab, standardizing on the `sma` user for all administrative operations and deployments.

## User Strategy

### sma User (System Administrator)
- **Purpose**: System administration, deployment, maintenance
- **SSH Key**: `id_ed25519_admin` 
- **Privileges**: sudo NOPASSWD, wheel group
- **Usage**: All lab tool deployments, system maintenance

### geir User (Developer)
- **Purpose**: Development work, daily usage, git operations
- **SSH Key**: `id_ed25519_dev`
- **Privileges**: Standard user with development tools
- **Usage**: Development workflows, git operations

## Deployment Workflow

### From Any Machine (Workstation or Laptop)

1. **Both machines have sma user configured** with admin SSH key
2. **Lab tool uses sma user consistently** for all remote operations
3. **Deploy-rs uses sma user** for automated deployments with rollback

### SSH Configuration

The SSH configuration supports both direct access patterns:

```bash
# Direct Tailscale access with sma user
ssh sma@sleeper-service.tail807ea.ts.net
ssh sma@grey-area.tail807ea.ts.net
ssh sma@reverse-proxy.tail807ea.ts.net
ssh sma@little-rascal.tail807ea.ts.net

# Local sma user (for deployment from laptop to workstation)
ssh sma@localhost
```

## Lab Tool Commands

All lab commands now work consistently from both machines:

```bash
# Status checking
lab status                    # Works from both workstation and laptop

# Deployment (using sma user automatically)
lab deploy sleeper-service    # Works from both machines
lab deploy grey-area         # Works from both machines
lab deploy little-rascal     # Deploy TO laptop FROM workstation
lab deploy congenital-optimist # Deploy TO workstation FROM laptop

# Deploy-rs (with automatic rollback)
lab deploy-rs sleeper-service
lab hybrid-update all
```

## Security Benefits

1. **Principle of Least Privilege**: sma user only for admin tasks
2. **Key Separation**: Admin and development keys are separate
3. **Consistent Access**: Same user across all machines for deployment
4. **Audit Trail**: Clear separation between admin and development activities

## Machine-Specific Notes

### congenital-optimist (Workstation)
- **Type**: Local deployment
- **SSH**: Uses localhost with sma user for consistency
- **Primary Use**: Development and deployment hub

### little-rascal (Laptop)  
- **Type**: Remote deployment
- **SSH**: Tailscale hostname with sma user
- **Primary Use**: Mobile development and deployment

### Remote Servers (sleeper-service, grey-area, reverse-proxy)
- **Type**: Remote deployment
- **SSH**: Tailscale hostnames with sma user
- **Access**: Both workstation and laptop can deploy

## Migration Benefits

1. **Simplified Workflow**: Same commands work from both machines
2. **Better Security**: Dedicated admin user for all system operations
3. **Consistency**: All deployments use the same SSH user pattern
4. **Flexibility**: Can deploy from either workstation or laptop seamlessly

## Testing the Setup

```bash
# Test SSH connectivity with sma user
ssh sma@sleeper-service.tail807ea.ts.net echo "Connection OK"
ssh sma@grey-area.tail807ea.ts.net echo "Connection OK"
ssh sma@little-rascal.tail807ea.ts.net echo "Connection OK"

# Test lab tool
lab status                    # Should show all machines
lab deploy sleeper-service    # Should work with sma user

# Test deploy-rs
lab deploy-rs sleeper-service --dry-run
```

## Implementation Status

- ✅ SSH keys configured for sma user on all machines
- ✅ Lab tool updated to use sma user for all operations
- ✅ Deploy-rs configuration updated to use sma user
- ✅ SSH client configuration updated with proper host patterns
- 📋 Ready for testing and validation

## Next Steps

1. Test SSH connectivity from both machines to all targets
2. Validate lab tool deployment commands
3. Test deploy-rs functionality with sma user
4. Update any remaining scripts that might use old SSH patterns
