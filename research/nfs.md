# NFS Best Practices and Troubleshooting Guide

## Overview

Network File System (NFS) is a distributed file system protocol that allows remote file access over a network. This document outlines best practices for configuring NFS in a home lab environment, focusing on permission management, security, and performance optimization.

## Current Configuration Analysis

### Identified Issues

1. **Missing User ID Mapping**: The current configuration doesn't ensure consistent UIDs/GIDs between server and clients
2. **Security Concerns**: Using `no_root_squash` poses security risks
3. **No ID Mapping Configuration**: NFSv4 ID mapping is not configured
4. **Missing Export Options**: Several important security and performance options are not set

## NFS Permission Management Best Practices

### 1. User and Group ID Consistency

**Problem**: NFS relies on UID/GID matching between server and clients. If a user has UID 1000 on the server but UID 1001 on the client, permission issues will occur.

**Solutions**:

#### Option A: Synchronized UIDs/GIDs (Recommended for Home Lab)
- Ensure the `media` group has the same GID (993) on all machines
- Create users with consistent UIDs across all systems
- Use centralized user management (LDAP/AD) for larger setups

#### Option B: NFSv4 ID Mapping
Configure NFSv4 ID mapping to translate between different UID/GID spaces:

```nix
# On both server and clients
services.rpcbind.enable = true;
services.nfs.idmapd = {
  enable = true;
  settings = {
    General = {
      Domain = "home.lab";  # Same domain on all machines
      Verbosity = 0;
    };
    Mapping = {
      Nobody-User = "nobody";
      Nobody-Group = "nogroup";
    };
  };
};
```

#### Option C: Use all_squash for Public Shares
For truly shared directories where ownership doesn't matter:

```nix
exports = ''
  /mnt/storage/shares 10.0.0.0/24(rw,sync,all_squash,anonuid=993,anongid=993)
'';
```

### 2. Security Best Practices

#### Root Squashing (Critical)
**Current Issue**: Using `no_root_squash` allows root on clients to access files as root on the server.

**Fix**: Use root squashing by default and only disable when absolutely necessary:

```nix
exports = ''
  # Default: root_squash is enabled (secure)
  /mnt/storage/media 10.0.0.0/24(rw,sync,no_subtree_check,root_squash)
  
  # Only for trusted admin workstations
  /mnt/storage/backups 10.0.0.100(rw,sync,no_subtree_check,no_root_squash)
'';
```

#### Port Security
Always use the `secure` option (enabled by default) to restrict access to privileged ports:

```nix
exports = ''
  /mnt/storage/media 10.0.0.0/24(rw,sync,secure,no_subtree_check)
'';
```

#### Client Mount Security
On client systems, use security options:

```bash
# Mount with nosuid to prevent privilege escalation
mount -t nfs -o nosuid,nodev server:/mnt/storage/media /mnt/media

# In /etc/fstab
server:/mnt/storage/media /mnt/media nfs nosuid,nodev,rw,hard,timeo=600 0 0
```

### 3. Export Options Explained

#### Permission and Security Options
- `root_squash`: Map root UID to anonymous user (default, secure)
- `no_root_squash`: Allow root access (use carefully)
- `all_squash`: Map all users to anonymous user
- `anonuid=N`: Set anonymous user UID
- `anongid=N`: Set anonymous group GID

#### Performance Options
- `sync`: Writes are committed before responding (safer, slower)
- `async`: Writes may be cached (faster, less safe)
- `no_wdelay`: Don't delay writes (for small random writes)

#### Access Control Options
- `rw`: Read-write access
- `ro`: Read-only access
- `secure`: Only accept requests from privileged ports (default)
- `insecure`: Accept requests from any port

#### Filesystem Options
- `no_subtree_check`: Don't verify file is in exported subtree (recommended)
- `subtree_check`: Verify file location (slight security benefit, performance cost)

## Recommended NFS Configuration

### Server Configuration (nfs.nix)

```nix
{
  config,
  pkgs,
  ...
}: {
  imports = [
    ../../modules/users/media-group.nix
  ];

  # NFSv4 ID mapping
  services.rpcbind.enable = true;
  services.nfs.idmapd = {
    enable = true;
    settings = {
      General = {
        Domain = "home.lab";
        Verbosity = 0;
      };
      Mapping = {
        Nobody-User = "nobody";
        Nobody-Group = "nogroup";
      };
    };
  };

  # NFS server configuration
  services.nfs.server = {
    enable = true;
    # Optimized exports with proper security
    exports = ''
      # Main storage - root squashed for security
      /mnt/storage 10.0.0.0/24(rw,sync,no_subtree_check,crossmnt) 100.64.0.0/10(rw,sync,no_subtree_check,crossmnt)
      
      # Media directory - accessible to media group
      /mnt/storage/media 10.0.0.0/24(rw,sync,no_subtree_check,root_squash) 100.64.0.0/10(rw,sync,no_subtree_check,root_squash)
      
      # Downloads - squash all users to media group
      /mnt/storage/downloads 10.0.0.0/24(rw,sync,no_subtree_check,all_squash,anonuid=993,anongid=993) 100.64.0.0/10(rw,sync,no_subtree_check,all_squash,anonuid=993,anongid=993)
      
      # Backups - admin only with root access
      /mnt/storage/backups 10.0.0.100(rw,sync,no_subtree_check,no_root_squash)
      
      # Public shares - anonymous access
      /mnt/storage/shares 10.0.0.0/24(rw,sync,no_subtree_check,all_squash,anonuid=993,anongid=993) 100.64.0.0/10(ro,sync,no_subtree_check,all_squash)
    '';
    createMountPoints = true;
  };

  # Directory permissions and ownership
  # IMPORTANT: Only create directories that are NOT ZFS mount points with tmpfiles
  # ZFS mount points must have their permissions set after mounting
  systemd.tmpfiles.rules = [
    # Only create non-ZFS directories - media is a ZFS dataset mount point
    "d /mnt/storage/downloads 2775 media media -"  # Owned by media group
    "d /mnt/storage/backups 0750 root root -"      # Admin only
    "d /mnt/storage/shares 2775 media media -"     # Public access via media group
  ];

  # Set permissions on ZFS-mounted datasets after they're mounted
  systemd.services.nfs-server.serviceConfig.ExecStartPost = [
    "${pkgs.coreutils}/bin/chown root:media /mnt/storage/media"
    "${pkgs.coreutils}/bin/chmod 2775 /mnt/storage/media"
  ];

  # Performance tuning
  boot.kernel.sysctl = {
    # Increase NFS server thread count for better performance
    "fs.nfs.nlm_tcpport" = 32768;
    "fs.nfs.nlm_udpport" = 32768;
  };

  # Required packages
  environment.systemPackages = with pkgs; [
    nfs-utils
  ];

  # Firewall configuration
  networking.firewall = {
    allowedTCPPorts = [ 
      111   # portmapper
      2049  # nfsd
      32768 # lockd
    ];
    allowedUDPPorts = [ 
      111   # portmapper
      2049  # nfsd
      32768 # lockd
    ];
  };
}
```

### Client Configuration

```nix
# Enable NFS client services
services.rpcbind.enable = true;
services.nfs.idmapd = {
  enable = true;
  settings = {
    General = {
      Domain = "home.lab";  # Must match server
    };
  };
};

# Example mount in /etc/fstab
# server:/mnt/storage/media /mnt/media nfs4 rw,hard,timeo=600,retrans=5,_netdev,nosuid 0 0
```

## NixOS-Specific NFS Research

### NFS Service Configuration in NixOS

NixOS provides declarative configuration for both NFS servers and clients through the `services.nfs` module. The configuration is highly modular and allows for comprehensive setup of NFS services.

#### Key NixOS NFS Modules and Options

Based on the nixpkgs documentation, NFS configuration in NixOS involves several key components:

1. **Basic NFS Support**:
   ```nix
   boot.supportedFilesystems = [ "nfs" ];
   services.rpcbind.enable = true;
   ```

2. **NFS Server Configuration**:
   - `services.nfs.server.enable` - Enable NFS server
   - `services.nfs.server.exports` - Define export configurations
   - `services.nfs.server.createMountPoints` - Auto-create mount points
   - `services.nfs.server.threads` - Number of NFS daemon threads

3. **NFS Client Configuration**:
   - `services.rpcbind.enable` - Required for NFS client operations
   - `services.nfs.idmapd` - NFSv4 ID mapping service
   - `fileSystems` - Declarative mount point definitions

4. **NFSv4 ID Mapping**:
   ```nix
   services.nfs.idmapd = {
     enable = true;
     settings = {
       General = {
         Domain = "home.lab";
         Verbosity = 0;
       };
       Mapping = {
         Nobody-User = "nobody";
         Nobody-Group = "nogroup";
       };
     };
   };
   ```

#### NixOS Advantages for NFS

1. **Declarative Configuration**: All NFS settings are defined in configuration files, making them reproducible and version-controlled.

2. **Automatic Service Dependencies**: NixOS automatically handles service dependencies (rpcbind, nfsd, mountd, etc.).

3. **Integrated Firewall Management**: Firewall rules can be declared alongside NFS configuration.

4. **Atomic Updates**: Configuration changes are applied atomically, reducing the risk of service disruption.

5. **Rollback Capability**: Previous configurations can be easily restored if issues occur.

### NixOS NFS Implementation Patterns

#### Pattern 1: Single-Host NFS Server
```nix
{
  services.nfs.server = {
    enable = true;
    exports = ''
      /mnt/storage 192.168.1.0/24(rw,sync,no_subtree_check)
    '';
    createMountPoints = true;
  };
  
  networking.firewall.allowedTCPPorts = [ 111 2049 ];
  networking.firewall.allowedUDPPorts = [ 111 2049 ];
}
```

#### Pattern 2: NFS Client with Declarative Mounts
```nix
{
  boot.supportedFilesystems = [ "nfs" ];
  services.rpcbind.enable = true;
  
  fileSystems."/mnt/nfs-share" = {
    device = "server.local:/mnt/storage";
    fsType = "nfs";
    options = [ "rw" "hard" "timeo=600" "_netdev" ];
  };
}
```

#### Pattern 3: High-Security NFS with Kerberos
```nix
{
  security.krb5 = {
    enable = true;
    package = pkgs.krb5;
    settings = {
      libdefaults.default_realm = "HOME.LAB";
      realms."HOME.LAB" = {
        kdc = "kerberos.home.lab";
        admin_server = "kerberos.home.lab";
      };
    };
  };
  
  services.nfs.server = {
    enable = true;
    exports = ''
      /secure/data 192.168.1.0/24(rw,sync,sec=krb5p,no_subtree_check)
    '';
  };
}
```

## Implementation Plan

### Phase 1: Basic NFS Server Setup (Week 1)

#### Objectives
- Set up basic NFS server on `sleeper-service`
- Configure secure exports with proper permission management
- Establish client connectivity from other home lab machines

#### Tasks

**1.1 Update sleeper-service NFS Configuration**
- Location: `machines/sleeper-service/nfs.nix`
- Actions:
  - Enable NFSv4 ID mapping service
  - Configure secure export options
  - Set up proper directory permissions with systemd tmpfiles
  - Add performance tuning parameters

**1.2 Create Media Group Module Enhancement**
- Location: `modules/users/media-group.nix`
- Actions:
  - Ensure consistent GID (993) across all machines
  - Add group-specific directory management
  - Configure proper setgid permissions

**1.3 Client Configuration Module**
- Location: `modules/services/nfs-client.nix` (new)
- Actions:
  - Create reusable NFS client configuration
  - Include NFSv4 ID mapping setup
  - Add common mount options and security settings

**1.4 Update Machine Configurations**
- Locations: 
  - `machines/congenital-optimist/configuration.nix`
  - `machines/grey-area/configuration.nix`
- Actions:
  - Import NFS client module
  - Add declarative mount points for shared storage
  - Configure firewall rules

#### Deliverables
- [ ] Updated `sleeper-service/nfs.nix` with secure configuration
- [ ] New `modules/services/nfs-client.nix` module
- [ ] Enhanced `modules/users/media-group.nix`
- [ ] Client configurations for all machines
- [ ] Documentation updates in this file

### Phase 2: Security Hardening (Week 2)

#### Objectives
- Implement security best practices
- Set up proper access controls
- Configure monitoring and logging

#### Tasks

**2.1 Security Enhancement**
- Actions:
  - Implement root squashing by default
  - Configure all_squash for public shares
  - Set up proper anonymous user mapping
  - Review and minimize export permissions

**2.2 Network Security**
- Actions:
  - Configure UFW/iptables rules for NFS ports
  - Set up VPN-only access for sensitive shares
  - Implement port restrictions (secure option)

**2.3 Monitoring Setup**
- Location: `modules/services/nfs-monitoring.nix` (new)
- Actions:
  - Set up NFS statistics collection
  - Configure log monitoring for access patterns
  - Create alerting for failed mount attempts

#### Deliverables
- [ ] Security-hardened NFS configuration
- [ ] Network security rules
- [ ] Monitoring and alerting system
- [ ] Security audit documentation

### Phase 3: Performance Optimization (Week 3)

#### Objectives
- Optimize NFS performance for home lab workloads
- Implement caching strategies
- Tune network and filesystem parameters

#### Tasks

**3.1 Server Performance Tuning**
- Actions:
  - Increase NFS daemon threads
  - Optimize kernel parameters for NFS
  - Configure appropriate read/write sizes
  - Set up async vs sync based on use case

**3.2 Client Optimization**
- Actions:
  - Configure optimal mount options
  - Set up client-side caching
  - Tune timeout and retry parameters

**3.3 Network Optimization**
- Actions:
  - Optimize TCP window sizes
  - Configure jumbo frames if supported
  - Set up link aggregation if available

#### Deliverables
- [ ] Performance-optimized server configuration
- [ ] Client performance tuning
- [ ] Network optimization settings
- [ ] Performance benchmarking results

### Phase 4: Advanced Features (Week 4)

#### Objectives
- Implement advanced NFS features
- Set up backup and disaster recovery
- Create automation and maintenance tools

#### Tasks

**4.1 NFSv4 Advanced Features**
- Actions:
  - Implement NFSv4 ACLs where appropriate
  - Set up NFSv4 referrals for distributed storage
  - Configure NFSv4 migration support

**4.2 Backup Integration**
- Actions:
  - Set up NFS-aware backup procedures
  - Configure snapshot-based backups for ZFS
  - Implement cross-site backup replication

**4.3 Automation and Maintenance**
- Location: `scripts/nfs-maintenance.sh` (new)
- Actions:
  - Create automated health checks
  - Set up export verification scripts
  - Implement automatic client discovery

#### Deliverables
- [ ] Advanced NFSv4 features configuration
- [ ] Integrated backup solution
- [ ] Automation scripts and tools
- [ ] Maintenance procedures documentation

### Implementation Steps

#### Step 1: Prepare the Environment
```bash
# Clone the repository and create feature branch
cd /home/geir/Home-lab
git checkout -b feature/nfs-implementation

# Create new module directories
mkdir -p modules/services
```

#### Step 2: Create Base NFS Client Module
```nix
# modules/services/nfs-client.nix
{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ../users/media-group.nix
  ];

  config = {
    boot.supportedFilesystems = [ "nfs" ];
    
    services.rpcbind.enable = true;
    services.nfs.idmapd = {
      enable = true;
      settings = {
        General = {
          Domain = "home.lab";
          Verbosity = 0;
        };
      };
    };

    environment.systemPackages = with pkgs; [
      nfs-utils
    ];
  };
}
```

#### Step 3: Update sleeper-service Configuration
Update the existing `machines/sleeper-service/nfs.nix` with the comprehensive configuration shown in the recommended section above.

#### Step 4: Test and Validate
```bash
# Build and test the configuration
sudo nixos-rebuild test

# Verify NFS services
systemctl status nfs-server
exportfs -v

# Test client connectivity
showmount -e sleeper-service
```

#### Step 5: Deploy to Other Machines
```bash
# Update each machine configuration
# Add imports and mount points
# Test connectivity from each client
```

### Risk Mitigation

#### Configuration Backup
- Always backup working configurations before changes
- Use git branches for experimental configurations
- Test changes on non-production machines first

#### Service Dependencies
- Ensure proper service ordering in systemd
- Handle network dependencies correctly
- Plan for graceful degradation if NFS is unavailable

#### Data Protection
- Implement proper backup strategies before enabling NFS
- Use read-only mounts for critical data initially
- Set up monitoring for unauthorized access

### Success Criteria

#### Phase 1 Success Criteria
- [ ] NFS server running on sleeper-service
- [ ] All home lab machines can mount shared storage
- [ ] Proper permissions for media group access
- [ ] Basic security measures in place

#### Phase 2 Success Criteria
- [ ] Security audit passes with no critical issues
- [ ] Monitoring system reports normal operations
- [ ] Access controls prevent unauthorized access

#### Phase 3 Success Criteria
- [ ] Performance benchmarks show acceptable speeds
- [ ] No timeout or connection issues under normal load
- [ ] Network utilization optimized

#### Phase 4 Success Criteria
- [ ] Advanced features working as expected
- [ ] Backup and recovery procedures tested
- [ ] Automation reduces manual maintenance

This implementation plan provides a structured approach to deploying NFS in your NixOS home lab environment, with proper security, performance, and maintainability considerations.

## ZFS and NFS Integration

### Important Considerations

When using ZFS datasets as NFS export points, there are several important considerations:

#### ZFS Mount Points vs tmpfiles.rules

**Critical Issue**: Do not use `systemd.tmpfiles.rules` to create directories that are ZFS dataset mount points. This can cause conflicts and permission issues.

**Example Problem**:
```nix
# WRONG - Don't do this if /mnt/storage/media is a ZFS dataset
systemd.tmpfiles.rules = [
  "d /mnt/storage/media 2775 root media -"  # This conflicts with ZFS mounting
];
```

**Correct Approach**:
```nix
# Only create directories that are NOT ZFS mount points
systemd.tmpfiles.rules = [
  "d /mnt/storage/downloads 2775 media media -"  # Regular directory
  "d /mnt/storage/backups 0750 root root -"      # Regular directory
];

# Set permissions on ZFS mount points after mounting
systemd.services.nfs-server.serviceConfig.ExecStartPost = [
  "${pkgs.coreutils}/bin/chown root:media /mnt/storage/media"
  "${pkgs.coreutils}/bin/chmod 2775 /mnt/storage/media"
];
```

#### ZFS Dataset Structure for NFS

A typical ZFS layout for NFS exports might look like:

```
storage                    # Pool (mounted at /mnt/storage)
├── storage/media         # Dataset (mounted at /mnt/storage/media)
├── storage/backups       # Dataset (mounted at /mnt/storage/backups)
└── Regular directories:  # Created with tmpfiles.rules
    ├── downloads/
    └── shares/
```

#### Service Dependencies

Ensure NFS services start after ZFS mounting:

```nix
systemd.services.nfs-server = {
  after = [ "zfs-mount.service" ];
  wants = [ "zfs-mount.service" ];
};
```

#### ZFS Native NFS Sharing

ZFS supports native NFS sharing, but NixOS typically uses the Linux kernel NFS server. For ZFS native sharing:

```bash
# Enable ZFS native NFS (alternative approach)
zfs set sharenfs="rw=@10.0.0.0/24,root=10.0.0.100" storage/media
```

However, the recommended approach for NixOS is to use the kernel NFS server with ZFS as the underlying filesystem.
