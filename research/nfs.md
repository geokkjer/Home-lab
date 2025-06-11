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
  systemd.tmpfiles.rules = [
    # Media group directories
    "d /mnt/storage/media 2775 root media -"       # Setgid for group inheritance
    "d /mnt/storage/downloads 2775 media media -"  # Owned by media group
    "d /mnt/storage/backups 0750 root root -"      # Admin only
    "d /mnt/storage/shares 2775 media media -"     # Public access via media group
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

## Troubleshooting Common Permission Issues

### 1. "Permission Denied" Errors

**Symptoms**: Users cannot access files they should be able to access

**Diagnosis**:
```bash
# Check UID/GID mapping
id username  # On both client and server

# Check export configuration
exportfs -v

# Check mount options
mount | grep nfs

# Check file permissions
ls -la /mnt/storage/media
```

**Solutions**:
- Ensure UID/GID consistency
- Check export options (root_squash, all_squash)
- Verify group membership
- Check directory setgid bit (2xxx permissions)

### 2. "Operation Not Permitted" for File Operations

**Symptoms**: Can read files but cannot create/modify/delete

**Solutions**:
- Check write permissions in exports (`rw` vs `ro`)
- Verify directory permissions (need write + execute)
- Check if filesystem is mounted read-only
- Ensure no conflicting mount options (`ro`, `nosuid`)

### 3. Files Created with Wrong Ownership

**Symptoms**: New files appear with unexpected UID/GID

**Solutions**:
- Use setgid bit on directories (`chmod g+s /directory`)
- Configure all_squash with appropriate anonuid/anongid
- Set up proper ID mapping
- Use ACLs for complex permission scenarios

### 4. Root Access Issues

**Symptoms**: Root operations fail on NFS mounts

**Solutions**:
- Check if `root_squash` is intended behavior
- Use `no_root_squash` only for trusted admin clients
- Consider using `sudo` on the NFS server instead

## Performance Optimization

### Server Tuning

```nix
# Increase NFS daemon threads
services.nfs.server.threads = 16;

# Kernel parameters for better NFS performance
boot.kernel.sysctl = {
  "net.core.rmem_max" = 134217728;
  "net.core.wmem_max" = 134217728;
  "net.ipv4.tcp_rmem" = "4096 65536 134217728";
  "net.ipv4.tcp_wmem" = "4096 65536 134217728";
};
```

### Client Tuning

```bash
# Mount with performance options
mount -t nfs4 -o rsize=1048576,wsize=1048576,hard,timeo=600 server:/path /mount
```

## Security Considerations

### 1. Network Security
- Use VPN or firewall rules to restrict NFS access
- Consider NFSv4 with Kerberos for authentication
- Enable RPC-with-TLS for encryption (Linux 6.5+)

### 2. File System Security
- Use minimal necessary permissions
- Regular security audits of export configurations
- Monitor NFS access logs
- Implement backup and recovery procedures

### 3. Access Control
```nix
# Example of layered security approach
exports = ''
  # Development - developers only, root squashed
  /srv/dev 192.168.1.0/24(rw,sync,root_squash,no_subtree_check)
  
  # Public - read-only, all users squashed
  /srv/public *(ro,sync,all_squash,no_subtree_check)
  
  # Admin - restricted to specific hosts, root access
  /srv/admin 192.168.1.10(rw,sync,no_root_squash,no_subtree_check)
'';
```

## Monitoring and Maintenance

### Server Monitoring
```bash
# Check NFS statistics
cat /proc/net/rpc/nfsd

# Monitor NFS threads
cat /proc/fs/nfsd/threads

# Check exports
exportfs -v

# Monitor client connections
ss -tuln | grep :2049
```

### Client Monitoring
```bash
# Check mount status
mount | grep nfs

# Monitor NFS statistics
nfsstat -c

# Check for stale handles
dmesg | grep -i nfs
```

## References

- [Linux NFS-HOWTO](https://tldp.org/HOWTO/NFS-HOWTO/)
- [exports(5) Manual Page](https://man7.org/linux/man-pages/man5/exports.5.html)
- [Arch Linux NFS Wiki](https://wiki.archlinux.org/title/NFS)
- [NFS Performance Tuning](https://nfs.sourceforge.net/)
- [Red Hat NFS Documentation](https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/9/html/managing_file_systems/)

## Conclusion

Proper NFS configuration requires careful attention to:
1. User/group ID consistency
2. Appropriate security settings
3. Performance optimization
4. Regular monitoring and maintenance

The recommended configuration provides a good balance of security, performance, and usability for a home lab environment. Always test changes in a development environment before applying to production systems.
