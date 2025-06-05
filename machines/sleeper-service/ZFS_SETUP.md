# ZFS Setup for sleeper-service

## Overview
sleeper-service now uses ZFS for enhanced data integrity, snapshots, and efficient storage management for the file server role.

## ZFS Pool Structure

### `filepool` - Main ZFS Pool
This pool contains all system and storage datasets:

```
filepool/root         # Root filesystem (/)
filepool/nix          # Nix store (/nix)  
filepool/var          # Variable data (/var)
filepool/storage      # NFS export storage (/mnt/storage)
```

## Storage Layout

### System Datasets
- **filepool/root**: System root filesystem with snapshots for rollback
- **filepool/nix**: Nix store, can be excluded from frequent snapshots
- **filepool/var**: System logs and variable data

### Storage Dataset  
- **filepool/storage**: Primary NFS export point containing:
  - `media/` - Media files shared via NFS
  - `downloads/` - Download directory (for Transmission when re-enabled)
  - `backups/` - Backup storage
  - `shares/` - General file shares

## ZFS Features Enabled

### Automatic Services
- **Auto-scrub**: Weekly integrity checks of all data
- **TRIM**: SSD optimization for supported drives
- **Snapshots**: Automatic snapshots for data protection (to be configured)

### Benefits for File Server
1. **Data Integrity**: Checksumming protects against bit rot
2. **Snapshots**: Point-in-time recovery for user data
3. **Compression**: Efficient storage usage
4. **Send/Receive**: Efficient backup to other ZFS systems
5. **Share Management**: Native NFS sharing support

## Deployment Notes

### Before First Boot
The actual ZFS pool creation needs to be done during installation:

```bash
# Example pool creation (adjust device names)
zpool create -f filepool /dev/sda
zfs create filepool/root
zfs create filepool/nix  
zfs create filepool/var
zfs create filepool/storage

# Set mount points
zfs set mountpoint=/ filepool/root
zfs set mountpoint=/nix filepool/nix
zfs set mountpoint=/var filepool/var  
zfs set mountpoint=/mnt/storage filepool/storage

# Enable compression for storage dataset
zfs set compression=lz4 filepool/storage
```

### Network Storage Integration
The `/mnt/storage` ZFS dataset is exported via NFS to the home lab network (10.0.0.0/24), replacing the previous "files.home" server functionality.

## Migration from Existing Setup
When deploying to the physical server:
1. Backup existing data from current file server
2. Create ZFS pool on target drives
3. Restore data to `/mnt/storage`
4. Update client machines to mount from new IP (10.0.0.8)

## Culture Reference
Like the GSV *Sleeper Service*, this configuration operates quietly in the background, providing reliable storage services with the redundancy and self-healing capabilities that ZFS brings to the table.
