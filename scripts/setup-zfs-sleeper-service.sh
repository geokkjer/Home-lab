#!/usr/bin/env bash
# ZFS Setup Script for sleeper-service
# This script configures the existing ZFS storage pool for NFS exports

set -euo pipefail

echo "=== ZFS Setup for sleeper-service ==="
echo "This script will configure the existing 'storage' pool for NFS exports"
echo "OS will remain on ext4 - only storage pool will be used for media/NFS"
echo ""
echo "Current ZFS pool status:"
zpool status storage
echo ""
echo "Current datasets:"
zfs list
echo ""
echo "The existing storage/media dataset with 903GB of data will be preserved"
echo "We'll set up proper mount points for NFS exports"
echo ""
read -p "Are you sure you want to proceed? (yes/no): " confirm

if [[ "$confirm" != "yes" ]]; then
    echo "Aborted."
    exit 1
fi

echo ""
echo "=== Step 1: Verifying ZFS tools ==="
if ! command -v zpool &> /dev/null; then
    echo "ERROR: ZFS tools not found!"
    exit 1
fi

echo ""
echo "=== Step 2: Checking existing pool ==="
if ! zpool status storage &> /dev/null; then
    echo "ERROR: Storage pool not found!"
    exit 1
fi

echo "Storage pool found. GUID: $(zpool get -H -o value guid storage)"

echo ""
echo "=== Step 3: Setting up storage mount points ==="

# Create mount point directory
echo "Creating /mnt/storage directory..."
mkdir -p /mnt/storage

# Set proper mount point for storage pool
echo "Setting mount point for storage pool..."
zfs set mountpoint=/mnt/storage storage

# Ensure media dataset has proper mountpoint
echo "Setting mount point for media dataset..."
zfs set mountpoint=/mnt/storage/media storage/media

# Create additional directories if needed
echo "Creating additional storage directories..."
mkdir -p /mnt/storage/{downloads,backups,shares}

# Set proper ownership for sma user
echo "Setting ownership for sma user..."
chown sma:users /mnt/storage/{media,downloads,backups,shares}

echo ""
echo "=== Step 4: Summary ==="
echo "ZFS storage setup complete!"
echo ""
echo "Storage pool: $(zpool get -H -o value guid storage)"
echo "Mount point: /mnt/storage"
echo "Media data: /mnt/storage/media (preserved)"
echo "Additional directories: downloads, backups, shares"
echo ""
echo "The existing 903GB of media data has been preserved."
echo "NFS exports can now use /mnt/storage/* paths."
echo ""
echo "Next: Deploy NixOS configuration to enable ZFS on boot"

echo ""
echo "=== ZFS Setup Complete! ==="
echo "Pool status:"
zpool status storage
echo ""
echo "Datasets:"
zfs list
echo ""
echo "You can now deploy the new NixOS configuration that uses ZFS."
echo "Note: The system will need to be rebooted after the deployment."
echo ""
echo "Next steps:"
echo "1. Copy the new Home-lab configuration to the server"
echo "2. Run: sudo nixos-rebuild boot --flake .#sleeper-service"
echo "3. Reboot the system to activate ZFS support"
