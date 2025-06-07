# NFS Server Configuration
# Network File System server for home lab storage
{ config, pkgs, ... }:

{
  # NFS server configuration
  services.nfs.server = {
    enable = true;
    # Export the storage directory (ZFS dataset)
    # Allow access from both local network and Tailscale network
    exports = ''
      /mnt/storage    10.0.0.0/24(rw,sync,no_subtree_check,no_root_squash) 100.64.0.0/10(rw,sync,no_subtree_check,no_root_squash)
    '';
    # Create exports on startup
    createMountPoints = true;
  };

  # Ensure the storage subdirectories exist (ZFS dataset is mounted at /mnt/storage)
  systemd.tmpfiles.rules = [
    "d /mnt/storage/media 0755 sma users -"
    "d /mnt/storage/downloads 0755 sma users -"
    "d /mnt/storage/backups 0755 sma users -"
    "d /mnt/storage/shares 0755 sma users -"
  ];

  # Required packages for NFS
  environment.systemPackages = with pkgs; [
    nfs-utils
  ];

  # Firewall rules are already configured in network module
}
