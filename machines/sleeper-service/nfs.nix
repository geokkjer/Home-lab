# NFS Server Configuration
# Network File System server for home lab storage
{
  config,
  pkgs,
  ...
}: {
  imports = [
    ../../modules/users/media-group.nix
  ];

  # NFS server configuration
  services.nfs.server = {
    enable = true;
    # Export the storage directory (ZFS dataset)
    # Allow access from both local network and Tailscale network
    exports = ''
      /mnt/storage       10.0.0.0/24(rw,sync,no_subtree_check,no_root_squash) 100.64.0.0/10(rw,sync,no_subtree_check,no_root_squash)
      /mnt/storage/media 10.0.0.0/24(rw,sync,no_subtree_check,no_root_squash) 100.64.0.0/10(rw,sync,no_subtree_check,no_root_squash)
    '';
    # Create exports on startup
    createMountPoints = true;
  };

  # Ensure the storage subdirectories exist with proper ownership (ZFS dataset is mounted at /mnt/storage)
  # Setting ownership to root:media with group write permissions for shared access
  systemd.tmpfiles.rules = [
    "d /mnt/storage/media 0775 root media -"
    "d /mnt/storage/downloads 0775 root media -"
    "d /mnt/storage/backups 0775 root media -"
    "d /mnt/storage/shares 0775 root media -"
  ];

  # Required packages for NFS
  environment.systemPackages = with pkgs; [
    nfs-utils
  ];

  # Firewall rules are already configured in network module
}
