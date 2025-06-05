# NFS Server Configuration
# Network File System server for home lab storage
{ config, pkgs, ... }:

{
  # NFS server configuration
  services.nfs.server = {
    enable = true;
    # Export the storage directory
    exports = ''
      /mnt/storage    10.0.0.0/24(rw,sync,no_subtree_check,no_root_squash)
    '';
    # Create exports on startup
    createMountPoints = true;
  };

  # Ensure the storage directory exists
  systemd.tmpfiles.rules = [
    "d /mnt/storage 0755 geir users -"
    "d /mnt/storage/media 0755 geir users -"
    "d /mnt/storage/downloads 0755 geir users -"
    "d /mnt/storage/backups 0755 geir users -"
  ];

  # Required packages for NFS
  environment.systemPackages = with pkgs; [
    nfs-utils
  ];

  # Firewall rules are already configured in network module
}
