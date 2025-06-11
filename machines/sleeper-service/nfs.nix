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

  # Enable RPC services for NFS
  services.rpcbind.enable = true;

  # NFSv4 ID mapping service
  services.nfs.idmapd = {
    enable = true;
    settings = {
      General = {
        Domain = "home.lab"; # Same domain on all machines
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

    # Export the storage directory (ZFS dataset)
    # Allow access from both local network and Tailscale network
    # Using layered security approach with different permission models
    exports = ''
      # Main storage - root squashed for security, crossmnt for subdirectories
      /mnt/storage 10.0.0.0/24(rw,sync,no_subtree_check,crossmnt,root_squash) 100.64.0.0/10(rw,sync,no_subtree_check,crossmnt,root_squash)

      # Media directory - accessible to media group, root squashed
      /mnt/storage/media 10.0.0.0/24(rw,sync,no_subtree_check,root_squash) 100.64.0.0/10(rw,sync,no_subtree_check,root_squash)

      # Downloads - all users squashed to media group for simplified permissions
      /mnt/storage/downloads 10.0.0.0/24(rw,sync,no_subtree_check,all_squash,anonuid=993,anongid=993) 100.64.0.0/10(rw,sync,no_subtree_check,all_squash,anonuid=993,anongid=993)

      # Backups - admin access only from specific trusted hosts
      /mnt/storage/backups 10.0.0.0/24(rw,sync,no_subtree_check,root_squash) 100.64.0.0/10(ro,sync,no_subtree_check,root_squash)

      # Shares - public access via media group
      /mnt/storage/shares 10.0.0.0/24(rw,sync,no_subtree_check,all_squash,anonuid=993,anongid=993) 100.64.0.0/10(rw,sync,no_subtree_check,all_squash,anonuid=993,anongid=993)
    '';
    # Don't create mount points automatically since they already exist
    createMountPoints = false;
  };

  # Ensure the storage subdirectories exist with proper ownership (ZFS dataset is mounted at /mnt/storage)
  # Using setgid bit (2xxx) for proper group inheritance on new files/directories
  # Note: /mnt/storage/media is a ZFS dataset mount point, so we don't create it with tmpfiles
  systemd.tmpfiles.rules = [
    # /mnt/storage/media is handled by ZFS mounting, not tmpfiles
    "d /mnt/storage/downloads 2775 media media -" # Owned by media group
    "d /mnt/storage/backups 0750 root root -" # Admin only, restricted access
    "d /mnt/storage/shares 2775 media media -" # Public access via media group
  ];

  # Set permissions on the ZFS-mounted media dataset after it's mounted
  # This ensures proper ownership even though it's a ZFS mount point
  systemd.services.nfs-server.serviceConfig.ExecStartPost = [
    "${pkgs.coreutils}/bin/chown root:media /mnt/storage/media"
    "${pkgs.coreutils}/bin/chmod 2775 /mnt/storage/media"
  ];

  # Performance tuning for NFS
  boot.kernel.sysctl = {
    # Network buffer optimizations
    "net.core.rmem_max" = 134217728;
    "net.core.wmem_max" = 134217728;
    "net.ipv4.tcp_rmem" = "4096 65536 134217728";
    "net.ipv4.tcp_wmem" = "4096 65536 134217728";

    # NFS-specific optimizations
    "fs.nfs.nlm_tcpport" = 32768;
    "fs.nfs.nlm_udpport" = 32768;
  };

  # Required packages for NFS
  environment.systemPackages = with pkgs; [
    nfs-utils
  ];

  # Firewall configuration for NFS services
  networking.firewall = {
    allowedTCPPorts = [
      111 # portmapper (rpcbind)
      2049 # nfsd
      32768 # lockd
    ];
    allowedUDPPorts = [
      111 # portmapper (rpcbind)
      2049 # nfsd
      32768 # lockd
    ];
  };
}
