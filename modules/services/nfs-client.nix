# NFS Client Configuration Module
# Provides standardized NFS client setup with ID mapping
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  imports = [
    ../users/media-group.nix
  ];

  config = {
    # Enable NFS filesystem support
    boot.supportedFilesystems = ["nfs"]; # Enable RPC services required for NFS
    services.rpcbind.enable = true;

    # NFSv4 ID mapping service - must match server configuration
    services.nfs.idmapd.settings = {
      General = {
        Domain = "home.lab"; # Must match server domain
        Verbosity = 0;
      };
      Mapping = {
        Nobody-User = "nobody";
        Nobody-Group = "nogroup";
      };
    };

    # NFS utilities for client operations
    environment.systemPackages = with pkgs; [
      nfs-utils
    ];
  };
}
