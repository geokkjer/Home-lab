{ config, pkgs, ... }:

{
  imports = [
    ../../../modules/users/media-group.nix
  ];

  services.transmission = {
    package = pkgs.transmission_4;
    enable = true;
    user = "sma";  # Using admin user for server processes
    group = "media";
    settings.rpc-port = 9091;
    settings.rpc-bind-address = "0.0.0.0";
    downloadDirPermissions = "775";
    settings = {
      download-dir = "/mnt/storage/downloads";
      rpc-whitelist = "127.0.0.1,10.0.0.*,100.*.*.*";
      rpc-host-whitelist = "sleeper-service,localhost,congenital-optimist";
    };
  };
  
  # Downloads directory ownership will be handled by NFS module tmpfiles rules
  # Removed duplicate tmpfiles rule since NFS module already creates this directory
}
