{ config, pkgs, ... }:

{
  services.transmission = {
    package = pkgs.transmission_4;
    enable = true;
    user = "sma";  # Using admin user for server processes
    group = "users";
    settings.rpc-port = 9091;
    settings.rpc-bind-address = "0.0.0.0";
    downloadDirPermissions = "770";
    settings = {
      download-dir = "/mnt/storage/downloads";
      rpc-whitelist = "127.0.0.1,10.0.0.*,100.*.*.*";
      rpc-host-whitelist = "sleeper-service,localhost,congenital-optimist";
    };
  };
  
  # Ensure downloads directory exists even without Transmission
  systemd.tmpfiles.rules = [
    "d /mnt/storage/downloads 0755 sma users -"
  ];
}
