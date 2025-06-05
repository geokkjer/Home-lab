{ config, pkgs, ... }:

{
  services.transmission = {
    enable = true;
    user = "geir";
    group = "users";
    settings.rpc-port = 9091;
    settings.rpc-bind-address = "0.0.0.0";
    downloadDirPermissions = "770";
    settings = {
      download-dir = "/mnt/storage/downloads";
      rpc-whitelist = "127.0.0.1,10.0.0.*,100.*.*.*";
      rpc-host-whitelist = "sleeper-service,localhost";
    };
  };
}
