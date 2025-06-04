{ config, pkgs, ... }:

{
  services.transmission = {
    enable = true;
    user = "geir";
    group = "users";
    #home = "/mnt/storage/";
    settings.rpc-port = 9091;
    settings.rpc-bind-address = "0.0.0.0";
    #openRPCPort = true;
    downloadDirPermissions = "770";
    settings = {
      download-dir = "/mnt/storage";
      #rpc-whitelist-enabled = true;
      rpc-whitelist = "127.0.0.1,10.0.0.*,100.*.*.*";
      rpc-host-whitelist = "idea,files,nixos-work,server1";
    };
  };
}
