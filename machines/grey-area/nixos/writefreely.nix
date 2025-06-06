{ pkgs, configs, ... }:
{
  services.writefreely = {
    enable = true;
    admin.name = "geir@geokkjer.eu";
    host = "blog.geokkjer.eu";
    database = {
      type = "sqlite3";
      #filename = "writefreely.db";
      #database = "writefreely";
    };
    nginx = {
      # Enable Nginx and configure it to serve WriteFreely.
      enable = true;
    };
    settings = {
      server = {
        port = 8088;
        bind = "0.0.0.0";
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 8088 ];
  networking.firewall.allowedUDPPorts = [ 8088 ];
}
