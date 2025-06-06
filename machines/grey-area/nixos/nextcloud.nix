{ pkgs, ... }:

{
  #  Nextcloud Config
  environment.etc."nextcloud-admin-pass".text = "siKKerhet666";
  services.nextcloud = {
    enable = true;
    hostName = "server1.tail807ea.ts.net";

    # Ssl Let'encrypt
    #hostName = "cloud.geokkjer.eu";
    #https = true;

    # Auto-update Nextcloud Apps
    autoUpdateApps.enable = true;
    # Set what time makes sense for you
    autoUpdateApps.startAt = "05:00:00";
    # enable redis cache
    configureRedis = true;
    # Create db locally , maybe not needed with sqlite
    database.createLocally = true;
    # Config options
    config = {
      dbtype = "sqlite";
      adminpassFile = "/etc/nextcloud-admin-pass";
      trustedProxies = [ "46.226.104.98" "100.75.29.52" ];
      extraTrustedDomains = [ "localhost" "*.cloudflare.net" "*.tail807ea.ts.net" "46.226.104.98" "*.geokkjer.eu" ];
    };
  };
}
