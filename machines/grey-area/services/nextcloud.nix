{
  config,
  pkgs,
  ...
}: {
  envvironment.etc."nextcloud-admin-pass".text = "changemeNOW";
  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud32;
    hostName = "nextcloud.grey-area";
    autoUpdateApps = true;

    #https = true;

    caching.redis = true;
    database.createLocally = true;
    config = {
      dbtype = "sqlite";
      adminpassFile = "/etc/nextcloud-admin-pass";
    };
  };

  # Open firewall ports for HTTP and HTTPS
  networking.firewall.allowedTCPPorts = [80 443];
}
