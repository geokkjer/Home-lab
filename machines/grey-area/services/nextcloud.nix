{
  config,
  pkgs,
  ...
}: {
  environment.etc."nextcloud-admin-pass".text = "changemeNOW";
  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud31;
    hostName = "cloud.geokkjer.eu";
    #autoUpdateApps = true;

    #https = true;

    caching.redis = true;
    #database.createLocally = true;
    config = {
      dbtype = "sqlite";
      adminpassFile = "/etc/nextcloud-admin-pass";
    };

    # Nextcloud-specific settings
    settings = {
      trusted_domains = [
        "nextcloud.grey-area"
        "grey-area" # Allow access via short hostname
        "100.109.28.53"
        "100.119.86.92" # Tailscale IP
        "*.tail807ea.ts.net" # Trust all Tailscale domains
      ];
      trusted_proxies = [
        "10.0.0.0/24" # Trust local network range
      ];
    };
  };

  # Open firewall ports for HTTP and HTTPS
  networking.firewall.allowedTCPPorts = [80 443];
}
