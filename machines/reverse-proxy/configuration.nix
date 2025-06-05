{ pkgs, configs, lib, ... }:
let
  Host = "vps1.tail807ea.ts.net";
in
{
  imports = [ 
    ../../modules/common/base.nix
    ../../modules/users/sma.nix
    ../../modules/security/ssh-keys.nix
  ];

  environment.systemPackages = with pkgs; [
    neovim curl htop bottom fastfetch
    tailscale git 
  ];

  # Hostname configuration
  networking.hostName = "reverse-proxy";

  # DMZ-specific firewall configuration - very restrictive
  networking.firewall = {
    enable = true;
    # Only allow HTTP/HTTPS from external network
    allowedTCPPorts = [ 80 443 ];
    allowedUDPPorts = [ ];
    # SSH only allowed on Tailscale interface (DMZ security)
    interfaces.tailscale0.allowedTCPPorts = [ 22 ];
    # Explicitly block all other traffic
    rejectPackets = true;
  };

  # Security services
  services.fail2ban = {
    enable = true;
    # Extra aggressive settings for DMZ
    bantime = "24h";
    maxretry = 3;
  };

  # Tailscale for secure management access
  services.tailscale.enable = true;

  # SSH configuration - ONLY accessible via Tailscale (DMZ security)
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = lib.mkForce "no";
      PasswordAuthentication = false;
      PubkeyAuthentication = true;
      AuthenticationMethods = "publickey";
      MaxAuthTries = 3;
      ClientAliveInterval = 300;
      ClientAliveCountMax = 2;
    };
    listenAddresses = [
      {
        addr = "100.96.189.104";  # Tailscale IP only
        port = 22;
      }
    ];
  };
  
  # nginx reverse proxy
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      "git.geokkjer.eu" = {
        addSSL = true;
        enableACME = true;
        locations."/".proxyPass = "http://apps:3000";
      };
      #"geokkjer.eu" = {
      #  default = true;
      #  forceSSL = true;
      #  enableACME = true;
      #  locations."/".proxyPass = "/var/wwww/homepage/";
      #};
    };
  };
  # acme let's encrypt
  security.acme = {
    acceptTerms = true;
    defaults = {
    email = "geir@geokkjer.eu";
    };
  };
}