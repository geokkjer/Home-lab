{ pkgs, configs, lib, ... }:
let
  Host = "vps1.tail807ea.ts.net";
in
{
  imports = [ 
    ../../modules/common/base.nix
    ../../modules/network/common.nix
    ../../modules/users/sma.nix
    ../../modules/security/ssh-keys.nix
  ];

  environment.systemPackages = with pkgs; [
    neovim curl htop bottom fastfetch
    tailscale git 
  ];

  # Override common.nix firewall settings for security
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 ]; # Only HTTP/HTTPS externally
    allowedUDPPorts = [ ];
    # SSH only allowed on Tailscale interface
    interfaces.tailscale0.allowedTCPPorts = [ 22 ];
  };

  # Security services
  services.fail2ban.enable = true;

  # tailscale
  services.tailscale.enable = true;

  # Hostname configuration
  networking.hostName = "reverse-proxy";
  
  # SSH configuration - only accessible via Tailscale
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = lib.mkForce "no";
      PasswordAuthentication = false;
    };
    listenAddresses = [
      {
        addr = "100.96.189.104";  # Tailscale IP from About.org
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