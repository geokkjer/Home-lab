{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [
    ./gandicloud.nix
    ../../modules/common/base.nix
    ../../modules/network/extraHosts.nix
    ../../modules/users/sma.nix
    ../../modules/security/ssh-keys.nix

    # Development (minimal for edge server)
    ../../modules/development/emacs.nix
  ];

  environment.systemPackages = with pkgs; [
    neovim
    fastfetch
    tailscale
  ];

  # Hostname configuration
  networking.hostName = "reverse-proxy";

  # DMZ-specific firewall configuration - simplified for testing
  networking.firewall = {
    enable = true;
    # Allow HTTP/HTTPS from external network and Git SSH on port 2222
    # Temporarily allow SSH from everywhere - rely on fail2ban for protection
    allowedTCPPorts = [22 80 443 2222];
    allowedUDPPorts = [];
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

  # Emacs server configuration (minimal for edge server)
  services.emacs-profiles = {
    enable = true;
    profile = "server";
    enableDaemon = false;
    user = "sma";
  };

  # SSH configuration - temporarily simplified for testing
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
        locations."/".proxyPass = "http://grey-area:3000";
      };
      #"geokkjer.eu" = {
      #  default = true;
      #  forceSSL = true;
      #  enableACME = true;
      #  locations."/".proxyPass = "/var/wwww/homepage/";
      #};
    };

    # Stream configuration for SSH forwarding to Git server
    streamConfig = ''
      upstream git_ssh_backend {
          server grey-area:22;
      }

      server {
          listen 2222;
          proxy_pass git_ssh_backend;
          proxy_timeout 300s;
          proxy_connect_timeout 10s;
          proxy_responses 1;
      }
    '';
  };
  # acme let's encrypt
  security.acme = {
    acceptTerms = true;
    defaults = {
      email = "geir@geokkjer.eu";
    };
  };
}
