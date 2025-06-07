{ pkgs, config, lib, ... }:

{
  imports = [ 
    ./gandicloud.nix
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
    # Allow HTTP/HTTPS from external network and Git SSH on port 1337
    allowedTCPPorts = [ 80 443 1337 ];
    allowedUDPPorts = [ ];
    # SSH only allowed from Tailscale network (100.64.0.0/10)
    extraCommands = ''
      # Allow SSH only from Tailscale network
      iptables -A nixos-fw -p tcp --dport 22 -s 100.64.0.0/10 -j ACCEPT
      iptables -A nixos-fw -p tcp --dport 22 -j DROP
    '';
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
    # Let SSH listen on default port, firewall restricts to Tailscale interface
    # This allows Tailscale to assign IP dynamically based on hostname
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

    # Stream configuration for SSH forwarding to Git server
    streamConfig = ''
      upstream git_ssh_backend {
          server apps:22;
      }
      
      server {
          listen 1337;
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