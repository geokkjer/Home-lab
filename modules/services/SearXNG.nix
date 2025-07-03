# SearXNG Service Configuration for Home Lab
#
# This module provides SearXNG configuration for private metasearch engine
# SearXNG aggregates results from various search engines while preserving privacy
#
# Features:
# - Uses reverse proxy for outbound traffic to search engines
# - Web UI only accessible from Tailscale network
# - No logging of user queries for privacy
# - Nginx reverse proxy with security headers
# - Rate limiting and security hardening
#
# Usage:
# In your machine's configuration.nix, add:
#   imports = [ ../../modules/services/SearXNG.nix ];
#   services.searxng-lab = {
#     enable = true;
#     hostName = "searxng.your-domain.com";
#     reverseProxyHost = "reverse-proxy";
#   };
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.searxng-lab;

  # Generate a unique secret key for this instance
  secretKeyFile = pkgs.writeText "searxng-secret" (builtins.hashString "sha256" cfg.hostName);

  # SearXNG settings configuration
  searxngSettings = {
    use_default_settings = true;

    server = {
      port = cfg.port;
      bind_address = "127.0.0.1";
      secret_key = "@SECRET_KEY@"; # Will be replaced at runtime
      base_url = "http://${cfg.hostName}";
      image_proxy = true;
      public_instance = false;
      limiter = true;
      method = "POST";

      default_http_headers = {
        X-Content-Type-Options = "nosniff";
        X-Download-Options = "noopen";
        X-Robots-Tag = "noindex, nofollow";
        Referrer-Policy = "no-referrer";
        X-Frame-Options = "DENY";
        X-XSS-Protection = "1; mode=block";
      };
    };

    general = {
      debug = false;
      instance_name = "Private Search - ${cfg.hostName}";
      privacypolicy_url = false;
      donation_url = false;
      contact_url = false;
      enable_metrics = false;
    };

    search = {
      safe_search = 0;
      autocomplete = "";
      default_lang = "en";
      ban_time_on_fail = 5;
      max_ban_time_on_fail = 120;
      formats = ["html" "json"];
    };

    ui = {
      static_use_hash = true;
      default_locale = "en";
      query_in_title = false;
      infinite_scroll = false;
      center_alignment = false;
      default_theme = "simple";
      hotkeys = "default";
      search_on_category_select = true;
    };

    # Configure outbound requests through reverse proxy
    outgoing = {
      request_timeout = 3.0;
      useragent_suffix = "";
      pool_connections = 100;
      pool_maxsize = 10;
      enable_http2 = true;
      using_tor_proxy = false;

      # Use reverse proxy for all outbound requests
      proxies = {
        http = "http://${cfg.reverseProxyHost}:${toString cfg.reverseProxyPort}";
        https = "http://${cfg.reverseProxyHost}:${toString cfg.reverseProxyPort}";
      };
    };
  };

  # Convert settings to YAML format
  settingsFile = pkgs.writeText "searxng-settings.yml" (lib.generators.toYAML {} searxngSettings);
in {
  options.services.searxng-lab = {
    enable = lib.mkEnableOption "SearXNG metasearch engine for home lab";

    hostName = lib.mkOption {
      type = lib.types.str;
      default = "searxng.local";
      example = "search.example.com";
      description = "Hostname for the SearXNG instance";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8888;
      description = "Port for SearXNG to listen on";
    };

    reverseProxyHost = lib.mkOption {
      type = lib.types.str;
      default = "reverse-proxy";
      example = "proxy.internal.lan";
      description = "Hostname of the reverse proxy for outbound traffic";
    };

    reverseProxyPort = lib.mkOption {
      type = lib.types.port;
      default = 3128;
      description = "Port of the reverse proxy for outbound traffic";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to open firewall for HTTP access";
    };

    tailscaleOnly = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to restrict access to Tailscale network only";
    };

    nginxVhost = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to create Nginx virtual host";
    };
  };

  config = lib.mkIf cfg.enable {
    # Create SearXNG user and group
    users.users.searxng = {
      isSystemUser = true;
      group = "searxng";
      home = "/var/lib/searxng";
      createHome = true;
      description = "SearXNG metasearch engine user";
    };

    users.groups.searxng = {};

    # Install SearXNG package
    environment.systemPackages = with pkgs; [
      searxng
    ];

    # Create configuration directory and files
    environment.etc."searxng/settings.yml" = {
      source = settingsFile;
      mode = "0644";
    };

    # SearXNG systemd service
    systemd.services.searxng = {
      description = "SearXNG metasearch engine";
      after = ["network-online.target"];
      wants = ["network-online.target"];
      wantedBy = ["multi-user.target"];

      environment = {
        SEARXNG_SETTINGS_PATH = "/etc/searxng/settings.yml";
        PYTHONPATH = "${pkgs.searxng.pythonPath}";
      };

      serviceConfig = {
        Type = "exec";
        User = "searxng";
        Group = "searxng";
        ExecStart = "${pkgs.searxng}/bin/searxng-run";
        Restart = "always";
        RestartSec = "10s";

        # Security hardening
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = ["/var/lib/searxng" "/tmp"];
        RestrictAddressFamilies = ["AF_INET" "AF_INET6"];

        # Process limits
        LimitNOFILE = 4096;
        MemoryMax = "512M";

        # Network restrictions for Tailscale-only access
        IPAddressDeny = lib.mkIf cfg.tailscaleOnly "any";
        IPAddressAllow = lib.mkIf cfg.tailscaleOnly [
          "100.0.0.0/8" # Tailscale network
          "127.0.0.0/8" # Localhost
          "::1/128" # IPv6 localhost
        ];
      };

      preStart = ''
        # Generate secret key and update settings
        SECRET_KEY=$(${pkgs.openssl}/bin/openssl rand -hex 32)
        ${pkgs.gnused}/bin/sed -i "s/@SECRET_KEY@/$SECRET_KEY/g" /etc/searxng/settings.yml

        # Ensure proper permissions
        mkdir -p /var/lib/searxng/cache
        chown -R searxng:searxng /var/lib/searxng
      '';
    };

    # Nginx reverse proxy configuration
    services.nginx = lib.mkIf cfg.nginxVhost {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      virtualHosts."${cfg.hostName}" = {
        listen = [
          {
            addr = "0.0.0.0";
            port = 80;
          }
        ];

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.port}";

          extraConfig = ''
            # Proxy headers
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;

            # Hide server information
            proxy_hide_header X-Powered-By;
            proxy_hide_header Server;

            ${lib.optionalString cfg.tailscaleOnly ''
              # Restrict access to Tailscale network only
              allow 100.0.0.0/8;     # Tailscale network
              allow 127.0.0.1;       # Localhost
              allow ::1;             # IPv6 localhost
              deny all;              # Deny all other access
            ''}
          '';
        };

        # Security headers for all responses
        extraConfig = ''
          # Security headers
          add_header X-Frame-Options DENY always;
          add_header X-Content-Type-Options nosniff always;
          add_header X-XSS-Protection "1; mode=block" always;
          add_header Referrer-Policy "no-referrer" always;
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;

          # Content Security Policy for SearXNG
          add_header Content-Security-Policy "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'; img-src 'self' data: https:; font-src 'self'; connect-src 'self'; frame-ancestors 'none'; form-action 'self';" always;

          # Hide server information
          server_tokens off;
          more_clear_headers Server;
        '';
      };
    };

    # Firewall configuration
    networking.firewall = lib.mkIf cfg.openFirewall {
      allowedTCPPorts = [80];

      # Tailscale-specific firewall rules
      extraCommands = lib.optionalString cfg.tailscaleOnly ''
        # Allow Tailscale network access to HTTP
        iptables -I nixos-fw -i tailscale0 -p tcp --dport 80 -j ACCEPT
        iptables -I nixos-fw -s 100.0.0.0/8 -p tcp --dport 80 -j ACCEPT
      '';

      extraStopCommands = lib.optionalString cfg.tailscaleOnly ''
        # Clean up Tailscale rules
        iptables -D nixos-fw -i tailscale0 -p tcp --dport 80 -j ACCEPT 2>/dev/null || true
        iptables -D nixos-fw -s 100.0.0.0/8 -p tcp --dport 80 -j ACCEPT 2>/dev/null || true
      '';
    };

    # Ensure required directories exist
    systemd.tmpfiles.rules = [
      "d /var/lib/searxng 0755 searxng searxng -"
      "d /var/lib/searxng/cache 0755 searxng searxng -"
    ];

    # Add any additional packages needed
    environment.systemPackages = with pkgs; [
      curl # For health checks
      jq # For JSON processing if needed
    ];
  };
}
