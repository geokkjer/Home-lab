# Reverse Proxy Services

## Migration Guide: Nginx → Caddy

This guide will help you migrate your reverse proxy from Nginx to Caddy on NixOS.

### Why Caddy?

**Advantages:**

- **Automatic HTTPS**: Built-in automatic TLS certificate management (no separate ACME config needed)
- **Simpler Configuration**: More intuitive syntax compared to Nginx
- **Zero-config HTTPS**: Certificates are obtained and renewed automatically
- **Modern Features**: HTTP/3, automatic HTTPS redirects, and more out of the box
- **Better Error Messages**: More helpful debugging information

**Trade-offs:**

- Slightly higher memory usage than Nginx
- Less mature ecosystem for some edge cases
- Different configuration paradigm (may require learning curve)

---

### Current Nginx Configuration

Your current setup includes:

- **HTTPS reverse proxy** for `git.geokkjer.eu` → `http://grey-area:3000` (Forgejo/Gitea)
- **SSH stream proxy** on port 2222 → `grey-area:22` (Git SSH access)
- **ACME/Let's Encrypt** integration for automatic SSL certificates

---

### Migration Steps

#### Step 1: Backup Current Configuration

```bash
# On reverse-proxy machine
sudo nixos-rebuild build
sudo cp -r /etc/nginx /etc/nginx.backup
sudo cp /etc/nixos/configuration.nix /etc/nixos/configuration.nix.backup
```

#### Step 2: Install Caddy Configuration

Replace your nginx configuration block in `/machines/reverse-proxy/configuration.nix`:

**Remove:**

```nix
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
    };

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

  security.acme = {
    acceptTerms = true;
    defaults = {
      email = "geir@geokkjer.eu";
    };
  };
```

**Add:**

```nix
  # Caddy reverse proxy
  services.caddy = {
    enable = true;
    
    email = "geir@geokkjer.eu";  # For ACME/Let's Encrypt
    
    virtualHosts = {
      # Forgejo/Gitea reverse proxy
      "git.geokkjer.eu" = {
        extraConfig = ''
          reverse_proxy http://grey-area:3000
        '';
      };
      
      # Add more services as needed
      # "cloud.geokkjer.eu" = {
      #   extraConfig = ''
      #     reverse_proxy http://grey-area:80
      #   '';
      # };
    };
  };

  # For SSH forwarding, we still need a TCP proxy
  # Caddy can handle this with layer4 plugin, or keep using systemd socket
  systemd.services.ssh-git-proxy = {
    description = "SSH proxy to Git server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    
    serviceConfig = {
      ExecStart = "${pkgs.socat}/bin/socat TCP-LISTEN:2222,fork,reuseaddr TCP:grey-area:22";
      Restart = "always";
      RestartSec = "10s";
    };
  };
```

#### Step 3: Open Required Firewall Ports

Ensure your firewall allows traffic:

```nix
  networking.firewall = {
    allowedTCPPorts = [ 80 443 2222 ];  # HTTP, HTTPS, Git SSH
    allowedUDPPorts = [ 443 ];           # HTTP/3 (QUIC)
  };
```

#### Step 4: Deploy and Test

```bash
# From your local machine
cd /home/geir/Home-lab

# Build configuration to check for errors
nixos-rebuild build --flake .#reverse-proxy --target-host sma@reverse-proxy --use-remote-sudo

# If successful, deploy
nixos-rebuild switch --flake .#reverse-proxy --target-host sma@reverse-proxy --use-remote-sudo
```

#### Step 5: Verify Services

```bash
# Test HTTPS
curl -v https://git.geokkjer.eu

# Test SSH forwarding (from your machine)
ssh -p 2222 git@reverse-proxy.example.com

# Check Caddy status
ssh sma@reverse-proxy "sudo systemctl status caddy"

# Check Caddy logs
ssh sma@reverse-proxy "sudo journalctl -u caddy -f"
```

---

### Advanced Caddy Configuration

#### Multiple Services Example

```nix
services.caddy.virtualHosts = {
  "git.geokkjer.eu" = {
    extraConfig = ''
      reverse_proxy http://grey-area:3000
    '';
  };
  
  "cloud.geokkjer.eu" = {
    extraConfig = ''
      reverse_proxy http://grey-area:80
    '';
  };
  
  "home.geokkjer.eu" = {
    extraConfig = ''
      reverse_proxy http://little-rascal:8080
    '';
  };
};
```

#### Custom Headers and Security

```nix
"git.geokkjer.eu" = {
  extraConfig = ''
    # Security headers
    header {
      Strict-Transport-Security "max-age=31536000; includeSubDomains; preload"
      X-Content-Type-Options "nosniff"
      X-Frame-Options "SAMEORIGIN"
      Referrer-Policy "strict-origin-when-cross-origin"
    }
    
    # Reverse proxy with custom settings
    reverse_proxy http://grey-area:3000 {
      # Preserve original headers
      header_up Host {host}
      header_up X-Real-IP {remote_host}
      header_up X-Forwarded-For {remote_host}
      header_up X-Forwarded-Proto {scheme}
      
      # Timeouts
      timeout 300s
    }
  '';
};
```

#### WebSocket Support

```nix
"app.geokkjer.eu" = {
  extraConfig = ''
    reverse_proxy http://backend:3000 {
      # WebSocket support is automatic in Caddy!
      # But you can be explicit:
      header_up Connection {>Connection}
      header_up Upgrade {>Upgrade}
    }
  '';
};
```

#### Using Caddyfile Directly (Alternative Method)

If you prefer writing a Caddyfile:

```nix
services.caddy = {
  enable = true;
  email = "geir@geokkjer.eu";
  
  configFile = pkgs.writeText "Caddyfile" ''
    git.geokkjer.eu {
      reverse_proxy http://grey-area:3000
    }
    
    cloud.geokkjer.eu {
      reverse_proxy http://grey-area:80
    }
  '';
};
```

---

### SSH Proxy Alternative: Caddy Layer4 Plugin

For a pure Caddy solution (requires Caddy with layer4 plugin):

```nix
services.caddy = {
  enable = true;
  package = pkgs.caddy.withPlugins {
    plugins = ["github.com/mholt/caddy-l4@latest"];
    hash = "...";  # Need to compute hash
  };
  
  globalConfig = ''
    layer4 {
      :2222 {
        route {
          proxy {
            upstream grey-area:22
          }
        }
      }
    }
  '';
};
```

---

### Rollback Plan

If you need to rollback to Nginx:

```bash
# On reverse-proxy machine
sudo nixos-rebuild switch --rollback

# Or restore backup configuration
sudo cp /etc/nixos/configuration.nix.backup /etc/nixos/configuration.nix
sudo nixos-rebuild switch
```

---

### Monitoring and Logs

```bash
# View Caddy logs
sudo journalctl -u caddy -f

# Check certificate status
sudo caddy list-certificates

# View configuration
sudo systemctl cat caddy

# Reload configuration without downtime
sudo systemctl reload caddy
```

---

### Troubleshooting

#### Certificates Not Obtained

```bash
# Check if ports 80/443 are accessible from internet
curl -v http://git.geokkjer.eu

# Check DNS
dig git.geokkjer.eu

# View detailed logs
sudo journalctl -u caddy -n 100 --no-pager
```

#### Service Not Starting

```bash
# Check configuration syntax
sudo caddy validate --config /etc/caddy/Caddyfile

# Check systemd status
sudo systemctl status caddy

# Check if ports are already in use
sudo ss -tlnp | grep -E ':(80|443)'
```

---

### Performance Comparison

After migration, compare:

```bash
# Response time
time curl -s https://git.geokkjer.eu > /dev/null

# Memory usage
ssh sma@reverse-proxy "free -h && ps aux | grep -E '(nginx|caddy)'"

# Connection stats
ab -n 1000 -c 10 https://git.geokkjer.eu/
```

---

### Next Steps

1. ✅ Backup current configuration
2. ✅ Update NixOS configuration
3. ✅ Deploy to reverse-proxy machine
4. ✅ Verify all services are accessible
5. ✅ Update documentation
6. ✅ Monitor for 24-48 hours
7. ✅ Remove nginx backup configuration

---

### Additional Resources

- [Caddy Documentation](https://caddyserver.com/docs/)
- [NixOS Caddy Options](https://search.nixos.org/options?query=services.caddy)
- [Caddy Community Forum](https://caddy.community/)
- [Migration Guide: Nginx to Caddy](https://caddyserver.com/docs/quick-starts/reverse-proxy)
