# SearXNG Service Configuration Guide

## Overview

This module provides a secure, privacy-focused SearXNG metasearch engine configuration designed for home lab environments. Key features include:

- **Privacy-first**: No query logging, private instance settings
- **Network Security**: Only accessible from Tailscale VPN network
- **Reverse Proxy**: Uses your reverse-proxy machine for outbound traffic
- **Security Hardening**: Systemd security restrictions, Content Security Policy headers
- **Easy Configuration**: Simple NixOS module with sensible defaults

## Architecture

```
[Client on Tailscale] 
    ↓ (HTTP/HTTPS)
[SearXNG Service] 
    ↓ (Search requests via HTTP proxy)
[Reverse Proxy] 
    ↓ (Outbound to internet)
[Search Engines]
```

## Configuration

### Basic Setup

Add to your machine's `configuration.nix`:

```nix
{
  imports = [ ../../modules/services/SearXNG.nix ];
  
  services.searxng-lab = {
    enable = true;
    hostName = "searxng.your-lab.internal";
    reverseProxyHost = "reverse-proxy";  # Your reverse proxy hostname
  };
}
```

### Advanced Configuration

```nix
{
  services.searxng-lab = {
    enable = true;
    hostName = "search.lab.local";
    port = 8888;                    # SearXNG backend port
    reverseProxyHost = "proxy.lab.local";
    reverseProxyPort = 3128;        # HTTP proxy port
    openFirewall = true;            # Open port 80 for HTTP access
    tailscaleOnly = true;           # Restrict to Tailscale network
    nginxVhost = true;              # Create Nginx reverse proxy
  };
}
```

## Reverse Proxy Configuration

Your reverse-proxy machine needs to provide HTTP proxy functionality. Add this to your reverse-proxy configuration:

```nix
# On reverse-proxy machine
services.squid = {
  enable = true;
  configText = ''
    # Basic proxy configuration
    http_port 3128
    
    # Allow connections from your home lab network
    acl homelab src 192.168.1.0/24
    acl homelab src 100.0.0.0/8  # Tailscale network
    
    # Allow CONNECT for HTTPS
    acl CONNECT method CONNECT
    acl SSL_ports port 443
    
    # Access rules
    http_access allow homelab
    http_access deny all
    
    # Don't log for privacy
    access_log none
    
    # Hide client IP from destination servers
    forwarded_for delete
  '';
};

# Open firewall for proxy
networking.firewall.allowedTCPPorts = [ 3128 ];
```

## Network Access

### Tailscale Setup

Ensure Tailscale is configured on both the SearXNG host and client machines:

```nix
# On SearXNG host and client machines
services.tailscale.enable = true;
```

### DNS Configuration

Add hostname resolution to your machines:

```nix
networking.hosts = {
  "100.x.x.x" = [ "searxng.lab.local" ];  # Replace with actual Tailscale IP
};
```

## Security Features

### Network Restrictions

- Only accepts connections from Tailscale network (100.0.0.0/8)
- Nginx access controls deny non-Tailscale traffic
- Systemd IPAddressAllow/Deny for service-level restrictions

### Privacy Protection

- No query logging enabled
- Private instance (not listed publicly)
- Rate limiting to prevent abuse
- Secure headers (CSP, HSTS, etc.)

### Systemd Hardening

- NoNewPrivileges: Prevents privilege escalation
- PrivateTmp: Isolated temporary directory
- ProtectSystem: Read-only system directories
- RestrictAddressFamilies: Only IPv4/IPv6 allowed

## Usage

1. **Deploy the Configuration**:

   ```bash
   nixos-rebuild switch
   ```

2. **Check Service Status**:

   ```bash
   systemctl status searxng
   systemctl status nginx
   ```

3. **Access the Interface**:
   Navigate to `http://searxng.lab.local` from a Tailscale-connected device

4. **Test Search**:
   Try searching for something to verify it works and uses the proxy

## Troubleshooting

### Check Service Logs

```bash
journalctl -u searxng -f
journalctl -u nginx -f
```

### Test Network Connectivity

```bash
# Test if SearXNG is running
curl -s http://localhost:8888 | head

# Test proxy connectivity from SearXNG host
curl -x http://reverse-proxy:3128 http://httpbin.org/ip

# Test Tailscale connectivity
ping 100.x.x.x  # Replace with your Tailscale IP
```

### Verify Proxy Usage

Check that searches go through your reverse proxy by monitoring its logs:

```bash
# On reverse-proxy machine
journalctl -u squid -f
```

### Debug Network Access

```bash
# Test access restriction
curl -H "Host: searxng.lab.local" http://your-tailscale-ip/

# Check Nginx access logs
tail -f /var/log/nginx/access.log
```

## Customization

### Search Engines

Modify the `searxngSettings` in the module to enable/disable specific search engines:

```nix
# In the module configuration
engines = [
  {
    name = "google";
    disabled = false;
  }
  {
    name = "bing"; 
    disabled = true;
  }
];
```

### Themes and UI

Change the UI theme and settings:

```nix
ui = {
  default_theme = "oscar";  # Other options: simple, oscar, pix-art
  infinite_scroll = true;
  center_alignment = true;
};
```

### Rate Limiting

Adjust rate limiting settings:

```nix
server = {
  limiter = true;
  public_instance = false;
  method = "POST";
};
```

## Integration with Other Services

### Monitoring

Add Prometheus monitoring (optional):

```nix
services.prometheus.exporters.nginx.enable = true;
services.prometheus.scrapeConfigs = [
  {
    job_name = "searxng";
    static_configs = [
      { targets = [ "localhost:8888" ]; }
    ];
  }
];
```

### Backup

Include SearXNG data in your backup strategy:

```nix
# Add to your backup configuration
"/var/lib/searxng"
```

## Security Considerations

1. **Keep Updated**: Regularly update SearXNG package
2. **Monitor Access**: Review Nginx access logs periodically  
3. **Proxy Security**: Ensure reverse proxy is properly secured
4. **Tailscale Security**: Use Tailscale ACLs to further restrict access
5. **Instance Privacy**: Never expose this instance publicly

## Performance Tuning

### For High Usage

```nix
# Increase worker processes
systemd.services.searxng.serviceConfig.MemoryMax = "1G";

# Nginx caching
services.nginx.virtualHosts."searxng.lab.local".locations."/static/" = {
  expires = "1d";
  extraConfig = "add_header Cache-Control public;";
};
```

### For Low Resources

```nix
# Reduce memory usage
systemd.services.searxng.serviceConfig.MemoryMax = "256M";

# Disable image proxy if not needed
server.image_proxy = false;
```
