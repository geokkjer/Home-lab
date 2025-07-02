# Example SearXNG Configuration for NixOS Home Lab
#
# This example shows how to enable SearXNG on one of your machines
# with proper reverse proxy integration and Tailscale-only access.
#
# Add this to your machine's configuration.nix:
{
  imports = [
    ../../modules/services/SearXNG.nix
  ];

  services.searxng-lab = {
    enable = true;
    hostName = "searxng.lab.local";

    # Configure reverse proxy for outbound traffic
    reverseProxyHost = "reverse-proxy";
    reverseProxyPort = 3128;

    # Security settings
    tailscaleOnly = true; # Only allow Tailscale network access
    openFirewall = true; # Open HTTP port
    nginxVhost = true; # Create Nginx virtual host
  };

  # Optional: Add hostname to /etc/hosts for easy access
  networking.hosts = {
    "127.0.0.1" = ["searxng.lab.local"];
  };
}
# Reverse Proxy Configuration (add to reverse-proxy machine)
#
# services.squid = {
#   enable = true;
#   configText = ''
#     http_port 3128
#     acl homelab src 100.0.0.0/8
#     acl homelab src 192.168.1.0/24
#     http_access allow homelab
#     http_access deny all
#     access_log none
#     forwarded_for delete
#   '';
# };
#
# networking.firewall.allowedTCPPorts = [ 3128 ];

