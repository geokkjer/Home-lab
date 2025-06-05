# Common Network Configuration
# Shared networking settings across all machines
{ config, pkgs, ... }:

{
  # Common networking settings
  networking = {
    # Enable nftables by default for all machines
    nftables.enable = true;
    
    # Common firewall settings
    firewall = {
      enable = true;
      # SSH is allowed by default on all machines
      allowedTCPPorts = [ 22 ];
    };
  };

  # Common services available on all machines
  services = {
    # Tailscale VPN for secure remote access
    tailscale.enable = true;
    
    # SSH access with secure defaults
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };
  };
}