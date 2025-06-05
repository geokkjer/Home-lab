# Networking Configuration - sleeper-service
# Xeon file server network setup  
{ config, pkgs, ... }:

{
  # Network configuration
  networking = {
    hostName = "sleeper-service";
    networkmanager.enable = true;
    nftables.enable = true;
    
    # Firewall configuration for file server
    firewall = {
      enable = true;
      allowedTCPPorts = [ 
        22    # SSH
        # Add other ports as needed for file sharing services
      ];
      allowedUDPPorts = [ ];
    };
  };

  # VPN and remote access
  services.tailscale.enable = true;
  
  # SSH configuration for headless server
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };
}
