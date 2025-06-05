# Networking Configuration - sleeper-service
# Xeon file server network setup  
{ config, pkgs, ... }:

{
  imports = [
    ./common.nix
  ];

  # Machine-specific network configuration
  networking = {
    hostName = "sleeper-service";
    networkmanager.enable = true;
    
    # Additional firewall ports for file server services
    # (Add specific ports as needed for file sharing services)
    firewall.allowedTCPPorts = [ 
      # Add additional ports here as needed
    ];
  };
}
