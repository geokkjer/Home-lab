# Networking Configuration - congenital-optimist
# AMD Threadripper workstation network setup
{ config, pkgs, ... }:

{
  imports = [
    ./common.nix
  ];

  # Machine-specific network configuration
  networking = {
    hostName = "congenital-optimist";
    hostId = "8425e349";
    networkmanager.enable = true;
    
    # Additional firewall ports for workstation services
    firewall.allowedTCPPorts = [ 
    ];
  };

}
