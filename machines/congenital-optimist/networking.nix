# Networking Configuration - congenital-optimist
# AMD Threadripper workstation network setup
{ config, pkgs, ... }:

{
  # Network configuration
  networking = {
    hostName = "congenital-optimist";
    hostId = "8425e349";
    networkmanager.enable = true;
    nftables.enable = true;
    
    # Firewall configuration for workstation
    firewall = {
      enable = true;
      allowedTCPPorts = [ 
        22    # SSH
        9091  # Transmission RPC
      ];
      allowedUDPPorts = [ 22 ];
    };
  };

  # VPN and remote access
  services.tailscale.enable = true;
  services.openssh.enable = true;

  # ZFS services for this machine
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };
}
