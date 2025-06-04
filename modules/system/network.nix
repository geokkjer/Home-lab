{ config, pkgs, ... }: {
  # Network configuration
  networking = {
    hostName = "congenital-optimist";
    hostId = "8425e349";
    networkmanager.enable = true;
    nftables.enable = true;
    
    # Firewall configuration
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
      allowedUDPPorts = [ 22 ];
    };
  };

  # VPN and remote access
  services.tailscale.enable = true;
  services.openssh.enable = true;

  # ZFS services
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };
}