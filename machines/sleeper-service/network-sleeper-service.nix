# Networking Configuration - sleeper-service
# Xeon file server network setup with systemd-networkd and static IPs
{ config, pkgs, ... }:

{
  imports = [
    ../../modules/network/common.nix
  ];

  # Machine-specific network configuration
  networking = {
    hostName = "sleeper-service";
    hostId = "8425e349"; # Unique identifier for the machine - DO NOT CHANGE!!
    
    # # Enable systemd-networkd for static networking
    # useNetworkd = false;
    # useDHCP = true;  
    
    # Disable NetworkManager in favor of systemd-networkd
    networkmanager.enable = true;
    
    # DNS configuration - use Pi-hole server
    nameservers = [ "10.0.0.14" "8.8.8.8" ];  # Pi-hole server, Google DNS fallback
    
    # Configure static IP for the main ethernet interface
    # interfaces.enp0s25 = {
    #   useDHCP = false;
    #   ipv4.addresses = [
    #     {
    #       address = "10.0.0.8";  # Static IP for sleeper-service (existing files.home machine)
    #       prefixLength = 24;
    #     }
    #   ];
    # };
    
    # Network gateway and DNS (based on nmap discovery)
    # defaultGateway = {
    #   address = "10.0.0.138";  # Discovered router at lan.home
    #   interface = "enp0s25";   # Main ethernet interface
    # };
    # nameservers = [ "10.0.0.14" "10.0.0.138" "8.8.8.8" ];  # Pi-hole, router, Google DNS fallback
    
    # Additional firewall ports for file server services
    firewall = {
      # Trust the Tailscale interface for mesh network access
      trustedInterfaces = [ "tailscale0" ];
      
      allowedTCPPorts = [ 
        22    # SSH
        111   # NFS portmapper
        2049  # NFS
        445   # SMB/CIFS
        139   # NetBIOS Session Service
        # Add additional ports here as needed
      ];
    
      allowedUDPPorts = [
        22    # SSH 
        111   # NFS portmapper
        2049  # NFS
        137   # NetBIOS Name Service
        138   # NetBIOS Datagram Service
      ];
    };
  };
}
