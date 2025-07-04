{ config, pkgs, ... }:
{
  virtualisation.incus = {
    enable = true;
    ui.enable = true;
    package = pkgs.incus-lts;  # Use LTS version to avoid cowsql build issues
  };

  environment.systemPackages = [
    pkgs.incus-lts
    pkgs.lxc
  ];
  
  networking.firewall.allowedTCPPorts = [ 8443 ];
}
