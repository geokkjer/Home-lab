{ config, pkgs, ... }:
{
  virtualisation.incus = {
    enable = true;
    ui.enable = true;
    package = pkgs.incus;
  };

  environment.systemPackages = with pkgs; [
    incus
    lxc
  ];
  
  networking.firewall.allowedTCPPorts = [ 8443 ];
}
