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
  users.users.geir = {
    extraGroups = [ 
      "incus-admin"
    ];
  };
  networking.firewall.allowedTCPPorts = [ 8443 ];
}
