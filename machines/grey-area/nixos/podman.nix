{ config, pkgs, ... }:
{
  virtualisation.podman.enable = true;
  virtualisation.podman.dockerCompat = true;
  virtualisation.podman.dockerSocket.enable = true;

  #virtualisation.defaultNetwork.settings.dns_enabled = true;
  environment.systemPackages = with pkgs; [
     podman-tui
     podman-compose
   ];

 }
