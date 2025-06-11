{
  config,
  pkgs,
  ...
}: {
  imports = [
    ../../../modules/users/media-group.nix
  ];

  services.jellyfin = {
    enable = true;
    group = "media";
  };
  networking.firewall.allowedTCPPorts = [8096 8920];
  networking.firewall.allowedUDPPorts = [1900 7359];
}
