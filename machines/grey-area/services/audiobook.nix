{
  configs,
  pkgs,
  ...
}: {
  imports = [
    ../../../modules/users/media-group.nix
  ];

  environment.systemPackages = [
    pkgs.audiobookshelf
  ];
  services.audiobookshelf.group = "media";
  services.audiobookshelf.enable = true;
  services.audiobookshelf.host = "0.0.0.0";
  services.audiobookshelf.port = 8000;
  services.audiobookshelf.openFirewall = true;
}
