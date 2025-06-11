{
  config,
  pkgs,
  ...
}: {
  imports = [
    ../../../modules/users/media-group.nix
  ];

  services.calibre-web = {
    enable = true;
    group = "media";
    listen = {
      ip = "0.0.0.0";
      port = 8083;
    };
    options = {
      calibreLibrary = "/mnt/remote/media/books/calibre/";
      enableBookUploading = true;
    };
  };
  networking.firewall.allowedTCPPorts = [8083];
}
