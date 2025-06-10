{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.niri.enable = true;
  environment.systemPackages = with pkgs; [
    # Niri scrolling window manager
    niri
    alacritty
  ];
}
