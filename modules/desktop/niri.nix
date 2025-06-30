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

    # Core Sway tools
    swaylock
    swayidle
    swaybg

    # Wayland utilities
    waybar # Status bar
    fuzzel # Application launcher
    gammastep # Blue light filter
    mako # Notification daemon
    flameshot # Screenshot tool
    wl-clipboard # Clipboard utilities

    # Additional Wayland tools
    grim # Screenshot utility
    slurp # Screen area selection
    wf-recorder # Screen recorder
  ];
}
