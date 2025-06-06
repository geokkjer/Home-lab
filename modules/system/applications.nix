{ config, pkgs, ... }: 
let
  # Import custom packages from the flake
  homeLabPackages = import ../../packages { inherit pkgs; };
in {
  # System applications and utilities
  environment.systemPackages = with pkgs; [
    # Home lab management tools
    homeLabPackages.lab

    # Terminal applications
    kitty
    terminator
    rio
    greetd.tuigreet

    # System monitoring
    glances
    inxi
    htop
    bottom
    systemctl-tui

    # File and data tools
    wget
    curl
    mc

    # Desktop integration
    dbus
    wayland
    xdg-utils
  ];

  # Flatpak support
  services.flatpak.enable = true;
}