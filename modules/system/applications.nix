{ config, pkgs, ... }: {
  # System applications and utilities
  environment.systemPackages = with pkgs; [
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