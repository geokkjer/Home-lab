{
  config,
  pkgs,
  ...
}: {
  imports = [
    ../sound/pipewire.nix
  ];

  # Common desktop configuration shared across all environments

  # XDG Portal configuration for Wayland/X11 compatibility
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
  };

  # Display manager and session management
  services.dbus.enable = true;

  # Enable XWayland for X11 app compatibility (Steam, etc.)
  programs.xwayland.enable = true;

  # Common desktop packages
  environment.systemPackages = with pkgs; [
    firefox
  ];
  # Flatpak support
  services.flatpak.enable = true;
}
