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
    extraPortals = [
      pkgs.xdg-desktop-portal-wlr
      pkgs.xdg-desktop-portal-gtk
    ];
    config.common.default = "*";
    config.niri.default = ["wlr" "gtk"];
  };

  # Display manager and session management
  services.dbus.enable = true;

  # Enable XWayland for X11 app compatibility (Steam, etc.)
  programs.xwayland.enable = true;

  # Common desktop packages
  environment.systemPackages = with pkgs; [
    firefox
  ];

  # GUI sudo askpass helper for desktop environments
  # A graphical askpass helper was previously used here (ksshaskpass), but the
  # libsForQt5.* attribute is no longer available on newer channels. To avoid
  # a hard dependency on a specific Qt package, leave `programs.ssh.askPassword`
  # unset and preserve `SSH_ASKPASS` in sudo environment if a desktop askpass
  # is provided by the user's session.
  security.sudo.extraConfig = ''
    Defaults env_keep += "SSH_ASKPASS"
  '';

  # Flatpak support
  services.flatpak.enable = true;
}
