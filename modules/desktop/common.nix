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
  programs.ssh.askPassword = "${pkgs.libsForQt5.ksshaskpass}/bin/ksshaskpass";
  security.sudo.extraConfig = ''
    Defaults env_keep += "SSH_ASKPASS"
  '';
  environment.variables.SUDO_ASKPASS = "${pkgs.libsForQt5.ksshaskpass}/bin/ksshaskpass";
  
  # Flatpak support
  services.flatpak.enable = true;
}
