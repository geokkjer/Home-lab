{
  config,
  pkgs,
  ...
}: {
  # GNOME Desktop Environment
  services.xserver = {
    enable = true;
    xkb.layout = "no";
  };

  # Use the new top-level option for desktop manager
  services.desktopManager = {
    gnome.enable = true;
  };

  # GNOME-specific packages
  environment.systemPackages = with pkgs; [
    gnome-extension-manager
    gnome-shell-extensions
    dconf-editor
    gnome-tweaks
    gnome-terminal
    nautilus
    capitaine-cursors-themed
  ];

  # GNOME services
  services.gnome = {
    gnome-keyring.enable = true;
    glib-networking.enable = true;
  };
}
