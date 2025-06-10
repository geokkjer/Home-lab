{ config, pkgs, ... }: {
  # GNOME Desktop Environment
  services.xserver = {
    enable = true;
    desktopManager.gnome.enable = true;
    xkb.layout = "no";
  };

  # GNOME-specific packages
  environment.systemPackages = with pkgs; [
    gnome-extension-manager
    gnome-shell-extensions
    dconf-editor
    gnome-tweaks
    gnome-terminal
    nautilus
  ];
  
  # GNOME services
  services.gnome = {
    gnome-keyring.enable = true;
    glib-networking.enable = true;
  };
}