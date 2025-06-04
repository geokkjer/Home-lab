{ config, pkgs, ... }: {
  # Sway Window Manager (Wayland-based i3 replacement)
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  # Sway-specific packages
  environment.systemPackages = with pkgs; [
    # Core Sway tools
    swaylock
    swayidle
    swaybg
    
    # Wayland utilities
    waybar          # Status bar
    fuzzel          # Application launcher
    gammastep       # Blue light filter
    mako            # Notification daemon
    flameshot       # Screenshot tool
    wl-clipboard    # Clipboard utilities
    
    # Additional Wayland tools
    grim            # Screenshot utility
    slurp           # Screen area selection
    wf-recorder     # Screen recorder
  ];
}