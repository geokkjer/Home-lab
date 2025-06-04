{ config, pkgs, ... }: {
  # User configuration for geir
  users.users.geir = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "libvirt" "incus-admin" "podman" ];
    shell = pkgs.zsh;
    packages = with pkgs; [
      # Browsers
      chromium
      vivaldi
      vivaldi-ffmpeg-codecs
      nyxt
      firefox

      # Terminal and shell tools
      starship
      fastfetch
      hyfetch
      nerdfetch
      zellij
      neo-cowsay
      fortune
      clolcat

      # Audio and system control
      ncpamixer
      pavucontrol

      # Desktop applications
      gimp
      obs-studio
      vesktop
      koodo-reader
      
      # System management
      virt-manager
      gnome-tweaks
      beauty-line-icon-theme

      # Emacs integration
      emacsPackages.vterm
    ];
  };
}