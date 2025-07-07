# Git configuration for Home Manager

{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userName = "Geir Okkenhaug Jerstad";
    userEmail = "geokkjer@gmail.com";
    
    # Git configuration
    extraConfig = {
      # Git settings will be configured here
    };
  };

  # Git-related packages
  home.packages = with pkgs; [
    git-credential-manager
    # Other git tools
  ];
}