# Zsh configuration for Home Manager
# Migrated from current zsh configuration in user modules

{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    
    # History configuration
    history = {
      size = 10000;
      path = "$HOME/.histfile";
    };

    # Shell aliases
    shellAliases = {
      # User-specific aliases will be migrated here
    };

    # Interactive shell initialization
    initExtra = ''
      # Shell initialization code will be migrated here
    '';
  };

  # Shell-related packages
  home.packages = with pkgs; [
    starship
    zoxide
    direnv
    # Other shell enhancement tools
  ];
}