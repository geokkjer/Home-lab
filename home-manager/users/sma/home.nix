# Main home configuration for user 'sma' (admin user)
# Minimal configuration for system administration tasks

{ config, lib, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should manage
  home.username = "sma";
  home.homeDirectory = "/home/sma";

  # This value determines the Home Manager release that your configuration is compatible with
  home.stateVersion = "25.05";

  # Let Home Manager install and manage itself
  programs.home-manager.enable = true;

  # Admin-specific packages
  home.packages = with pkgs; [
    # System administration tools
  ];

  # Basic shell configuration
  programs.zsh = {
    enable = true;
    history = {
      size = 5000;
      path = "$HOME/.histfile";
    };
  };

  # Basic git configuration
  programs.git = {
    enable = true;
    userName = "System Admin";
    userEmail = "admin@example.com";
  };

  # Admin-specific environment variables
  home.sessionVariables = {
    EDITOR = "nano";
  };
}