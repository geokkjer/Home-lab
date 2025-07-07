# Main home configuration for user 'geir'
# This is the entry point for all user-specific Home Manager configuration

{ config, lib, pkgs, ... }:

{
  imports = [
    ./profiles/development.nix
    ./programs/emacs.nix
    ./programs/zsh.nix
    ./programs/git.nix
    ./programs/development.nix
    ./services/emacs-daemon.nix
    ./services/desktop.nix
  ];

  # Home Manager needs a bit of information about you and the paths it should manage
  home.username = "geir";
  home.homeDirectory = "/home/geir";

  # This value determines the Home Manager release that your configuration is compatible with
  home.stateVersion = "25.05";

  # Let Home Manager install and manage itself
  programs.home-manager.enable = true;

  # User-specific packages will be defined in profiles and programs
  home.packages = with pkgs; [
    # Essential user packages will be moved here from system configuration
  ];

  # User-specific environment variables
  home.sessionVariables = {
    EDITOR = "emacs";
    BROWSER = "firefox";
    TERMINAL = "kitty";
  };
}