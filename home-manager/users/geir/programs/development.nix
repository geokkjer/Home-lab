# Development tools and environment configuration

{ config, lib, pkgs, ... }:

{
  # Development packages
  home.packages = with pkgs; [
    # Development tools will be migrated here
    # neovim, vscode, nodejs, etc.
  ];

  # Development tool configurations
  programs = {
    # Individual development tool configurations
  };

  # Development environment variables
  home.sessionVariables = {
    # Development-specific environment variables
  };
}