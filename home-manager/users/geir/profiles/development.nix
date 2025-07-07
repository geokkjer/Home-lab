# Development profile configuration for geir
# Contains development-specific packages and configurations

{ config, lib, pkgs, ... }:

{
  # Development-specific packages
  home.packages = with pkgs; [
    # Development tools
    # Will be migrated from current user configuration
  ];

  # Development-specific environment variables
  home.sessionVariables = {
    # Development environment variables
  };

  # Development-specific services and configurations
}