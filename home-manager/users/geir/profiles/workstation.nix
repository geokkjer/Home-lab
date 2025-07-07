# Workstation profile configuration for geir
# Contains full workstation packages and configurations for powerful machines

{ config, lib, pkgs, ... }:

{
  # Workstation-specific packages
  home.packages = with pkgs; [
    # Full workstation packages
    # Creative tools, media applications, etc.
  ];

  # Workstation-specific configurations
}