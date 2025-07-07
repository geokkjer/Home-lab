# Emacs daemon service configuration for Home Manager
# Migrated from system-level emacs service

{ config, lib, pkgs, ... }:

{
  services.emacs = {
    enable = true;
    # package will be set from emacs program configuration
    # Additional daemon-specific configuration
  };

  # User-level systemd service customization if needed
}