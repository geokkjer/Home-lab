{
  config,
  pkgs,
  ...
}: {
  # Music Production Module
  # This module provides a complete music production environment with CDP8 and SoundThread

  imports = [
    ../../sound/music-software.nix
  ];

  # Additional music production specific configuration can go here
  # For example, if you want to add SuperCollider or other music tools:
  # environment.systemPackages = with pkgs; [
  #   supercollider-with-plugins
  # ];
}
