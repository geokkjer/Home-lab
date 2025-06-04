{ pkgs ? import <nixpkgs> { } }:

{
  # Custom packages for Home-lab infrastructure
  
  # Home-lab specific tools and utilities
  home-lab-tools = pkgs.callPackage ./home-lab-tools.nix { };
  
  # Re-export commonly used packages with custom configurations
  inherit (pkgs)
    # Core utilities that might be customized
    git
    curl
    wget
    ;
}