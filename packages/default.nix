{ pkgs ? import <nixpkgs> { } }:

{
  # Custom packages for Home-lab infrastructure
  
  # Home-lab administration command-line tool
  lab = pkgs.callPackage ./home-lab-tools.nix { };
  
  # Re-export commonly used packages with custom configurations
  inherit (pkgs)
    # Core utilities that might be customized
    git
    curl
    wget
    ;
}