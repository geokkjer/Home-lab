# Default.nix for lab-tool
# Provides the lab-tool package for inclusion in other Nix expressions
(import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {}).callPackage ./. {}
