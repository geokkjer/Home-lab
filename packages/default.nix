{pkgs ? import <nixpkgs> {}}: {
  # Custom packages for Home-lab infrastructure

  # Home-lab administration command-line tool
  lab = (pkgs.callPackage ./lab-tools.nix {}).lab;
}
