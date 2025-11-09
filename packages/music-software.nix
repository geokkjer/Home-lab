{pkgs ? import <nixpkgs> {}}: {
  # Music software packages for Home-lab infrastructure

  # Composers Desktop Project (CDP) - comprehensive audio signal processing system
  cdp8 = pkgs.callPackage ./cdp.nix {};

  # SoundThread - Node-based GUI for The Composers Desktop Project (CDP)
  soundthread = pkgs.callPackage ./soundthread.nix {};
}
