{pkgs ? import <nixpkgs> {}}: {
  # Custom packages for Home-lab infrastructure

  # Home-lab administration command-line tool
  lab = (pkgs.callPackage ./lab-tools.nix {}).lab;

  # Claude Task Master AI package
  claude-task-master-ai = pkgs.callPackage ./claude-task-master-ai.nix {};

  # Re-export commonly used packages with custom configurations
  # (Basic CLI tools moved to base.nix)
}
