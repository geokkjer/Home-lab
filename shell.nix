# Nix shell for Home Lab development with deploy-rs and lab-tool

{
  description = "Home Lab dev shell with deploy-rs and lab-tool";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = { self, nixpkgs, deploy-rs, ... }@inputs: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        pkgs.git
        pkgs.guile_3_0
        pkgs.guile-ssh
        pkgs.guile-json
        pkgs.guile-git
        pkgs.guile-gcrypt
        pkgs.openssh
        pkgs.nixos-rebuild
        deploy-rs.packages.${system}.deploy-rs
        (import ./packages/lab-tool/default.nix { inherit (pkgs) lib stdenv makeWrapper guile_3_0 guile-ssh guile-json guile-git guile-gcrypt openssh git nixos-rebuild; })
      ];
      shellHook = ''
        echo "Dev shell: deploy-rs and lab-tool available."
        echo "Try: lab status, lab deploy <machine>, or deploy . <target>"
      '';
    };
  };
}
