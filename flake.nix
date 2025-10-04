{
  description = "Home Lab NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    ...
  } @ inputs: let
    system = "x86_64-linux";

    # Create unstable package set
    unstable = import nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;
    };

    # Shared special arguments for all machines
    specialArgs = {
      inherit inputs unstable;
    };
  in {
    # NixOS system configurations
    nixosConfigurations = {
      congenital-optimist = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules = [
          ./machines/congenital-optimist/configuration.nix
          ./machines/congenital-optimist/hardware-co.nix
          ./modules/common/nix.nix
          ./modules/common/base.nix
          ./modules/common/tty.nix
        ];
      };

      sleeper-service = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules = [
          ./machines/sleeper-service/configuration.nix
          ./machines/sleeper-service/hardware-configuration.nix
          ./modules/common/nix.nix
          ./modules/common/base.nix
          ./modules/common/tty.nix
        ];
      };

      limiting-factor = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules = [
          ./machines/limiting-factor/configuration.nix
          ./machines/limiting-factor/hardware-configuration.nix
          ./modules/common/nix.nix
          ./modules/common/base.nix
          ./modules/common/tty.nix
        ];
      };

      reverse-proxy = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules = [
          ./machines/reverse-proxy/configuration.nix
          ./machines/reverse-proxy/gandicloud.nix
          ./modules/common/nix.nix
          ./modules/common/base.nix
          ./modules/common/tty.nix
        ];
      };

      grey-area = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules = [
          ./machines/grey-area/configuration.nix
          ./machines/grey-area/hardware-configuration.nix
          ./modules/common/nix.nix
          ./modules/common/base.nix
          ./modules/common/tty.nix
        ];
      };

      little-rascal = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules = [
          ./machines/little-rascal/configuration.nix
          ./machines/little-rascal/hardware-configuration.nix
          ./modules/common/nix.nix
          ./modules/common/base.nix
          ./modules/common/tty.nix
        ];
      };
    };

    # Minimal development shell for compatibility
    devShells.${system}.default =
      let
        # Create deployment scripts
        ds = nixpkgs.legacyPackages.${system}.writeShellScriptBin "ds" ''
          nixos-rebuild boot --flake .#sleeper-service --target-host sma@sleeper-service.tail807ea.ts.net --use-remote-sudo "$@"
        '';
        dg = nixpkgs.legacyPackages.${system}.writeShellScriptBin "dg" ''
          nixos-rebuild boot --flake .#grey-area --target-host sma@grey-area.tail807ea.ts.net --use-remote-sudo "$@"
        '';
        dr = nixpkgs.legacyPackages.${system}.writeShellScriptBin "dr" ''
          nixos-rebuild boot --flake .#reverse-proxy --target-host sma@reverse-proxy.tail807ea.ts.net --use-remote-sudo "$@"
        '';
        dl = nixpkgs.legacyPackages.${system}.writeShellScriptBin "dl" ''
          nixos-rebuild boot --flake .#little-rascal --target-host sma@little-rascal.tail807ea.ts.net --use-remote-sudo "$@"
        '';
        dc = nixpkgs.legacyPackages.${system}.writeShellScriptBin "dc" ''
          nixos-rebuild boot --flake .#congenital-optimist --use-remote-sudo "$@"
        '';
        dlf = nixpkgs.legacyPackages.${system}.writeShellScriptBin "dlf" ''
          nixos-rebuild boot --flake .#limiting-factor --target-host sma@limiting-factor.tail807ea.ts.net --use-remote-sudo "$@"
        '';
      in
      nixpkgs.legacyPackages.${system}.mkShell {
        buildInputs = with nixpkgs.legacyPackages.${system}; [
          nixd
          alejandra
          git
          ds
          dg
          dr
          dl
          dc
          dlf
        ];
        shellHook = ''
          echo "Home-lab NixOS deployment environment"
          echo "Available deployment commands:"
          echo "  ds  - Deploy sleeper-service"
          echo "  dg  - Deploy grey-area"
          echo "  dr  - Deploy reverse-proxy"
          echo "  dl  - Deploy little-rascal"
          echo "  dc  - Deploy congenital-optimist (local)"
          echo "  dlf - Deploy limiting-factor"
          echo ""
          echo "Each command supports additional nixos-rebuild options:"
          echo "  ds --show-trace    # Deploy with trace"
          echo "  dg test           # Test without switching"
          echo ""
        '';
      };

    # Minimal packages for compatibility
    packages.${system}.default = nixpkgs.legacyPackages.${system}.hello;


  };
}
