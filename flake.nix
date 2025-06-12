{
  description = "Home Lab NixOS Configuration - congenital-optimist & sleeper-service";

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
      # congenital-optimist - AMD Threadripper workstation
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

      # sleeper-service - Intel Xeon file server
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

      # reverse-proxy - VPS edge server with Nginx reverse proxy
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

      # grey-area - Services host (Forgejo, Jellyfin, etc.)
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
    };

    # Custom packages for the home lab
    packages.${system} = import ./packages {
      pkgs = nixpkgs.legacyPackages.${system};
    };

    # Development shells for different projects
    devShells.${system} = {
      default = nixpkgs.legacyPackages.${system}.mkShell {
        buildInputs = with nixpkgs.legacyPackages.${system}; [
          nixd
          alejandra
          nixpkgs-fmt
          git
          emacs
        ];
        shellHook = ''
          echo "Home-lab development environment"
          echo "Available configurations:"
          echo "  - congenital-optimist (Threadripper workstation)"
          echo "  - sleeper-service (Xeon file server)"
          echo "  - reverse-proxy (VPS edge server)"
          echo "  - grey-area (Services host: Forgejo, Jellyfin, etc.)"
          echo ""
          echo "Build with: nixos-rebuild build --flake .#<config>"
          echo "Switch with: nixos-rebuild switch --flake .#<config>"
        '';
      };

      # Dotfiles development shell
      dotfiles = nixpkgs.legacyPackages.${system}.mkShell {
        buildInputs = with nixpkgs.legacyPackages.${system}; [
          emacs
          pandoc
          starship
          nixpkgs-fmt
          alejandra
        ];
        shellHook = ''
          echo "Literate dotfiles development environment"
          echo "Tangle dotfiles with: emacs --batch -l org --eval \"(org-babel-tangle-file \\\"README.org\\\")\""
        '';
      };
    };

    # Overlays for package customizations
    overlays.default = import ./overlays;

    # Applications that can be run directly
    apps.${system} = {
      # Tangle all user dotfiles
      tangle-dotfiles = {
        type = "app";
        program = "${nixpkgs.legacyPackages.${system}.writeShellScript "tangle-dotfiles" ''
          cd users/geir/dotfiles
          ${nixpkgs.legacyPackages.${system}.emacs}/bin/emacs --batch -l org --eval "(org-babel-tangle-file \"README.org\")"
          echo "Dotfiles tangled successfully!"
        ''}";
      };

      # Check flake configuration
      check-config = {
        type = "app";
        program = "${nixpkgs.legacyPackages.${system}.writeShellScript "check-config" ''
          echo "Checking flake configuration..."
          nix flake check
          echo "Configuration check complete!"
        ''}";
      };
    };

    # Formatter for Nix files
    formatter.${system} = nixpkgs.legacyPackages.${system}.alejandra;
  };
}
