{
  description = "Home Lab NixOS Configuration - congenital-optimist & sleeper-service";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    deploy-rs,
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

      # little-rascal - Development laptop with Niri + CLI login
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
          echo "  - little-rascal (Development laptop with Niri)"
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

    # Deploy-rs configuration for automated deployments
    deploy.nodes = {
      sleeper-service = {
        hostname = "sleeper-service.tail807ea.ts.net";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.sleeper-service;
          sshUser = "sma";
          sudo = "sudo -u";
          autoRollback = true;
          magicRollback = true;
          activationTimeout = 180;
          confirmTimeout = 30;
        };
      };

      grey-area = {
        hostname = "grey-area.tail807ea.ts.net";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.grey-area;
          sshUser = "sma";
          sudo = "sudo -u";
          autoRollback = true;
          magicRollback = true;
          activationTimeout = 180;
          confirmTimeout = 30;
        };
      };

      reverse-proxy = {
        hostname = "reverse-proxy.tail807ea.ts.net";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.reverse-proxy;
          sshUser = "sma";
          sudo = "sudo -u";
          autoRollback = true;
          magicRollback = true;
          activationTimeout = 240; # VPS might be slower
          confirmTimeout = 30;
        };
      };

      congenital-optimist = {
        hostname = "localhost";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.congenital-optimist;
          sshUser = "sma";
          sudo = "sudo -u";
          autoRollback = true;
          magicRollback = true;
          activationTimeout = 180;
          confirmTimeout = 30;
        };
      };

      little-rascal = {
        hostname = "little-rascal.tail807ea.ts.net";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.little-rascal;
          sshUser = "geir";
          sudo = "sudo -u";
          autoRollback = true;
          magicRollback = true;
          activationTimeout = 180;
          confirmTimeout = 30;
        };
      };
    };

    # Deploy-rs checks (recommended by deploy-rs)
    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

    # Formatter for Nix files
    formatter.${system} = nixpkgs.legacyPackages.${system}.alejandra;
  };
}
