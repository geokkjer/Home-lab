{
  description = "Home Lab MCP Integration - Guile MCP Server and VS Code Extension";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      # Development shell
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          # Guile and Scheme ecosystem
          guile
          guile-json
          guile-ssh
          guile-websocket

          # Node.js for VS Code extension development
          nodejs
          nodePackages.npm
          nodePackages.typescript

          # Development and testing tools
          git
          curl
          jq
          netcat

          # Documentation and analysis
          pandoc
          graphviz

          # Optional: VS Code for development
          # vscode
        ];

        shellHook = ''
          echo "🏠 Home Lab MCP Integration Development Environment"
          echo ""
          echo "Available tools:"
          echo "  • Guile with JSON, SSH, WebSocket support"
          echo "  • Node.js with TypeScript and VS Code tools"
          echo "  • Development utilities: git, curl, jq, netcat"
          echo ""
          echo "Quick start:"
          echo "  1. npm install          # Install extension dependencies"
          echo "  2. npm run compile      # Compile TypeScript extension"
          echo "  3. ./setup-mcp-integration.sh  # Run full setup"
          echo ""
          echo "MCP Server: ./guile-mcp-server.scm"
          echo "VS Code Extension: ./vscode-homelab-extension.ts"
          echo ""
        '';

        # Environment variables
        GUILE_LOAD_PATH = "${pkgs.guile-json}/share/guile/site/3.0:${pkgs.guile-ssh}/share/guile/site/3.0";
        NODE_PATH = "${pkgs.nodePackages.typescript}/lib/node_modules";
      };

      # Packages
      packages = {
        # Guile MCP Server package
        guile-mcp-server = pkgs.stdenv.mkDerivation {
          pname = "guile-mcp-server";
          version = "0.1.0";

          src = ./.;

          buildInputs = with pkgs; [
            guile
            guile-json
            guile-ssh
          ];

          installPhase = ''
            mkdir -p $out/bin
            cp guile-mcp-server.scm $out/bin/guile-mcp-server
            chmod +x $out/bin/guile-mcp-server
          '';

          meta = with pkgs.lib; {
            description = "Home Lab MCP Server implemented in Guile Scheme";
            license = licenses.mit;
            maintainers = [];
            platforms = platforms.unix;
          };
        };

        # VS Code Extension package
        vscode-homelab-extension = pkgs.stdenv.mkDerivation {
          pname = "vscode-homelab-extension";
          version = "0.1.0";

          src = ./.;

          buildInputs = with pkgs; [
            nodejs
            npm
          ];

          buildPhase = ''
            npm install
            npm run compile
          '';

          installPhase = ''
            mkdir -p $out
            cp -r . $out/
          '';

          meta = with pkgs.lib; {
            description = "VS Code extension for Home Lab MCP integration";
            license = licenses.mit;
            maintainers = [];
          };
        };
      };

      # Default package
      defaultPackage = self.packages.${system}.guile-mcp-server;

      # Apps for easy running
      apps = {
        mcp-server = {
          type = "app";
          program = "${self.packages.${system}.guile-mcp-server}/bin/guile-mcp-server";
        };
      };

      defaultApp = self.apps.${system}.mcp-server;
    });
}
