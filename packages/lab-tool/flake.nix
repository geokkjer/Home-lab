{
  description = "Home Lab Tool - Guile implementation with MCP server";

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

      # Guile libraries we need
      guileLibs = with pkgs; [
        guile_3_0
        guile-ssh
        guile-json
        guile-git
        guile-gcrypt
      ];

      # Build the Guile lab tool
      lab-tool = pkgs.stdenv.mkDerivation {
        pname = "lab-tool";
        version = "0.1.0";

        src = ./.;

        buildInputs = guileLibs;
        nativeBuildInputs = [pkgs.makeWrapper];

        buildPhase = ''
          # Compile Guile modules for better performance
          mkdir -p $out/share/guile/site/3.0
          cp -r . $out/share/guile/site/3.0/lab-tool/

          # Compile .scm files to .go files
          for file in $(find . -name "*.scm"); do
            echo "Compiling $file"
            guild compile -L . -o $out/share/guile/site/3.0/''${file%.scm}.go $file || true
          done
        '';

        installPhase = ''
                      mkdir -p $out/bin

                      # Create the main lab executable
                      cat > $out/bin/lab << EOF
          #!/usr/bin/env bash
          export GUILE_LOAD_PATH="$out/share/guile/site/3.0/lab-tool:\$GUILE_LOAD_PATH"
          export GUILE_LOAD_COMPILED_PATH="$out/share/guile/site/3.0/lab-tool:\$GUILE_LOAD_COMPILED_PATH"
          exec ${pkgs.guile_3_0}/bin/guile "$out/share/guile/site/3.0/lab-tool/main.scm" "\$@"
          EOF
                      chmod +x $out/bin/lab

                      # Create MCP server executable
                      cat > $out/bin/lab-mcp-server << EOF
          #!/usr/bin/env bash
          export GUILE_LOAD_PATH="$out/share/guile/site/3.0/lab-tool:\$GUILE_LOAD_PATH"
          export GUILE_LOAD_COMPILED_PATH="$out/share/guile/site/3.0/lab-tool:\$GUILE_LOAD_COMPILED_PATH"
          exec ${pkgs.guile_3_0}/bin/guile -L "$out/share/guile/site/3.0/lab-tool" -c "(use-modules (mcp server)) (run-mcp-server)"
          EOF
                      chmod +x $out/bin/lab-mcp-server

                      # Wrap executables with proper environment
                      wrapProgram $out/bin/lab \
                        --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.openssh pkgs.git pkgs.nixos-rebuild]}

                      wrapProgram $out/bin/lab-mcp-server \
                        --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.openssh pkgs.git pkgs.nixos-rebuild]}
        '';

        meta = with pkgs.lib; {
          description = "Home Lab Tool - Guile implementation with MCP integration";
          license = licenses.mit;
          platforms = platforms.linux;
          maintainers = ["geir@home-lab"];
        };
      };
    in {
      packages = {
        default = lab-tool;
        lab-tool = lab-tool;
      };

      devShells.default = pkgs.mkShell {
        buildInputs =
          guileLibs
          ++ (with pkgs; [
            # Development tools
            emacs

            # System tools for lab operations
            openssh
            git
            nixos-rebuild

            # Optional for advanced features
            sqlite
            redis
          ]);

        shellHook = ''
          echo "🧪 Home Lab Tool Development Environment"
          echo "Available commands:"
          echo "  guile                  - Start Guile REPL"
          echo "  guild compile <file>   - Compile Guile modules"
          echo "  ./main.scm help        - Test the lab tool"
          echo ""
          echo "Module path: $(pwd)"

          export GUILE_LOAD_PATH="$(pwd):$GUILE_LOAD_PATH"
          export LAB_DEV_MODE=1
        '';
      };

      apps = {
        default = flake-utils.lib.mkApp {
          drv = lab-tool;
          name = "lab";
        };

        mcp-server = flake-utils.lib.mkApp {
          drv = lab-tool;
          name = "lab-mcp-server";
        };
      };
    });
}
