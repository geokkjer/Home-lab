{
  lib,
  stdenv,
  guile,
  makeWrapper,
  writeShellScriptBin,
}: let
  # Lab Tool - K.I.S.S Refactored Implementation
  lab-tool = stdenv.mkDerivation {
    pname = "lab-tool";
    version = "2.0.0-kiss";

    src = ./lab;

    nativeBuildInputs = [makeWrapper];
    buildInputs = [
      guile
      # Runtime dependencies for auto-update functionality will be in PATH
    ];

    installPhase = ''
            mkdir -p $out/share/lab-tool
            cp -r . $out/share/lab-tool/

            mkdir -p $out/bin

            # Create the main lab tool executable
            cat > $out/bin/lab << 'EOF'
      #!/usr/bin/env bash
      export GUILE_LOAD_PATH="$out/share/lab-tool:$GUILE_LOAD_PATH"
      exec ${guile}/bin/guile "$out/share/lab-tool/main.scm" "$@"
      EOF
            chmod +x $out/bin/lab

            # Create aliases for convenience
            ln -s $out/bin/lab $out/bin/lab-tool
    '';

    meta = with lib; {
      description = "K.I.S.S refactored home lab management tool";
      longDescription = ''
        A modular, functional home lab management tool following K.I.S.S principles:
        - Modular: Each module has single responsibility
        - Functional: Pure functions separated from side effects
        - Small: Individual modules under 50 lines
        - Simple: One function does one thing well

        Features:
        - Infrastructure status checking
        - Machine management and deployment
        - SSH connectivity testing
        - Modular architecture for easy extension
      '';
      homepage = "https://github.com/geirda/Home-lab";
      license = licenses.mit;
      maintainers = ["geir"];
      platforms = platforms.unix;
    };
  };

  # MCP Server placeholder (for future implementation)
  mcp-server = writeShellScriptBin "mcp-server" ''
    echo "MCP Server - Coming Soon!"
    echo "This will provide Model Context Protocol integration"
    exit 0
  '';

  # RAG System placeholder (for future implementation)
  rag-system = writeShellScriptBin "rag-system" ''
    echo "RAG System - Coming Soon!"
    echo "This will provide Retrieval-Augmented Generation capabilities"
    exit 0
  '';
in {
  # Export individual tools
  inherit lab-tool mcp-server rag-system;

  # Main package combines all tools
  default = stdenv.mkDerivation {
    pname = "home-lab-tools";
    version = "2.0.0-kiss";

    dontUnpack = true;

    nativeBuildInputs = [makeWrapper];

    installPhase = ''
            mkdir -p $out/bin

            # Link all tools
            ln -s ${lab-tool}/bin/* $out/bin/
            ln -s ${mcp-server}/bin/* $out/bin/
            ln -s ${rag-system}/bin/* $out/bin/

            # Create main entry point that shows all available tools
            cat > $out/bin/home-lab-tools << 'EOF'
      #!/usr/bin/env bash
      echo "🏠 Home Lab Tools - K.I.S.S Edition"
      echo "=================================="
      echo ""
      echo "Available Tools:"
      echo "  lab          - Lab management tool (K.I.S.S refactored)"
      echo "  mcp-server   - Model Context Protocol server"
      echo "  rag-system   - Retrieval-Augmented Generation system"
      echo ""
      echo "Examples:"
      echo "  lab status           # Show infrastructure status"
      echo "  lab machines         # List all machines"
      echo "  lab deploy machine   # Deploy to machine"
      echo "  lab auto-update      # Automatic system update"
      echo "  mcp-server          # Start MCP server"
      echo "  rag-system          # Start RAG system"
      echo ""
      echo "For detailed help: lab help"
      EOF
            chmod +x $out/bin/home-lab-tools
    '';

    meta = with lib; {
      description = "Complete home lab tooling suite";
      longDescription = ''
        Comprehensive home lab management tooling following K.I.S.S principles.
        Includes lab tool, MCP server, and RAG system components.
      '';
      homepage = "https://github.com/geirda/Home-lab";
      license = licenses.mit;
      maintainers = ["geir"];
      platforms = platforms.unix;
    };
  };
}
