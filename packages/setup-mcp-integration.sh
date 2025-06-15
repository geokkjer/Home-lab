#!/bin/bash
# Home Lab MCP Integration Setup and Test Script

set -e

echo "🏠 Setting up Home Lab MCP Integration..."

# Check prerequisites
check_prereqs() {
    echo "📋 Checking prerequisites..."
    
    if ! command -v guile &> /dev/null; then
        echo "❌ Guile not found. Install with: sudo apt install guile-3.0-dev"
        exit 1
    fi
    
    if ! command -v node &> /dev/null; then
        echo "❌ Node.js not found. Install Node.js first."
        exit 1
    fi
    
    if ! command -v code &> /dev/null; then
        echo "❌ VS Code not found. Install VS Code first."
        exit 1
    fi
    
    echo "✅ Prerequisites satisfied"
}

# Install Guile dependencies
install_guile_deps() {
    echo "📦 Installing Guile dependencies..."
    
    # Check if guile-json is available
    if guile -c "(use-modules (json))" 2>/dev/null; then
        echo "✅ guile-json already available"
    else
        echo "🔧 Installing guile-json..."
        # Try different methods to install guile-json
        if command -v guix &> /dev/null; then
            guix install guile-json
        elif command -v apt &> /dev/null; then
            sudo apt install guile-json
        else
            echo "⚠️  Please install guile-json manually"
        fi
    fi
}

# Set up VS Code extension
setup_extension() {
    echo "🔧 Setting up VS Code extension..."
    
    # Install npm dependencies
    npm install
    
    # Compile TypeScript
    npm run compile
    
    echo "✅ Extension compiled successfully"
}

# Test MCP server standalone
test_mcp_server() {
    echo "🧪 Testing MCP server..."
    
    # Make server executable
    chmod +x guile-mcp-server.scm
    
    # Test basic functionality
    echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}' | ./guile-mcp-server.scm > test_output.json
    
    if grep -q "protocolVersion" test_output.json; then
        echo "✅ MCP server responding correctly"
        rm test_output.json
    else
        echo "❌ MCP server test failed"
        cat test_output.json
        exit 1
    fi
}

# Install VS Code extension
install_extension() {
    echo "📥 Installing VS Code extension..."
    
    # Package extension
    if command -v vsce &> /dev/null; then
        vsce package
        code --install-extension *.vsix
    else
        echo "📝 Extension files ready. Install vsce to package: npm install -g @vscode/vsce"
        echo "   Then run: vsce package && code --install-extension *.vsix"
    fi
}

# Create example workspace
create_example() {
    echo "📁 Creating example workspace..."
    
    mkdir -p example-homelab/{hosts,services}
    
    cat > example-homelab/flake.nix << 'EOF'
{
  description = "Home Lab Infrastructure";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    nixosConfigurations = {
      server1 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./hosts/server1.nix ];
      };
      
      server2 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./hosts/server2.nix ];
      };
    };
  };
}
EOF

    cat > example-homelab/hosts/server1.nix << 'EOF'
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "server1";
  
  services.nginx.enable = true;
  services.postgresql.enable = true;
  
  system.stateVersion = "23.11";
}
EOF

    cat > example-homelab/hosts/server2.nix << 'EOF'
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "server2";
  
  services.grafana.enable = true;
  services.prometheus.enable = true;
  
  system.stateVersion = "23.11";
}
EOF

    echo "✅ Example workspace created in example-homelab/"
}

# Main setup flow
main() {
    check_prereqs
    install_guile_deps
    setup_extension
    test_mcp_server
    create_example
    
    echo ""
    echo "🎉 Setup complete!"
    echo ""
    echo "Next steps:"
    echo "1. Open example-homelab/ in VS Code"
    echo "2. Use Ctrl+Shift+P and search 'Home Lab' commands"
    echo "3. Try 'Home Lab: Show Infrastructure Status'"
    echo "4. Test Copilot integration with infrastructure context"
    echo ""
    echo "Available commands:"
    echo "- Home Lab: Connect to MCP Server"
    echo "- Home Lab: Deploy Machine"
    echo "- Home Lab: Show Infrastructure Status"
    echo "- Home Lab: Generate NixOS Configuration"
    echo "- Home Lab: List Available Tools"
    echo ""
    echo "The MCP server provides context to GitHub Copilot about your infrastructure!"
}

# Run setup
main "$@"
