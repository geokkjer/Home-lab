{ lib, stdenv, writeShellScriptBin, ... }:

writeShellScriptBin "home-lab-tools" ''
  #!/usr/bin/env bash
  
  # Home-lab administration tools
  # Placeholder for custom utilities and scripts
  
  case "$1" in
    "status")
      echo "Home-lab infrastructure status:"
      echo "  congenital-optimist: $(systemctl is-active tailscale || echo 'unknown')"
      echo "  sleeper-service: Checking connectivity..."
      ;;
    "backup")
      echo "Initiating backup procedures..."
      echo "This would trigger backup scripts across the infrastructure"
      ;;
    "monitor")
      echo "System monitoring overview:"
      echo "Use this space for custom monitoring commands"
      ;;
    "deploy")
      echo "Deploying configurations..."
      echo "This would handle nixos-rebuild across machines"
      ;;
    *)
      echo "Home-lab Tools"
      echo "Usage: $0 {status|backup|monitor|deploy}"
      echo ""
      echo "Available commands:"
      echo "  status  - Check infrastructure status"
      echo "  backup  - Run backup procedures"
      echo "  monitor - Show monitoring overview"
      echo "  deploy  - Deploy configurations"
      ;;
  esac
''