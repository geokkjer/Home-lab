{ lib, stdenv, writeShellScriptBin, rsync, openssh, ... }:

writeShellScriptBin "lab" ''
  #!/usr/bin/env bash
  
  # Home-lab administration tools
  # Deploy and manage NixOS configurations across home lab infrastructure
  
  set -euo pipefail
  
  # Configuration
  HOMELAB_ROOT="/home/geir/Home-lab"
  TEMP_CONFIG_DIR="/tmp/home-lab-config"
  
  # Color output
  RED='\033[0;31m'
  GREEN='\033[0;32m'
  YELLOW='\033[1;33m'
  BLUE='\033[0;34m'
  NC='\033[0m' # No Color
  
  log() {
    echo -e "''${BLUE}[lab]''${NC} $1"
  }
  
  success() {
    echo -e "''${GREEN}[lab]''${NC} $1"
  }
  
  warn() {
    echo -e "''${YELLOW}[lab]''${NC} $1"
  }
  
  error() {
    echo -e "''${RED}[lab]''${NC} $1" >&2
  }
  
  # Deployment function
  deploy_machine() {
    local machine="$1"
    local mode="''${2:-boot}"  # boot, test, or switch
    
    case "$machine" in
      "sleeper-service")
        local target_host="sma@sleeper-service"
        ;;
      "grey-area")
        local target_host="sma@grey-area"
        ;;
      "reverse-proxy")
        local target_host="sma@reverse-proxy"
        ;;
      *)
        error "Unknown machine: $machine"
        error "Available machines: sleeper-service, grey-area, reverse-proxy"
        exit 1
        ;;
    esac
    
    log "Deploying $machine (mode: $mode)"
    
    # Sync configuration to target machine
    log "Syncing configuration to $target_host..."
    if ! ${rsync}/bin/rsync -av --delete "$HOMELAB_ROOT/" "$target_host:$TEMP_CONFIG_DIR/"; then
      error "Failed to sync configuration to $machine"
      exit 1
    fi
    
    # Deploy the configuration
    log "Running nixos-rebuild $mode on $machine..."
    if ! ${openssh}/bin/ssh "$target_host" "cd $TEMP_CONFIG_DIR && sudo nixos-rebuild $mode --flake .#$machine"; then
      error "Failed to deploy configuration to $machine"
      exit 1
    fi
    
    success "Successfully deployed $machine"
  }
  
  # Show deployment status
  show_status() {
    log "Home-lab infrastructure status:"
    
    # Check congenital-optimist (local)
    if /run/current-system/sw/bin/systemctl is-active --quiet tailscaled; then
      success "  congenital-optimist: ✓ Online (local)"
    else
      warn "  congenital-optimist: ⚠ Tailscale inactive"
    fi
    
    # Check remote machines
    for machine in sleeper-service grey-area reverse-proxy; do
      if ${openssh}/bin/ssh -o ConnectTimeout=5 -o BatchMode=yes "sma@$machine" "echo OK" >/dev/null 2>&1; then
        success "  $machine: ✓ Online"
      else
        warn "  $machine: ⚠ Unreachable"
      fi
    done
  }
  
  # Main command handling
  case "''${1:-}" in
    "deploy")
      if [[ $# -lt 2 ]]; then
        error "Usage: lab deploy <machine> [mode]"
        error "Machines: sleeper-service, grey-area, reverse-proxy"
        error "Modes: boot (default), test, switch"
        exit 1
      fi
      
      machine="$2"
      mode="''${3:-boot}"
      
      if [[ ! "$mode" =~ ^(boot|test|switch)$ ]]; then
        error "Invalid mode: $mode. Use boot, test, or switch"
        exit 1
      fi
      
      deploy_machine "$machine" "$mode"
      ;;
      
    "status")
      show_status
      ;;
      
    *)
      echo "Home-lab Management Tool"
      echo ""
      echo "Usage: lab <command> [options]"
      echo ""
      echo "Available commands:"
      echo "  deploy <machine> [mode]  - Deploy configuration to a machine"
      echo "                            Machines: sleeper-service, grey-area, reverse-proxy"  
      echo "                            Modes: boot (default), test, switch"
      echo "  status                   - Check infrastructure connectivity"
      echo ""
      echo "Examples:"
      echo "  lab deploy sleeper-service boot    # Deploy and set for next boot"
      echo "  lab deploy grey-area switch        # Deploy and switch immediately"
      echo "  lab status                         # Check all machines"
      ;;
  esac
''