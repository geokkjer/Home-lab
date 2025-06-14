{
  lib,
  stdenv,
  writeShellScriptBin,
  rsync,
  openssh,
  ...
}:
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
      "congenital-optimist")
        # Local deployment - no SSH needed
        log "Deploying $machine (mode: $mode) locally"

        # Deploy the configuration locally
        log "Running nixos-rebuild $mode locally..."
        if ! sudo nixos-rebuild $mode --flake "$HOMELAB_ROOT#$machine"; then
          error "Failed to deploy configuration to $machine"
          exit 1
        fi

        success "Successfully deployed $machine"
        return 0
        ;;
      "sleeper-service")
        local target_host="admin-sleeper"
        ;;
      "grey-area")
        local target_host="admin-grey"
        ;;
      "reverse-proxy")
        local target_host="admin-reverse"
        ;;
      *)
        error "Unknown machine: $machine"
        error "Available machines: congenital-optimist, sleeper-service, grey-area, reverse-proxy"
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

  # Deploy with deploy-rs function
  deploy_rs_machine() {
    local machine="$1"
    local dry_run="''${2:-false}"

    log "Using deploy-rs for $machine deployment"

    cd "$HOMELAB_ROOT"

    if [[ "$dry_run" == "true" ]]; then
      log "Running dry-run deployment..."
      if ! nix run github:serokell/deploy-rs -- ".#$machine" --dry-activate; then
        error "Deploy-rs dry-run failed for $machine"
        return 1
      fi
      success "Deploy-rs dry-run completed for $machine"
    else
      log "Running actual deployment..."
      if ! nix run github:serokell/deploy-rs -- ".#$machine"; then
        error "Deploy-rs deployment failed for $machine"
        return 1
      fi
      success "Deploy-rs deployment completed for $machine"
    fi
  }

  # Update flake inputs function
  update_flake() {
    log "Updating flake inputs..."
    cd "$HOMELAB_ROOT"

    if ! nix flake update; then
      error "Failed to update flake inputs"
      return 1
    fi

    log "Checking updated flake configuration..."
    if ! nix flake check; then
      error "Flake check failed after update"
      return 1
    fi

    success "Flake inputs updated successfully"

    # Show what changed
    log "Flake lock changes:"
    git diff --no-index /dev/null flake.lock | grep "+" | head -10 || true
  }

  # Hybrid update: flake update + deploy-rs deployment
  hybrid_update() {
    local target="''${1:-all}"
    local dry_run="''${2:-false}"

    log "Starting hybrid update process (target: $target, dry-run: $dry_run)"

    # Step 1: Update flake inputs
    if ! update_flake; then
      error "Failed to update flake - aborting hybrid update"
      return 1
    fi

    # Step 2: Deploy with deploy-rs
    if [[ "$target" == "all" ]]; then
      local machines=("sleeper-service" "grey-area" "reverse-proxy" "congenital-optimist")
      local failed_machines=()

      for machine in "''${machines[@]}"; do
        log "Deploying updated configuration to $machine..."
        if deploy_rs_machine "$machine" "$dry_run"; then
          success "✓ $machine updated successfully"
        else
          error "✗ Failed to update $machine"
          failed_machines+=("$machine")
        fi
        echo ""
      done

      if [[ ''${#failed_machines[@]} -eq 0 ]]; then
        success "All machines updated successfully with hybrid approach!"
      else
        error "Failed to update: ''${failed_machines[*]}"
        return 1
      fi
    else
      deploy_rs_machine "$target" "$dry_run"
    fi
  }

  # Update all machines function (legacy method)
  update_all_machines() {
    local mode="''${1:-boot}"  # boot, test, or switch
    local machines=("congenital-optimist" "sleeper-service" "grey-area" "reverse-proxy")
    local failed_machines=()

    log "Starting update of all machines (mode: $mode) - using legacy method"

    for machine in "''${machines[@]}"; do
      log "Updating $machine..."
      if deploy_machine "$machine" "$mode"; then
        success "✓ $machine updated successfully"
      else
        error "✗ Failed to update $machine"
        failed_machines+=("$machine")
      fi
      echo ""  # Add spacing between machines
    done

    if [[ ''${#failed_machines[@]} -eq 0 ]]; then
      success "All machines updated successfully!"
    else
      error "Failed to update: ''${failed_machines[*]}"
      exit 1
    fi
  }

  # Simple connection test - removed complex generation info due to bash escaping issues
  # This will be reimplemented in a more robust language later
  test_connection() {
    local machine="$1"
    local admin_alias="$2"

    if [[ "$machine" == "congenital-optimist" ]]; then
      echo "    Status: Local machine"
    else
      if ${openssh}/bin/ssh -o ConnectTimeout=3 -o BatchMode=yes "$admin_alias" "echo OK" >/dev/null 2>&1; then
        echo "    Status: Connected via $admin_alias"
      else
        echo "    Status: Connection failed"
      fi
    fi
  }

  # Show deployment status (simplified - removed complex bash escaping)
  show_status() {
    log "Home-lab infrastructure status:"

    # Check congenital-optimist (local)
    if /run/current-system/sw/bin/systemctl is-active --quiet tailscaled; then
      success "  congenital-optimist: ✓ Online (local)"

      # Show simple connection test if verbose
      if [[ "''${1:-}" == "-v" ]]; then
        test_connection "congenital-optimist" ""
      fi
    else
      warn "  congenital-optimist: ⚠ Tailscale inactive"
    fi

    # Check if -v (verbose) flag is passed
    local verbose=0
    if [[ "''${1:-}" == "-v" ]]; then
      verbose=1
    fi

    # Check remote machines
    for machine in sleeper-service grey-area reverse-proxy; do
      # Set admin alias for SSH connection
      local admin_alias
      case "$machine" in
        "sleeper-service")
          admin_alias="admin-sleeper"
          tailscale_hostname="sleeper-service.tail807ea.ts.net"
          ;;
        "grey-area")
          admin_alias="admin-grey"
          tailscale_hostname="grey-area.tail807ea.ts.net"
          ;;
        "reverse-proxy")
          admin_alias="admin-reverse"
          tailscale_hostname="reverse-proxy.tail807ea.ts.net"
          ;;
      esac

      # Test SSH connectivity with debug info if in verbose mode
      if [[ $verbose -eq 1 ]]; then
        log "Testing SSH connection to $admin_alias (admin alias)..."
        ${openssh}/bin/ssh -v -o ConnectTimeout=5 -o BatchMode=yes "$admin_alias" "echo ✓ SSH connection to $admin_alias successful" 2>&1

        log "Testing SSH connection to sma@$tailscale_hostname (Tailscale direct)..."
        ${openssh}/bin/ssh -v -o ConnectTimeout=5 -o BatchMode=yes -i ~/.ssh/id_ed25519_admin "sma@$tailscale_hostname" "echo ✓ SSH connection to $tailscale_hostname successful" 2>&1
      fi

      # Try admin alias first (should work for all machines)
      if ${openssh}/bin/ssh -o ConnectTimeout=3 -o BatchMode=yes "$admin_alias" "echo OK" >/dev/null 2>&1; then
        success "  $machine: ✓ Online (admin access)"

        # Show simple connection test if verbose
        if [[ $verbose -eq 1 ]]; then
          test_connection "$machine" "$admin_alias"
        fi

      # Fallback to direct Tailscale connection with admin key
      elif ${openssh}/bin/ssh -o ConnectTimeout=5 -o BatchMode=yes -i ~/.ssh/id_ed25519_admin "sma@$tailscale_hostname" "echo OK" >/dev/null 2>&1; then
        success "  $machine: ✓ Online (Tailscale)"

        # Show simple connection test if verbose
        if [[ $verbose -eq 1 ]]; then
          test_connection "$machine" "sma@$tailscale_hostname"
        fi

      else
        warn "  $machine: ⚠ Unreachable"
        if [[ $verbose -eq 1 ]]; then
          log "    ℹ️  Note: Tried both admin alias ($admin_alias) and direct Tailscale connection"
          log "    ℹ️  Check if machine is online and SSH service is running"
          test_connection "$machine" "$admin_alias"  # Show failed connection info
        fi
      fi
    done
  }

  # Main command handling
  case "''${1:-}" in
    "deploy")
      if [[ $# -lt 2 ]]; then
        error "Usage: lab deploy <machine> [mode]"
        error "Machines: congenital-optimist, sleeper-service, grey-area, reverse-proxy"
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

    "deploy-rs")
      if [[ $# -lt 2 ]]; then
        error "Usage: lab deploy-rs <machine> [--dry-run]"
        error "Machines: congenital-optimist, sleeper-service, grey-area, reverse-proxy"
        exit 1
      fi

      machine="$2"
      dry_run="false"

      if [[ "''${3:-}" == "--dry-run" ]]; then
        dry_run="true"
      fi

      deploy_rs_machine "$machine" "$dry_run"
      ;;

    "update-flake")
      update_flake
      ;;

    "hybrid-update")
      target="''${2:-all}"
      dry_run="false"

      if [[ "''${3:-}" == "--dry-run" ]]; then
        dry_run="true"
      fi

      hybrid_update "$target" "$dry_run"
      ;;

    "status")
      show_status "''${2:-}"
      ;;

    "update")
      mode="''${2:-boot}"

      if [[ ! "$mode" =~ ^(boot|test|switch)$ ]]; then
        error "Invalid mode: $mode. Use boot, test, or switch"
        exit 1
      fi

      update_all_machines "$mode"
      ;;

    *)
      echo "Home-lab Management Tool"
      echo ""
      echo "Usage: lab <command> [options]"
      echo ""
      echo "Available commands:"
      echo "  deploy <machine> [mode]     - Deploy configuration to a machine (legacy method)"
      echo "                               Machines: congenital-optimist, sleeper-service, grey-area, reverse-proxy"
      echo "                               Modes: boot (default), test, switch"
      echo "  deploy-rs <machine> [opts]  - Deploy using deploy-rs (modern method)"
      echo "                               Options: --dry-run"
      echo "  update [mode]               - Update all machines (legacy method)"
      echo "                               Modes: boot (default), test, switch"
      echo "  update-flake                - Update flake inputs and check configuration"
      echo "  hybrid-update [target] [opts] - Update flake + deploy with deploy-rs"
      echo "                               Target: machine name or 'all' (default)"
      echo "                               Options: --dry-run"
      echo "  status [-v]                 - Check infrastructure connectivity"
      echo "                               -v: verbose SSH debugging"
      echo ""
      echo "Deployment Methods:"
      echo "  Legacy (SSH + rsync):       Reliable, tested, slower"
      echo "  Deploy-rs:                  Modern, automatic rollback, parallel deployment"
      echo "  Hybrid:                     Combines flake updates with deploy-rs safety"
      echo ""
      echo "Ollama AI Tools (when available):"
      echo "  ollama-cli <command>        - Manage Ollama service and models"
      echo "  monitor-ollama [opts]       - Monitor Ollama service health"
      echo ""
      echo "Examples:"
      echo "  # Legacy deployment"
      echo "  lab deploy sleeper-service boot       # Deploy and set for next boot"
      echo "  lab deploy grey-area switch           # Deploy and switch immediately"
      echo "  lab update boot                       # Update all machines for next boot"
      echo ""
      echo "  # Modern deploy-rs deployment"
      echo "  lab deploy-rs sleeper-service         # Deploy with automatic rollback"
      echo "  lab deploy-rs grey-area --dry-run     # Test deployment without applying"
      echo ""
      echo "  # Hybrid approach (recommended for updates)"
      echo "  lab hybrid-update sleeper-service     # Update flake + deploy specific machine"
      echo "  lab hybrid-update all --dry-run       # Test update all machines"
      echo "  lab update-flake                      # Just update flake inputs"
      echo ""
      echo "  # Status and monitoring"
      echo "  lab status                            # Check all machines"
      echo "  lab status -v                         # Verbose SSH debugging"
      echo ""
      echo "  # Ollama AI tools"
      echo "  ollama-cli status                     # Check Ollama service status"
      echo "  ollama-cli models                     # List installed AI models"
      echo "  monitor-ollama --test-inference       # Full Ollama health check"
      ;;
  esac
''
