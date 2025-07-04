;; backup of old main.scm

;; ...existing code...
#!/usr/bin/env guile
!#

;; Home Lab Tool - Main Entry Point
;; K.I.S.S Refactored Implementation

;; Load path is set by the wrapper script in default.nix
;; No need to add current directory when running from Nix

(use-modules (ice-9 match)
             (ice-9 format)
             (utils config)
             (utils logging)
             (lab core)
             (lab machines)
             (lab deployment)
             (lab auto-update))

;; Initialize logging
(set-log-level! 'info)

;; Pure function: Command help text
(define (get-help-text)
  "Pure function returning help text"
  "Home Lab Tool - SSH + Rsync Edition

USAGE: lab <command> [args...]

COMMANDS:
  status              Show infrastructure status
  machines            List all machines
  deploy <machine> [options]  Deploy configuration to machine using SSH + rsync + nixos-rebuild
                      Options: --dry-run, --boot, --test, --use-deploy-rs
  deploy-all [options]        Deploy to all machines using SSH + rsync + nixos-rebuild
  update              Update flake inputs
  auto-update         Perform automatic system update with health checks
  auto-update-status  Show auto-update service status and logs
  health [machine]    Check machine health (all if no machine specified)
  ssh <machine>       SSH to machine (using sma user)
  test-rollback <machine>     Test deployment with rollback (uses deploy-rs)
  help                Show this help

EXAMPLES:
  lab status
  lab machines
  lab deploy little-rascal                # Deploy with SSH + rsync (default)
  lab deploy little-rascal --dry-run      # Test deployment without applying
  lab deploy little-rascal --boot         # Deploy but only activate on next boot
  lab deploy little-rascal --test         # Deploy but don't make permanent
  lab deploy little-rascal --use-deploy-rs # Use deploy-rs instead of SSH method
  lab deploy-all                          # Deploy to all machines
  lab deploy-all --dry-run                # Test deployment to all machines
  lab update                              # Update flake inputs
  lab test-rollback sleeper-service       # Test rollback functionality (deploy-rs)
  lab ssh sleeper-service                 # SSH to machine as sma user

SSH + Rsync Features (Default):
- Fast: Only syncs changed files with rsync
- Simple: Uses standard nixos-rebuild workflow
- Reliable: Same command workflow as manual deployment
- Flexible: Supports boot, test, and switch modes

Deploy-rs Features (Optional with --use-deploy-rs):
- Automatic rollback on deployment failure
- Health checks after deployment
- Magic rollback for network connectivity issues
- Atomic deployments with safety guarantees

This implementation uses SSH + rsync + nixos-rebuild by default:
- Fast: Efficient file synchronization
- Simple: Standard NixOS deployment workflow
- Consistent: Same user (sma) for all operations
- Flexible: Multiple deployment modes available"

;; ...existing code...
