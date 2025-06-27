# modules/services/lab-auto-update.nix - NixOS service for automatic lab updates

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.lab-auto-update;
  
  # Get the lab tool from our packages
  labTool = pkgs.callPackage ../../packages/lab-tools.nix {};
  
  # Auto-update script that uses the Guile lab tool
  autoUpdateScript = pkgs.writeShellScript "lab-auto-update" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    LOG_FILE="/var/log/lab-auto-update.log"
    LOCK_FILE="/var/run/lab-auto-update.lock"
    
    # Ensure we don't run multiple instances
    if [ -f "$LOCK_FILE" ]; then
      echo "$(date): Auto-update already running (lock file exists)" >> "$LOG_FILE"
      exit 1
    fi
    
    # Create lock file
    echo $$ > "$LOCK_FILE"
    
    # Cleanup function
    cleanup() {
      rm -f "$LOCK_FILE"
    }
    trap cleanup EXIT
    
    echo "$(date): Starting lab auto-update" >> "$LOG_FILE"
    
    # Change to the lab directory
    cd "${cfg.flakePath}"
    
    # Run the Guile lab tool auto-update command
    if ${labTool}/bin/lab auto-update 2>&1 | tee -a "$LOG_FILE"; then
      echo "$(date): Auto-update completed successfully" >> "$LOG_FILE"
    else
      echo "$(date): Auto-update failed with exit code $?" >> "$LOG_FILE"
      exit 1
    fi
  '';
  
in
{
  options.services.lab-auto-update = {
    enable = mkEnableOption "Lab auto-update service";
    
    schedule = mkOption {
      type = types.str;
      default = "02:00";
      description = "Time to run updates (HH:MM format)";
    };
    
    randomizedDelay = mkOption {
      type = types.str;
      default = "30m";
      description = "Maximum random delay before starting update";
    };
    
    flakePath = mkOption {
      type = types.str;
      default = "/home/geir/Projects/home-lab";
      description = "Path to the home lab flake directory";
    };
    
    persistent = mkOption {
      type = types.bool;
      default = true;
      description = "Whether the timer should be persistent across reboots";
    };
    
    logRetentionDays = mkOption {
      type = types.int;
      default = 30;
      description = "Number of days to retain auto-update logs";
    };
  };

  config = mkIf cfg.enable {
    # Systemd service for the auto-update
    systemd.services.lab-auto-update = {
      description = "Home Lab Auto-Update Service";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        Group = "root";
        ExecStart = "${autoUpdateScript}";
        
        # Security settings
        PrivateTmp = true;
        ProtectSystem = false;  # We need to modify the system
        ProtectHome = true;
        NoNewPrivileges = false; # We need privileges for nixos-rebuild
        
        # Resource limits
        MemoryMax = "2G";
        CPUQuota = "50%";
        
        # Timeout settings
        TimeoutStartSec = "30m";
        TimeoutStopSec = "5m";
      };
      
      # Environment variables for the service
      environment = {
        PATH = lib.makeBinPath (with pkgs; [
          nix
          nixos-rebuild
          git
          openssh
          rsync
          gawk
          gnused
          coreutils
          util-linux
          systemd
        ]);
        NIX_PATH = "nixpkgs=${pkgs.path}";
      };
    };

    # Systemd timer for scheduling
    systemd.timers.lab-auto-update = {
      description = "Home Lab Auto-Update Timer";
      wantedBy = [ "timers.target" ];
      
      timerConfig = {
        OnCalendar = "daily";
        Persistent = cfg.persistent;
        RandomizedDelaySec = cfg.randomizedDelay;
        
        # Run at the specified time
        OnCalendar = "*-*-* ${cfg.schedule}:00";
        
        # Accuracy settings
        AccuracySec = "1min";
      };
    };

    # Log rotation for auto-update logs
    services.logrotate.settings.lab-auto-update = {
      files = "/var/log/lab-auto-update.log";
      frequency = "daily";
      rotate = cfg.logRetentionDays;
      compress = true;
      delaycompress = true;
      missingok = true;
      notifempty = true;
      create = "644 root root";
      postrotate = ''
        systemctl reload-or-restart rsyslog.service > /dev/null 2>&1 || true
      '';
    };

    # Ensure log directory exists with proper permissions
    systemd.tmpfiles.rules = [
      "d /var/log 0755 root root -"
      "f /var/log/lab-auto-update.log 0644 root root -"
    ];
    
    # Add a systemd target for manual triggering
    systemd.targets.lab-manual-update = {
      description = "Manual Lab Update Target";
    };
    
    # Service for manual updates (without reboot)
    systemd.services.lab-manual-update = {
      description = "Manual Home Lab Update (No Reboot)";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        Group = "root";
        ExecStart = "${labTool}/bin/lab update";
        RemainAfterExit = false;
      };
      
      environment = {
        PATH = lib.makeBinPath (with pkgs; [
          nix
          nixos-rebuild
          git
          openssh
          rsync
        ]);
      };
    };
  };
}