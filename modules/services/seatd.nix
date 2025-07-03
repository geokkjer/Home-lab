# Seatd service module with greetd display manager and boot log suppression
# This module provides a clean login experience without systemd boot messages
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  options.services.seatd-clean = {
    enable = mkEnableOption "seatd with greetd and clean boot";

    suppressBootMessages = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to suppress systemd boot messages that interfere with login screen";
    };

    tuigreetCommand = mkOption {
      type = types.str;
      default = "${pkgs.niri}/bin/niri";
      description = "Command to execute after login via tuigreet (default: niri)";
    };

    tuigreetTheme = mkOption {
      type = types.str;
      default = "border=magenta;text=cyan;prompt=green;time=red;action=blue;button=yellow;container=black;input=red";
      description = "Custom theme string for tuigreet (see tuigreet README for options)";
    };
  };

  config = mkIf config.services.seatd-clean.enable {
    # Enable greetd display manager with tuigreet
    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --remember --theme '${config.services.seatd-clean.tuigreetTheme}' --cmd ${config.services.seatd-clean.tuigreetCommand}";
          user = "greeter";
        };
      };
    };

    # Suppress systemd boot messages when enabled
    boot = mkIf config.services.seatd-clean.suppressBootMessages {
      # Kernel parameters to reduce boot noise
      kernelParams = [
        "quiet" # Reduce kernel messages
        "systemd.show_status=false" # Hide systemd startup status
        "rd.systemd.show_status=false" # Hide initrd systemd status
        "rd.udev.log_level=3" # Reduce udev messages
        "udev.log_level=3" # Reduce udev messages
      ];

      # Console configuration
      consoleLogLevel = 0; # Minimum console log level

      # Plymouth for clean boot (optional)
      plymouth = {
        enable = false; # Keep disabled for now, can be enabled if desired
      };
    };

    # Systemd and journald configuration to reduce noise
    systemd = mkIf config.services.seatd-clean.suppressBootMessages {
      # Reduce log levels
      extraConfig = ''
        LogLevel=warning
        DefaultStandardOutput=null
        DefaultStandardError=null
      '';
    };

    # Journald configuration
    services.journald = mkIf config.services.seatd-clean.suppressBootMessages {
      extraConfig = ''
        # Reduce console output
        ForwardToConsole=no
        MaxLevelConsole=warning
        # Keep reasonable log retention
        MaxRetentionSec=7day
        SystemMaxUse=500M
      '';
    };

    # Ensure proper TTY configuration for clean display
    console = {
      earlySetup = true;
    };
  };
}
