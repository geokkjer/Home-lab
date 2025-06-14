# NixOS Ollama Service Configuration
#
# This module provides a comprehensive Ollama service configuration for the home lab.
# Ollama is a tool for running large language models locally with an OpenAI-compatible API.
#
# Features:
# - Secure service isolation with dedicated user
# - Configurable network binding (localhost by default for security)
# - Resource management and monitoring
# - Integration with existing NixOS infrastructure
# - Optional GPU acceleration support
# - Comprehensive logging and monitoring
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.homelab-ollama;
in {
  options.services.homelab-ollama = {
    enable = mkEnableOption "Ollama local LLM service for home lab";

    package = mkOption {
      type = types.package;
      default = pkgs.ollama;
      description = "The Ollama package to use";
    };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = ''
        The host address to bind to. Use "0.0.0.0" to allow external access.
        Default is localhost for security.
      '';
    };

    port = mkOption {
      type = types.port;
      default = 11434;
      description = "The port to bind to";
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/ollama";
      description = "Directory to store Ollama data including models";
    };

    user = mkOption {
      type = types.str;
      default = "ollama";
      description = "User account under which Ollama runs";
    };

    group = mkOption {
      type = types.str;
      default = "ollama";
      description = "Group under which Ollama runs";
    };

    environmentVariables = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = ''
        Environment variables for the Ollama service.
        Common variables:
        - OLLAMA_ORIGINS: Allowed origins for CORS (default: http://localhost,http://127.0.0.1)
        - OLLAMA_CONTEXT_LENGTH: Context window size (default: 2048)
        - OLLAMA_NUM_PARALLEL: Number of parallel requests (default: 1)
        - OLLAMA_MAX_QUEUE: Maximum queued requests (default: 512)
        - OLLAMA_DEBUG: Enable debug logging (default: false)
        - OLLAMA_MODELS: Model storage directory
      '';
      example = {
        OLLAMA_ORIGINS = "http://localhost,http://127.0.0.1,http://grey-area.lan";
        OLLAMA_CONTEXT_LENGTH = "4096";
        OLLAMA_DEBUG = "1";
      };
    };

    models = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        List of models to automatically download on service start.
        Models will be pulled using 'ollama pull <model>'.

        Popular models:
        - "llama3.3:8b" - Meta's latest Llama model (8B parameters)
        - "mistral:7b" - Mistral AI's efficient model
        - "codellama:7b" - Code-focused model
        - "gemma2:9b" - Google's Gemma model
        - "qwen2.5:7b" - Multilingual model with good coding

        Note: Models are large (4-32GB each). Ensure adequate storage.
      '';
      example = ["llama3.3:8b" "codellama:7b" "mistral:7b"];
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to open the firewall for the Ollama service.
        Only enable if you need external access to the API.
      '';
    };

    enableGpuAcceleration = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enable GPU acceleration for model inference.
        Requires compatible GPU and drivers (NVIDIA CUDA or AMD ROCm).

        For NVIDIA: Ensure nvidia-docker and nvidia-container-toolkit are configured.
        For AMD: Ensure ROCm is installed and configured.
      '';
    };

    resourceLimits = {
      maxMemory = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Maximum memory usage for the Ollama service (systemd MemoryMax).
          Use suffixes like "8G", "16G", etc.
          Set to null for no limit.
        '';
        example = "16G";
      };

      maxCpuPercent = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = ''
          Maximum CPU usage percentage (systemd CPUQuota).
          Value between 1-100. Set to null for no limit.
        '';
        example = 80;
      };
    };

    backup = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable automatic backup of custom models and configuration";
      };

      destination = mkOption {
        type = types.str;
        default = "/backup/ollama";
        description = "Backup destination directory";
      };

      schedule = mkOption {
        type = types.str;
        default = "daily";
        description = "Backup schedule (systemd timer format)";
      };
    };

    monitoring = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable monitoring and health checks";
      };

      healthCheckInterval = mkOption {
        type = types.str;
        default = "30s";
        description = "Health check interval";
      };
    };
  };

  config = mkIf cfg.enable {
    # Ensure the Ollama package is available in the system
    environment.systemPackages = [cfg.package];

    # User and group configuration
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      home = cfg.dataDir;
      createHome = true;
      description = "Ollama service user";
      shell = pkgs.bash;
    };

    users.groups.${cfg.group} = {};

    # GPU support configuration
    hardware.opengl = mkIf cfg.enableGpuAcceleration {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    # NVIDIA GPU support
    services.xserver.videoDrivers = mkIf (cfg.enableGpuAcceleration && config.hardware.nvidia.modesetting.enable) ["nvidia"];

    # AMD GPU support
    systemd.packages = mkIf (cfg.enableGpuAcceleration && config.hardware.amdgpu.opencl.enable) [pkgs.rocmPackages.clr];

    # Main Ollama service
    systemd.services.ollama = {
      description = "Ollama Local LLM Service";
      wantedBy = ["multi-user.target"];
      after = ["network-online.target"];
      wants = ["network-online.target"];

      environment =
        {
          OLLAMA_HOST = "${cfg.host}:${toString cfg.port}";
          OLLAMA_MODELS = "${cfg.dataDir}/models";
          OLLAMA_RUNNERS_DIR = "${cfg.dataDir}/runners";
        }
        // cfg.environmentVariables;

      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/ollama serve";
        User = cfg.user;
        Group = cfg.group;
        Restart = "always";
        RestartSec = "3";

        # Security hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = mkIf (!cfg.enableGpuAcceleration) true;
        ProtectHostname = true;
        ProtectClock = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = ["AF_UNIX" "AF_INET" "AF_INET6"];
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        RemoveIPC = true;

        # Resource limits
        MemoryMax = mkIf (cfg.resourceLimits.maxMemory != null) cfg.resourceLimits.maxMemory;
        CPUQuota = mkIf (cfg.resourceLimits.maxCpuPercent != null) "${toString cfg.resourceLimits.maxCpuPercent}%";

        # File system access
        ReadWritePaths = [cfg.dataDir];
        StateDirectory = "ollama";
        CacheDirectory = "ollama";
        LogsDirectory = "ollama";

        # GPU access for NVIDIA
        SupplementaryGroups = mkIf (cfg.enableGpuAcceleration && config.hardware.nvidia.modesetting.enable) ["video" "render"];

        # For AMD GPU access, allow access to /dev/dri
        DeviceAllow = mkIf (cfg.enableGpuAcceleration && config.hardware.amdgpu.opencl.enable) [
          "/dev/dri"
          "/dev/kfd rw"
        ];
      };

      # Ensure data directory exists with correct permissions
      preStart = ''
        mkdir -p ${cfg.dataDir}/{models,runners}
        chown -R ${cfg.user}:${cfg.group} ${cfg.dataDir}
        chmod 755 ${cfg.dataDir}
      '';
    };

    # Model download service (runs after ollama is up)
    systemd.services.ollama-model-download = mkIf (cfg.models != []) {
      description = "Download Ollama Models";
      wantedBy = ["multi-user.target"];
      after = ["ollama.service"];
      wants = ["ollama.service"];

      environment = {
        OLLAMA_HOST = "${cfg.host}:${toString cfg.port}";
      };

      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = cfg.group;
        RemainAfterExit = true;
        TimeoutStartSec = "30min"; # Models can be large
      };

      script = ''
        # Wait for Ollama to be ready
        echo "Waiting for Ollama service to be ready..."
        while ! ${cfg.package}/bin/ollama list >/dev/null 2>&1; do
          sleep 2
        done

        echo "Ollama is ready. Downloading configured models..."
        ${concatMapStringsSep "\n" (model: ''
            echo "Downloading model: ${model}"
            if ! ${cfg.package}/bin/ollama list | grep -q "^${model}"; then
              ${cfg.package}/bin/ollama pull "${model}"
            else
              echo "Model ${model} already exists, skipping download"
            fi
          '')
          cfg.models}

        echo "Model download completed"
      '';
    };

    # Health check service
    systemd.services.ollama-health-check = mkIf cfg.monitoring.enable {
      description = "Ollama Health Check";
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = cfg.group;
        ExecStart = pkgs.writeShellScript "ollama-health-check" ''
          # Basic health check - verify API is responding
          if ! ${pkgs.curl}/bin/curl -f -s "http://${cfg.host}:${toString cfg.port}/api/tags" >/dev/null; then
            echo "Ollama health check failed - API not responding"
            exit 1
          fi

          # Check if we can list models
          if ! ${cfg.package}/bin/ollama list >/dev/null 2>&1; then
            echo "Ollama health check failed - cannot list models"
            exit 1
          fi

          echo "Ollama health check passed"
        '';
      };
    };

    # Health check timer
    systemd.timers.ollama-health-check = mkIf cfg.monitoring.enable {
      description = "Ollama Health Check Timer";
      wantedBy = ["timers.target"];
      timerConfig = {
        OnBootSec = "5min";
        OnUnitActiveSec = cfg.monitoring.healthCheckInterval;
        Persistent = true;
      };
    };

    # Backup service
    systemd.services.ollama-backup = mkIf cfg.backup.enable {
      description = "Backup Ollama Data";
      serviceConfig = {
        Type = "oneshot";
        User = "root"; # Need root for backup operations
        ExecStart = pkgs.writeShellScript "ollama-backup" ''
          mkdir -p "${cfg.backup.destination}"

          # Backup custom models and configuration (excluding large standard models)
          echo "Starting Ollama backup to ${cfg.backup.destination}"

          # Create timestamped backup
          backup_dir="${cfg.backup.destination}/$(date +%Y%m%d_%H%M%S)"
          mkdir -p "$backup_dir"

          # Backup configuration and custom content
          if [ -d "${cfg.dataDir}" ]; then
            # Only backup manifests and small configuration files, not the large model blobs
            find "${cfg.dataDir}" -name "*.json" -o -name "*.yaml" -o -name "*.txt" | \
              ${pkgs.rsync}/bin/rsync -av --files-from=- / "$backup_dir/"
          fi

          # Keep only last 7 backups
          find "${cfg.backup.destination}" -maxdepth 1 -type d -name "????????_??????" | \
            sort -r | tail -n +8 | xargs -r rm -rf

          echo "Ollama backup completed"
        '';
      };
    };

    # Backup timer
    systemd.timers.ollama-backup = mkIf cfg.backup.enable {
      description = "Ollama Backup Timer";
      wantedBy = ["timers.target"];
      timerConfig = {
        OnCalendar = cfg.backup.schedule;
        Persistent = true;
      };
    };

    # Firewall configuration
    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [cfg.port];
    };

    # Log rotation
    services.logrotate.settings.ollama = {
      files = ["/var/log/ollama/*.log"];
      frequency = "daily";
      rotate = 7;
      compress = true;
      delaycompress = true;
      missingok = true;
      notifempty = true;
      create = "644 ${cfg.user} ${cfg.group}";
    };

    # Add helpful aliases
    environment.shellAliases = {
      ollama-status = "systemctl status ollama";
      ollama-logs = "journalctl -u ollama -f";
      ollama-models = "${cfg.package}/bin/ollama list";
      ollama-pull = "${cfg.package}/bin/ollama pull";
      ollama-run = "${cfg.package}/bin/ollama run";
    };

    # Ensure proper permissions for model directory
    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 0755 ${cfg.user} ${cfg.group} -"
      "d ${cfg.dataDir}/models 0755 ${cfg.user} ${cfg.group} -"
      "d ${cfg.dataDir}/runners 0755 ${cfg.user} ${cfg.group} -"
    ];
  };
}
