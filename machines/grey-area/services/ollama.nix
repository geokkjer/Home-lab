# Ollama Service Configuration for Grey Area
#
# This service configuration deploys Ollama on the grey-area application server.
# Ollama provides local LLM hosting with an OpenAI-compatible API for development
# assistance, code review, and general AI tasks.
{
  config,
  lib,
  pkgs,
  ...
}: {
  # Import the home lab Ollama module
  imports = [
    ../../../modules/services/ollama.nix
  ];

  # Enable Ollama service with appropriate configuration for grey-area
  services.homelab-ollama = {
    enable = true;

    # Network configuration - localhost only for security by default
    host = "0.0.0.0";
    port = 11434;

    # Environment variables for optimal performance
    environmentVariables = {
      # Allow CORS from local network (adjust as needed)
      OLLAMA_ORIGINS = "http://localhost,http://127.0.0.1,http://grey-area.lan,http://grey-area";

      # Larger context window for development tasks
      OLLAMA_CONTEXT_LENGTH = "4096";

      # Allow multiple parallel requests
      OLLAMA_NUM_PARALLEL = "2";

      # Increase queue size for multiple users
      OLLAMA_MAX_QUEUE = "256";

      # Enable debug logging initially for troubleshooting
      OLLAMA_DEBUG = "1";
    };

    # Automatically download essential models
    models = [
      # General purpose model - good balance of size and capability
      "llama3.3:8b"

      # Code-focused model for development assistance
      "codellama:7b"

      # Fast, efficient model for quick queries
      "mistral:7b"
    ];

    # Resource limits to prevent impact on other services
    resourceLimits = {
      # Limit memory usage to prevent OOM issues with Jellyfin/other services
      maxMemory = "12G";

      # Limit CPU usage to maintain responsiveness for other services
      maxCpuPercent = 75;
    };

    # Enable monitoring and health checks
    monitoring = {
      enable = true;
      healthCheckInterval = "60s";
    };

    # Enable backup for custom models and configuration
    backup = {
      enable = true;
      destination = "/var/backup/ollama";
      schedule = "weekly"; # Weekly backup is sufficient for models
    };

    openFirewall = true; # Set to true if you want to allow external access

    # GPU acceleration (enable if grey-area has a compatible GPU)
    #enableGpuAcceleration = false; # Set to true if NVIDIA/AMD GPU available
  };

  # Create backup directory with proper permissions
  systemd.tmpfiles.rules = [
    "d /var/backup/ollama 0755 root root -"
  ];

  # Optional: Create a simple web interface using a lightweight tool
  # This could be added later if desired for easier model management

  # Add useful packages for AI development
  environment.systemPackages = with pkgs; [
    # CLI clients for testing
    curl
    jq

    # Python packages for AI development (optional)
    (python3.withPackages (ps:
      with ps; [
        requests
        openai # For OpenAI-compatible API testing
      ]))
  ];

  # Create a simple script for testing Ollama
  environment.etc."ollama-test.sh" = {
    text = ''
      #!/usr/bin/env bash
      # Simple test script for Ollama service

      echo "Testing Ollama service..."

      # Test basic connectivity
      if curl -s http://localhost:11434/api/tags >/dev/null; then
        echo "✓ Ollama API is responding"
      else
        echo "✗ Ollama API is not responding"
        exit 1
      fi

      # List available models
      echo "Available models:"
      curl -s http://localhost:11434/api/tags | jq -r '.models[]?.name // "No models found"'

      # Simple generation test if models are available
      if curl -s http://localhost:11434/api/tags | jq -e '.models | length > 0' >/dev/null; then
        echo "Testing text generation..."
        model=$(curl -s http://localhost:11434/api/tags | jq -r '.models[0].name')
        response=$(curl -s -X POST http://localhost:11434/api/generate \
          -H "Content-Type: application/json" \
          -d "{\"model\": \"$model\", \"prompt\": \"Hello, world!\", \"stream\": false}" | \
          jq -r '.response // "No response"')
        echo "Response from $model: $response"
      else
        echo "No models available for testing"
      fi
    '';
    mode = "0755";
  };

  # Firewall rule comments for documentation
  # To enable external access later, you would:
  # 1. Set services.homelab-ollama.openFirewall = true;
  # 2. Or configure a reverse proxy (recommended for production)

  # Example reverse proxy configuration (commented out):
  /*
  services.nginx = {
    enable = true;
    virtualHosts."ollama.grey-area.lan" = {
      listen = [
        { addr = "0.0.0.0"; port = 8080; }
      ];
      locations."/" = {
        proxyPass = "http://127.0.0.1:11434";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };
    };
  };
  */
}
