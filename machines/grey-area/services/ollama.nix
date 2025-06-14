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
  # Enable Ollama service with appropriate configuration for grey-area
  services.ollama = {
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
      OLLAMA_DEBUG = "0";
    };

    openFirewall = true; # Set to true if you want to allow external access

    # GPU acceleration (enable if grey-area has a compatible GPU)
    #enableGpuAcceleration = false; # Set to true if NVIDIA/AMD GPU available
  };

  # Apply resource limits using systemd overrides
  systemd.services.ollama = {
    serviceConfig = {
      MemoryMax = "20G";
      CPUQuota = "800%";
    };
  };

  # Add useful packages for AI development
  environment.systemPackages = with pkgs; [
    # CLI clients for testing
    curl
    jq
  ];
}
