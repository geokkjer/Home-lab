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

    # Environment variables for optimal CPU performance
    environmentVariables = {
      # Allow CORS from local network (adjust as needed)
      OLLAMA_ORIGINS = "http://localhost,http://127.0.0.1,http://grey-area.lan,http://grey-area";

      # Optimized context window for TaskMaster AI
      OLLAMA_CONTEXT_LENGTH = "8192";

      # CPU-optimized parallel processing
      OLLAMA_NUM_PARALLEL = "4";
      OLLAMA_MAX_LOADED_MODELS = "3";

      # Increased queue for better throughput
      OLLAMA_MAX_QUEUE = "512";

      # CPU performance optimizations
      OLLAMA_FLASH_ATTENTION = "1";
      OLLAMA_KV_CACHE_TYPE = "q8_0"; # More memory efficient than f16

      # Force specific CPU library for optimal performance
      OLLAMA_LLM_LIBRARY = "cpu_avx2";

      # Enable CPU optimizations
      OLLAMA_CPU_HBM = "0"; # Disable unless you have high bandwidth memory
      OLLAMA_OPENMP = "1"; # Enable OpenMP for parallel processing

      # Disable debug for performance
      OLLAMA_DEBUG = "0";
    };

    openFirewall = true; # Set to true if you want to allow external access

    # GPU acceleration (enable if grey-area has a compatible GPU)
    #enableGpuAcceleration = false; # Set to true if NVIDIA/AMD GPU available
  };

  # Apply resource limits and CPU optimizations using systemd overrides
  systemd.services.ollama = {
    serviceConfig = {
      # Memory management for CPU inference
      MemoryMax = "20G";
      MemoryHigh = "16G";
      MemorySwapMax = "4G";

      # CPU optimization
      CPUQuota = "800%";
      CPUWeight = "100";

      # I/O optimization for model loading
      IOWeight = "100";
      IOSchedulingClass = "1";
      IOSchedulingPriority = "2";

      # Process limits
      LimitNOFILE = "65536";
      LimitNPROC = "8192";

      # Enable CPU affinity if needed (comment out if not beneficial)
      # CPUAffinity = "0-7";
    };

    # Additional environment variables for CPU optimization
    environment = {
      # OpenMP threading
      OMP_NUM_THREADS = "8";
      OMP_PROC_BIND = "close";
      OMP_PLACES = "cores";

      # MKL optimizations (if available)
      MKL_NUM_THREADS = "8";
      MKL_DYNAMIC = "false";

      # BLAS threading
      OPENBLAS_NUM_THREADS = "8";
      VECLIB_MAXIMUM_THREADS = "8";
    };
  };

  # Add useful packages for AI development
  environment.systemPackages = with pkgs; [
    # CLI clients for testing
    curl
    jq
  ];
}
