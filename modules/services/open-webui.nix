# Open WebUI Service Configuration
#
# This module provides Open WebUI configuration for interacting with Ollama
# Open WebUI provides a user-friendly web interface for local LLMs
{
  config,
  lib,
  pkgs,
  ...
}: {
  # Enable the built-in NixOS open-webui service
  services.open-webui = {
    enable = true;
    port = 8080;
    host = "0.0.0.0";

    environment = {
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";
      OLLAMA_API_BASE_URL = "http://127.0.0.1:11434";
      # Disable authentication for easier development access
      WEBUI_AUTH = "False";
    };
  };

  # Open firewall for web interface
  networking.firewall.allowedTCPPorts = [ 8080 ];
}
