{ pkgs, configs, ... }:
{
  services.openvscode-server = {
    enable = true;
    telemetryLevel = "off";
    port = 3003;
    host = "0.0.0.0";
  };
}
