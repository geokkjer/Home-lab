{
  inputs,
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.services.antigravity;
in {
  options.services.antigravity = {
    enable = lib.mkEnableOption "Google Antigravity IDE";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      inputs.antigravity-nix.packages.${pkgs.system}.default
    ];
  };
}
