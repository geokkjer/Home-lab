{
  config,
  lib,
  pkgs,
  ...
}: {
  # Optional configuration to disable automatic RNNoise filter
  # This can be imported if the automatic filter causes distortion

  services.pipewire = {
    extraConfig.pipewire."15-disable-auto-rnnoise" = {
      "context.modules" = [
        # Commenting out the automatic RNNoise filter
        # Users should use EasyEffects for manual noise suppression instead
        # {
        #   name = "libpipewire-module-filter-chain";
        #   args = {
        #     "node.description" = "Noise Canceling Source";
        #     # ... rest of RNNoise config
        #   };
        # }
      ];
    };
  };
}
