{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./easyeffects-presets.nix
    ./audio-desktop-integration.nix
  ];
  # Enable PipeWire with full audio stack
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    
    # Enable WirePlumber session manager
    wireplumber.enable = true;
    
    # Extra configuration for noise suppression
    extraConfig.pipewire."10-noise-suppression" = {
      "context.properties" = {
        "default.clock.rate" = 48000;
        "default.clock.quantum" = 1024;
        "default.clock.min-quantum" = 32;
        "default.clock.max-quantum" = 2048;
      };
      
      "context.modules" = [
        {
          name = "libpipewire-module-filter-chain";
          args = {
            "node.description" = "Noise Canceling Source";
            "media.name" = "Noise Canceling Source";
            "filter.graph" = {
              nodes = [
                {
                  type = "ladspa";
                  name = "rnnoise";
                  plugin = "${pkgs.rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so";
                  label = "noise_suppressor_stereo";
                  control = {
                    "VAD Threshold (%)" = 50.0;
                    "VAD Grace Period (ms)" = 200;
                    "Retroactive VAD Grace (ms)" = 0;
                  };
                }
              ];
            };
            "capture.props" = {
              "node.name" = "capture.rnnoise_source";
              "node.passive" = true;
              "audio.rate" = 48000;
            };
            "playback.props" = {
              "node.name" = "rnnoise_source";
              "media.class" = "Audio/Source";
              "audio.rate" = 48000;
            };
          };
        }
      ];
    };
  };

  # Install audio management and GUI applications
  environment.systemPackages = with pkgs; [
    # Noise suppression plugin
    rnnoise-plugin # RNNoise LADSPA plugin
    
    # Audio control and monitoring
    pavucontrol # PulseAudio volume control (works with PipeWire)
    helvum # Graphical patchbay for PipeWire
    qpwgraph # Qt-based PipeWire graph manager
    easyeffects # Audio effects and noise suppression GUI
    pwvucontrol # Native PipeWire volume control

    # Audio utilities
    wireplumber # WirePlumber session manager
    pipewire-pulse # PulseAudio compatibility
    pipecontrol # PipeWire control utility
    alsa-utils # ALSA utilities for testing

    # Validation script
    (writeShellScriptBin "validate-audio" (builtins.readFile ./validate-audio.sh))

    # Optional: Professional audio tools
    # qjackctl          # JACK control GUI (for JACK applications)
    # carla             # Audio plugin host
  ];

  # Enable real-time audio processing
  security.rtkit.enable = true;

  # Audio group for users
  users.groups.audio = {};

  # Set environment variables for better audio performance
  environment.variables = {
    # PipeWire environment variables
    PIPEWIRE_LATENCY = "1024/48000";
    # Ensure applications use PipeWire
    PULSE_RUNTIME_PATH = "/run/user/$UID/pulse";
  };

  # Enable additional audio-related services
  services = {
    # Enable udev rules for audio devices
    udev.packages = with pkgs; [
      alsa-utils
    ];
  };

  # User session configuration for audio
  systemd.user.services.pipewire-pulse.wantedBy = ["default.target"];
}
