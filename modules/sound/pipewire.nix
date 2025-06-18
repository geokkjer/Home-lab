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

    # Add noise suppression and audio processing packages
    extraPackages = with pkgs; [
      rnnoise-plugin # RNNoise noise suppression
      easyeffects # Modern audio effects and filters
    ];
  };

  # Install audio management and GUI applications
  environment.systemPackages = with pkgs; [
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

  # System-wide PipeWire configuration
  environment.etc = {
    # Main PipeWire configuration
    "pipewire/pipewire.conf.d/10-noise-suppression.conf".text = ''
      context.properties = {
        default.clock.rate = 48000
        default.clock.quantum = 1024
        default.clock.min-quantum = 32
        default.clock.max-quantum = 2048
      }

      context.modules = [
        {
          name = libpipewire-module-filter-chain
          args = {
            node.description = "Noise Canceling Source"
            media.name = "Noise Canceling Source"
            filter.graph = {
              nodes = [
                {
                  type = ladspa
                  name = rnnoise
                  plugin = ${pkgs.rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so
                  label = noise_suppressor_stereo
                  control = {
                    "VAD Threshold (%)" = 50.0
                    "VAD Grace Period (ms)" = 200
                    "Retroactive VAD Grace (ms)" = 0
                  }
                }
              ]
            }
            capture.props = {
              node.name = "capture.rnnoise_source"
              node.passive = true
              audio.rate = 48000
            }
            playback.props = {
              node.name = "rnnoise_source"
              media.class = "Audio/Source"
              audio.rate = 48000
            }
          }
        }
      ]
    '';

    # WirePlumber configuration for noise suppression
    "wireplumber/wireplumber.conf.d/51-noise-suppression.conf".text = ''
      monitor.alsa.rules = [
        {
          matches = [
            {
              device.name = "~alsa_card.*"
            }
          ]
          actions = {
            update-props = {
              device.profile-set = "auto"
              device.auto-profile = true
            }
          }
        }
      ]

      monitor.bluez.rules = [
        {
          matches = [
            {
              device.name = "~bluez_card.*"
            }
          ]
          actions = {
            update-props = {
              bluez5.auto-connect = [ "hfp_hf" "hsp_hs" "a2dp_sink" ]
              bluez5.hw-volume = [ "hfp_hf" "hsp_hs" "a2dp_sink" ]
            }
          }
        }
      ]
    '';

    # Audio quality and latency optimization
    "pipewire/pipewire-pulse.conf.d/10-audio-quality.conf".text = ''
      pulse.properties = {
        pulse.min.req = 32/48000
        pulse.default.req = 1024/48000
        pulse.min.quantum = 32/48000
        pulse.max.quantum = 2048/48000
      }

      stream.properties = {
        node.latency = 1024/48000
        resample.quality = 4
        channelmix.normalize = false
        channelmix.mix-lfe = false
        session.suspend-timeout-seconds = 0
      }
    '';
  };

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
