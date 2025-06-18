{
  config,
  lib,
  pkgs,
  ...
}: {
  # Desktop entries for quick audio management access
  environment.etc = {
    # Desktop entry for EasyEffects
    "xdg/autostart/easyeffects.desktop".text = ''
      [Desktop Entry]
      Name=EasyEffects
      Comment=Audio effects for PipeWire applications
      Icon=easyeffects
      Exec=easyeffects --gapplication-service
      Terminal=false
      Type=Application
      Categories=AudioVideo;Audio;
      StartupNotify=true
      X-GNOME-Autostart-enabled=true
    '';

    # Custom desktop entry for audio control center
    "applications/audio-control-center.desktop".text = ''
      [Desktop Entry]
      Version=1.0
      Type=Application
      Name=Audio Control Center
      Comment=Centralized audio management
      Icon=audio-volume-high
      Categories=AudioVideo;Audio;Settings;
      Keywords=audio;sound;volume;pipewire;pulseaudio;
      StartupNotify=true
      Terminal=false
      Exec=sh -c 'if command -v easyeffects >/dev/null 2>&1; then easyeffects; elif command -v pavucontrol >/dev/null 2>&1; then pavucontrol; elif command -v pwvucontrol >/dev/null 2>&1; then pwvucontrol; else helvum; fi'
    '';
  };

  # Create a script for easy audio management
  environment.systemPackages = with pkgs; [
    (writeShellScriptBin "audio-setup" ''
      #!/bin/bash

      echo "🎵 Audio Control Center"
      echo "======================"
      echo ""
      echo "Available audio applications:"
      echo ""

      if command -v easyeffects >/dev/null 2>&1; then
        echo "  1. EasyEffects - Audio effects and noise suppression"
      fi

      if command -v pavucontrol >/dev/null 2>&1; then
        echo "  2. PulseAudio Volume Control - Volume and device management"
      fi

      if command -v pwvucontrol >/dev/null 2>&1; then
        echo "  3. PipeWire Volume Control - Native PipeWire control"
      fi

      if command -v helvum >/dev/null 2>&1; then
        echo "  4. Helvum - PipeWire patchbay"
      fi

      if command -v qpwgraph >/dev/null 2>&1; then
        echo "  5. qpwgraph - Qt PipeWire graph manager"
      fi

      echo ""
      echo "  🔧 Audio Tools:"
      echo "     • pw-top - Monitor PipeWire performance"
      echo "     • pw-dump - Inspect PipeWire objects"
      echo "     • pw-metadata - View/set PipeWire metadata"
      echo "     • wpctl - WirePlumber control utility"
      echo ""

      echo "Choose an option (1-5) or press Enter for EasyEffects:"
      read -r choice

      case $choice in
        1|"")
          if command -v easyeffects >/dev/null 2>&1; then
            echo "Starting EasyEffects..."
            easyeffects
          else
            echo "EasyEffects not found!"
          fi
          ;;
        2)
          if command -v pavucontrol >/dev/null 2>&1; then
            echo "Starting PulseAudio Volume Control..."
            pavucontrol
          else
            echo "pavucontrol not found!"
          fi
          ;;
        3)
          if command -v pwvucontrol >/dev/null 2>&1; then
            echo "Starting PipeWire Volume Control..."
            pwvucontrol
          else
            echo "pwvucontrol not found!"
          fi
          ;;
        4)
          if command -v helvum >/dev/null 2>&1; then
            echo "Starting Helvum..."
            helvum
          else
            echo "Helvum not found!"
          fi
          ;;
        5)
          if command -v qpwgraph >/dev/null 2>&1; then
            echo "Starting qpwgraph..."
            qpwgraph
          else
            echo "qpwgraph not found!"
          fi
          ;;
        *)
          echo "Invalid choice"
          ;;
      esac
    '')

    (writeShellScriptBin "microphone-test" ''
      #!/bin/bash

      echo "🎤 Microphone Test & Setup"
      echo "=========================="
      echo ""

      # Check if PipeWire is running
      if ! pgrep -x pipewire >/dev/null; then
        echo "❌ PipeWire is not running!"
        exit 1
      fi

      echo "✅ PipeWire is running"

      # Check for RNNoise
      if ls /nix/store/*/lib/ladspa/librnnoise_ladspa.so >/dev/null 2>&1; then
        echo "✅ RNNoise plugin is available"
      else
        echo "⚠️  RNNoise plugin not found"
      fi

      # List audio sources
      echo ""
      echo "📺 Available audio sources:"
      wpctl status | grep -A 20 "Audio Sources"

      echo ""
      echo "🔊 Available audio sinks:"
      wpctl status | grep -A 20 "Audio Sinks"

      echo ""
      echo "Would you like to:"
      echo "  1. Test microphone input"
      echo "  2. Open EasyEffects for noise suppression setup"
      echo "  3. Show detailed audio device information"
      echo "  4. Monitor audio levels"
      echo ""
      read -p "Choose an option (1-4): " choice

      case $choice in
        1)
          echo "Recording 5 seconds of audio for playback test..."
          echo "Speak into your microphone now!"
          if command -v arecord >/dev/null 2>&1 && command -v aplay >/dev/null 2>&1; then
            arecord -d 5 -f cd /tmp/mic_test.wav && echo "Playing back recording..." && aplay /tmp/mic_test.wav
            rm -f /tmp/mic_test.wav
          else
            echo "❌ ALSA utilities not available"
          fi
          ;;
        2)
          if command -v easyeffects >/dev/null 2>&1; then
            echo "Opening EasyEffects..."
            easyeffects &
          else
            echo "❌ EasyEffects not found"
          fi
          ;;
        3)
          echo ""
          echo "🔍 Detailed audio information:"
          echo ""
          pw-dump | jq '.[] | select(.info.props."media.class" == "Audio/Source" or .info.props."media.class" == "Audio/Sink") | {id: .id, name: .info.props."node.name", description: .info.props."node.description", class: .info.props."media.class"}'
          ;;
        4)
          echo "Monitoring audio levels (Ctrl+C to stop)..."
          if command -v pw-top >/dev/null 2>&1; then
            pw-top
          else
            echo "Monitoring with wpctl..."
            while true; do
              clear
              wpctl status
              sleep 2
            done
          fi
          ;;
        *)
          echo "Invalid choice"
          ;;
      esac
    '')
  ];
}
