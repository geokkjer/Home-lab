{
  config,
  pkgs,
  ...
}: {
  # Music Production Software Module
  # This module provides a complete music production environment with:
  # - CDP8 (Composers Desktop Project)
  # - SoundThread (Node-based GUI for CDP)
  # - Samplebrain (Sample manipulation tool)
  # - VCV Rack (Modular synthesizer)
  # - Audacity (Audio editor)
  # - SuperCollider (Audio synthesis language)

  # Import music software packages
  environment.systemPackages = with pkgs; let
    musicPkgs = callPackage ../../packages/music-software.nix {};
    st = musicPkgs.soundthread;
    sb = samplebrain;
  in [
    # === CDP & SoundThread Integration ===
    # SoundThread with bundled CDP tools (includes all 220 CDP binaries)
    # Wrap to expose CDP tools directly in bin directory
    (
      pkgs.symlinkJoin {
        name = "soundthread-with-cdp";
        paths = [st];
        postBuild = ''
          mkdir -p $out/bin
          for tool in ${st}/cdp-bin/*; do
            ln -sf "$tool" "$out/bin/$(basename $tool)"
          done
        '';
      }
    )
    # SoundThread desktop application wrapper
    (
      pkgs.makeDesktopItem {
        name = "soundthread";
        desktopName = "SoundThread";
        exec = "${st}/bin/SoundThread";
        icon = "soundthread";
        comment = "Node-based GUI for The Composers Desktop Project (CDP)";
        categories = ["Audio" "AudioVideo"];
        terminal = false;
      }
    )

    # === Sample Manipulation ===
    # Samplebrain desktop application wrapper
    (
      pkgs.makeDesktopItem {
        name = "samplebrain";
        desktopName = "Samplebrain";
        exec = "${sb}/bin/samplebrain";
        icon = "samplebrain";
        comment = "Interactive tool for exploring multi-sample instruments";
        categories = ["Audio" "AudioVideo" "Music"];
        terminal = false;
      }
    )

    # === Modular Synthesis & General Audio ===
    vcv-rack # Modular synthesizer
    audacity # Multi-track audio editor

    # === Audio Synthesis & Composition ===
    supercollider-with-plugins # Audio synthesis language and environment
  ];

  # Export CDP_PATH environment variable
  environment.sessionVariables = {
    CDP_PATH = "${(pkgs.callPackage ../../packages/music-software.nix {}).soundthread}/cdp/bin";
  };

  # Create a profile.d script to add CDP tools to PATH
  environment.etc."profile.d/soundthread-cdp.sh".text = ''
    export PATH="${(pkgs.callPackage ../../packages/music-software.nix {}).soundthread}/cdp-bin:$PATH"
  '';
}
