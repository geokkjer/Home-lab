{
  config,
  pkgs,
  ...
}: {
  # Music software module for importing from packages
  # This provides CDP8 and SoundThread packages

  # Import music software packages
  environment.systemPackages = with pkgs.callPackage ../../packages/music-software.nix {}; [
    # SoundThread with bundled CDP tools (includes all 220 CDP binaries)
    # Wrap to expose CDP tools directly in bin directory
    (
      let
        st = soundthread;
      in
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
