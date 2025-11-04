{
  stdenv,
  fetchurl,
  lib,
  xorg,
  libGL,
  alsa-lib,
  pulseaudio,
  libxcb,
  libX11,
  gcc-unwrapped,
  makeWrapper,
  patchelf,
  libxkbcommon,
  libxkbcommon-x11,
  wayland,
}:
stdenv.mkDerivation rec {
  pname = "soundthread";
  version = "0.4.0-beta";

  # Download prebuilt Linux binary
  # Note: SoundThread includes patched CDP binaries (cdprogs_linux.tar.gz)
  # Following upstream approach to use included CDP rather than external package
  src = fetchurl {
    url = "https://github.com/j-p-higgins/SoundThread/releases/download/v${version}/SoundThread_v0-4-0-beta_linux_x86_64.tar.gz";
    sha256 = "6899693155c4941316baf546b0f7b406e7de0163e32a815cc4e865acc91b1f09";
  };

  nativeBuildInputs = [makeWrapper patchelf];

  buildInputs = [
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXinerama
    xorg.libXi
    xorg.libXxf86vm
    xorg.libXext
    xorg.libXdmcp
    xorg.libXrender
    libGL
    alsa-lib
    pulseaudio
    libxcb
    libxkbcommon
    libxkbcommon-x11
    wayland
    gcc-unwrapped.lib
  ];

  # Extract the archive
  unpackPhase = ''
    tar -xzf $src
    # The archive typically contains a SoundThread directory or binary
    ls -la
  '';

  # No configuration or building needed since it's a prebuilt binary
  configurePhase = "true";
  buildPhase = "true";

  # Installation phase: copy binary and set up wrapper
  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/lib
    mkdir -p $out/cdp/bin

    # Extract and install SoundThread GUI binary
    if [ -f "SoundThread_v0-4-0-beta_linux_x86_64/SoundThread.x86_64" ]; then
      cp SoundThread_v0-4-0-beta_linux_x86_64/SoundThread.x86_64 $out/bin/SoundThread
    elif [ -f "SoundThread.x86_64" ]; then
      cp SoundThread.x86_64 $out/bin/SoundThread
    elif [ -f "SoundThread/SoundThread.x86_64" ]; then
      cp SoundThread/SoundThread.x86_64 $out/bin/SoundThread
    else
      echo "Error: Could not find SoundThread binary"
      exit 1
    fi

    chmod +x $out/bin/SoundThread

    # Patch the binary to use NixOS dynamic linker
    patchelf --set-interpreter ${stdenv.cc.bintools.dynamicLinker} $out/bin/SoundThread

    # Extract and install bundled CDP binaries
    # SoundThread includes patched CDP binaries in cdprogs_linux.tar.gz
    cdpTarball=""
    if [ -f "SoundThread_v0-4-0-beta_linux_x86_64/cdprogs_linux.tar.gz" ]; then
      cdpTarball="SoundThread_v0-4-0-beta_linux_x86_64/cdprogs_linux.tar.gz"
    elif [ -f "cdprogs_linux.tar.gz" ]; then
      cdpTarball="cdprogs_linux.tar.gz"
    fi

    if [ -n "$cdpTarball" ]; then
      # Extract CDP tarball (creates cdprogs_linux/ directory)
      tar -xzf "$cdpTarball" -C $out/cdp/
      # Move binaries to bin directory for cleaner structure
      if [ -d "$out/cdp/cdprogs_linux" ]; then
        mv $out/cdp/cdprogs_linux/* $out/cdp/bin/
        rmdir $out/cdp/cdprogs_linux
      fi
      chmod +x $out/cdp/bin/* 2>/dev/null || true

      # Patch all CDP binaries to use NixOS dynamic linker
      for tool in $out/cdp/bin/*; do
        if file "$tool" | grep -q "ELF"; then
          patchelf --set-interpreter ${stdenv.cc.bintools.dynamicLinker} "$tool" 2>/dev/null || true
        fi
      done
    else
      echo "Warning: Could not find bundled CDP binaries (cdprogs_linux.tar.gz)"
    fi
  '';

  dontStrip = true;
  dontPatchShebangs = true;

  # Post-install phase: wrap the binary to handle CDP path and dependencies
  postInstall = ''
    # Use bundled CDP binaries that come with SoundThread
    # These are patched versions maintained by the SoundThread author
    cdpBinPath="$out/cdp/bin"

    # Create a wrapper script that sets the necessary environment for SoundThread
    wrapProgram $out/bin/SoundThread \
      --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [
      libGL
      alsa-lib
      pulseaudio
      xorg.libX11
      xorg.libXcursor
      xorg.libXrandr
      xorg.libXinerama
      xorg.libXi
      xorg.libXxf86vm
      xorg.libXext
      xorg.libXdmcp
      xorg.libXrender
      libxcb
      libxkbcommon
      libxkbcommon-x11
      wayland
      gcc-unwrapped.lib
    ]}" \
      --prefix PATH : "$cdpBinPath" \
      --set CDP_PATH "$cdpBinPath" \
      --set XDG_DATA_HOME "$out/share"
  '';

  # Instead of creating wrapper scripts that get lost, provide a separate bin directory for CDP tools
  # This will be referenced directly from the nix store path
  postFixup = ''
    # Create symlinks for all CDP tools in a subdirectory accessible to users
    mkdir -p $out/cdp-bin
    for tool in "$out/cdp/bin"/*; do
      toolName=$(basename "$tool")
      ln -s "$tool" "$out/cdp-bin/$toolName"
    done
  '';

  meta = with lib; {
    description = "Node-based GUI for The Composers Desktop Project (CDP)";
    longDescription = ''
      SoundThread is a cross-platform user interface for The Composers Desktop
      Project (CDP) suite of sound manipulation tools. It allows for modular
      style routing of various CDP processes to quickly build up complex Threads
      for extensive sound manipulation.

      This package uses the patched CDP binaries bundled with SoundThread
      (cdprogs_linux.tar.gz), which includes bug fixes maintained by the
      SoundThread author. Following the upstream approach of using the included
      CDP binaries rather than external packages ensures compatibility and
      access to all patches applied by the maintainer.
    '';
    homepage = "https://github.com/j-p-higgins/SoundThread";
    downloadPage = "https://github.com/j-p-higgins/SoundThread/releases";
    license = licenses.mit;
    maintainers = with maintainers; [
      /*
      add your name here
      */
    ];
    platforms = platforms.linux;
    broken = false;
  };
}
