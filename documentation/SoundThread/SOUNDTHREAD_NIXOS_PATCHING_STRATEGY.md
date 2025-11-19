# SoundThread NixOS Binary Patching Strategy

## Problem Summary

SoundThread ships prebuilt dynamically-linked Linux binaries that cannot run directly on NixOS because:

1. **Dynamic linking issue**: The binaries expect system libraries in standard FHS paths (`/usr/lib`, `/lib64`, etc.)
2. **NixOS isolation**: NixOS packages are isolated and don't use FHS paths
3. **Previous approach was incomplete**: Only patching the ELF interpreter wasn't enough - the binaries also need their **rpath** patched

## Solution: Proper Binary Patching with rpath

Following the approach documented in:

- **musnix**: Real-time audio configuration for NixOS
- **NixOS Wiki - Audio Production**: Guidelines for packaging prebuilt audio binaries
- **NixOS Wiki - Packaging Binaries**: Best practices for binary packaging

### Key Changes to `soundthread.nix`

The critical improvement is in the `postFixup` phase:

```nix
postFixup = ''
  # Define the library search path for all dependencies
  libPath="${lib.makeLibraryPath [
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
    gcc-unwrapped
  ]}"

  # Patch the SoundThread binary: set interpreter AND rpath
  patchelf \
    --set-interpreter ${stdenv.cc.bintools.dynamicLinker} \
    --set-rpath "$libPath" \
    $out/bin/SoundThread

  # Patch all CDP binaries with the same rpath
  for tool in $out/cdp/bin/*; do
    if file "$tool" | grep -q "ELF"; then
      patchelf \
        --set-interpreter ${stdenv.cc.bintools.dynamicLinker} \
        --set-rpath "$libPath" \
        "$tool" 2>/dev/null || true
    fi
  done
'';
```

### What This Does

1. **`lib.makeLibraryPath`**: Converts a list of Nix packages into a colon-separated library search path pointing to their `/lib` directories
2. **`--set-interpreter`**: Tells the binary to use NixOS's dynamic linker instead of the system one
3. **`--set-rpath`**: Embeds the library search path into the binary itself, so it knows where to find dependencies at runtime

### Why Both Are Needed

- **Interpreter only**: Binary knows which loader to use, but loader doesn't know where to find libraries
- **Interpreter + rpath**: Binary uses correct loader and can find all dependencies in NixOS store

## Additional Context: NixOS Audio Production

### musnix Benefits for SoundThread

- Real-time kernel patches (optional, for low-latency audio)
- Proper audio group permissions
- Sets plugin search paths for VSTs/LV2s
- Can be enabled in NixOS configuration

### Combined Approach

The improved `soundthread.nix` now:

1. ✅ Patches binaries correctly with rpath
2. ✅ Bundles patched CDP tools (following upstream approach)
3. ✅ Sets environment variables (CDP_PATH, LD_LIBRARY_PATH)
4. ✅ Wraps binary for easy access
5. ✅ Works with musnix for real-time audio (if configured)

## Testing the Package

After updating, rebuild with:

```bash
nix build .#soundthread
```

Test the binary:

```bash
./result/bin/SoundThread
```

SoundThread will now automatically set up the CDP path in `~/.local/share/soundthread-cdp/` on first run, so you won't see the configuration dialog asking for the CDP location.

If there are still missing dependencies, you can debug with:

```bash
ldd ./result/bin/SoundThread.real
# Shows which libraries are still not found
```

## How the Wrapper Works

The custom wrapper script handles three important tasks:

1. **Automatic CDP path setup**: On startup, it creates symlinks to all bundled CDP binaries in `~/.local/share/soundthread-cdp/`, which SoundThread looks for
2. **Environment configuration**: Sets `CDP_PATH` environment variable so SoundThread knows where to find the tools
3. **Library path handling**: The `wrapProgram` mechanism adds `LD_LIBRARY_PATH` so the binary can find system libraries in NixOS

This approach avoids the configuration dialog while keeping all binaries read-only in the Nix store.

## References

- [musnix GitHub](https://github.com/musnix/musnix) - Real-time audio for NixOS
- [NixOS Wiki - Audio Production](https://wiki.nixos.org/wiki/Audio_production)
- [NixOS Wiki - Packaging Binaries](https://wiki.nixos.org/wiki/Packaging/Binaries)
- [NixOS Manual - Nix Expression Language](https://nixos.org/manual/nix/stable/)
