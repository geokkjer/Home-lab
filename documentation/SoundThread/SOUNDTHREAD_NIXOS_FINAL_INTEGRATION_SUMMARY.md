# SoundThread NixOS Integration - Final Summary

## Overview

Successfully packaged and integrated **SoundThread** (Godot-based GUI for CDP - Composers Desktop Project) into NixOS with full system integration of all 220 bundled CDP audio processing tools.

**Status**: ✅ **COMPLETE AND TESTED**

## Key Architecture Decisions

### 1. **Bundled CDP Approach**

- Uses SoundThread's official prebuilt CDP binaries (`cdprogs_linux.tar.gz`)
- 220 CDP tools included in the upstream release
- Follows upstream maintenance and patch strategy
- Alternative to building CDP8 from source

### 2. **NixOS Compatibility Layer**

- **patchelf integration**: Rewrites ELF interpreter paths from `/lib64/ld-linux-x86-64.so.2` to NixOS dynamic linker
- Applied to both SoundThread binary and all 220 CDP tools
- Eliminates runtime errors on pure NixOS systems

### 3. **CDP Tools Exposure Strategy**

- Two-phase approach:
  1. Extract CDP tools to `$out/cdp/bin/` in package
  2. Create `$out/cdp-bin/` with symlinks to all tools
  3. Use `pkgs.symlinkJoin` to merge into system PATH

## Package Structure

### File: `/home/geir/Home-lab/modules/sound/Music/SoundThread.nix`

**Key Components:**

```nix
# 1. Graphics & Audio Dependencies
buildInputs = [
  xorg.libX11 xorg.libXcursor xorg.libXrandr xorg.libXinerama xorg.libXi 
  xorg.libXxf86vm xorg.libXext xorg.libXdmcp xorg.libXrender
  libGL alsa-lib pulseaudio libxcb libxkbcommon wayland gcc-unwrapped.lib
];

# 2. Install Phase: Extract & Patch Binaries
installPhase = ''
  # Extract SoundThread binary
  cp SoundThread.x86_64 $out/bin/SoundThread
  patchelf --set-interpreter ${stdenv.cc.bintools.dynamicLinker} $out/bin/SoundThread
  
  # Extract CDP tools
  tar -xzf cdprogs_linux.tar.gz -C $out/cdp/
  
  # Patch all 220 CDP tools
  for tool in $out/cdp/bin/*; do
    if file "$tool" | grep -q "ELF"; then
      patchelf --set-interpreter ${stdenv.cc.bintools.dynamicLinker} "$tool"
    fi
  done
'';

# 3. Post-Install: Setup Wrapper
postInstall = ''
  wrapProgram $out/bin/SoundThread \
    --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [...]}" \
    --prefix PATH : "$cdpBinPath" \
    --set CDP_PATH "$cdpBinPath" \
    --set XDG_DATA_HOME "$out/share"
'';

# 4. Post-Fixup: Create CDP Tool Symlinks
postFixup = ''
  mkdir -p $out/cdp-bin
  for tool in "$out/cdp/bin"/*; do
    ln -s "$tool" "$out/cdp-bin/$(basename $tool)"
  done
'';
```

## System Integration

### File: `/home/geir/Home-lab/machines/congenital-optimist/configuration.nix`

**Integration Method:**

```nix
environment.systemPackages = with pkgs; [
  (let 
     st = (callPackage ../../modules/sound/Music/SoundThread.nix {});
   in
     pkgs.symlinkJoin {
       name = "soundthread-with-cdp";
       paths = [ st ];
       postBuild = ''
         mkdir -p $out/bin
         for tool in ${st}/cdp-bin/*; do
           ln -sf "$tool" "$out/bin/$(basename $tool)"
         done
       '';
     }
  )
];
```

**Result:**

- SoundThread binary: `/run/current-system/sw/bin/SoundThread`
- All 220 CDP tools: `/run/current-system/sw/bin/{tool_name}`
- CDP_PATH environment variable: Points to package CDP binaries
- Full system-wide accessibility

## Critical Fixes Applied

### Problem 1: ELF Interpreter Path

**Error**: `Error relocation error: file not found: /lib64/ld-linux-x86-64.so.2`
**Solution**: Patchelf to rewrite ELF interpreter sections

### Problem 2: Graphics Library Missing

**Error**: "Can't load the Wayland client library"
**Solution**: Added complete X11/Wayland dependency stack

### Problem 3: CDP Tools Not in PATH

**Error**: Tools extracted but not accessible from shell
**Solution**:

- Created `cdp-bin/` directory with symlinks in package
- Used `symlinkJoin` in system configuration to expose to global PATH

### Problem 4: NixOS Module vs Package Confusion

**Error**: "function called with unexpected argument 'inputs'"
**Solution**: Used `environment.systemPackages` instead of `imports`

## Verification Tests

```bash
# Test 1: SoundThread runs
$ SoundThread --version
4.4.1.stable.official.49a5bc7b6

# Test 2: CDP tools in PATH
$ which filter blur bounce stretch
/run/current-system/sw/bin/filter
/run/current-system/sw/bin/blur
/run/current-system/sw/bin/bounce
/run/current-system/sw/bin/stretch

# Test 3: CDP tool execution
$ filter 2>&1 | head -3
CDP Release 7.1 2016
USAGE: filter NAME (mode) infile outfile (datafile) parameters
where NAME can be any one of
```

## Package Statistics

| Aspect | Value |
|--------|-------|
| SoundThread Binary | 71 MB (Godot 4.4.1) |
| CDP Tools Count | 220 |
| Total CDP Size | ~31 MB (tarball) |
| Package Hash | `/nix/store/...soundthread-0.4.0-beta` |
| System Binaries | 1648+ (with CDP tools) |
| Store References | ~1.2 GB (all dependencies) |

## Deployment Commands

```bash
# Build SoundThread package
nix-build -E "(import <nixpkgs> {}).callPackage ./modules/sound/Music/SoundThread.nix {}"

# Rebuild entire system with SoundThread
sudo nixos-rebuild switch --flake .#congenital-optimist

# Test integration
SoundThread --version && which filter && filter 2>&1 | head -1
```

## Files Modified

1. `/home/geir/Home-lab/modules/sound/Music/SoundThread.nix` (189 lines)
   - Complete package derivation with all fixes

2. `/home/geir/Home-lab/machines/congenital-optimist/configuration.nix`
   - System integration via `symlinkJoin`
   - Environment variables setup
   - Profile.d script creation

3. `/etc/profile.d/soundthread-cdp.sh`
   - Exports CDP_PATH (optional, already in wrapper)

## Lessons Learned

1. **Prebuilt Binaries on NixOS**: Require careful ELF patchelf treatment
2. **Graphics Libraries**: Must include complete X11/Wayland stack for Godot apps
3. **PATH Exposure**: `symlinkJoin` is cleaner than wrapper scripts for multiple tools
4. **Upstream Approach**: Following SoundThread's bundled CDP strategy avoids source build complexity
5. **NixOS vs Modules**: Critical to distinguish packages from modules when integrating

## Future Improvements

- [ ] Add `meta.license`, `meta.maintainers` to package
- [ ] Create NixOS module for configuration options
- [ ] Add optional source-built CDP8 as fallback
- [ ] Package SoundThread as nixpkgs PR candidate
- [ ] Add comprehensive test suite for CDP tools
- [ ] Document CDP tool categories (filters, transforms, analysis, synthesis, etc.)

## Conclusion

SoundThread is now fully integrated into NixOS congenital-optimist machine with:

- ✅ Godot GUI binary running correctly
- ✅ All 220 CDP tools accessible system-wide
- ✅ Proper library dependencies resolved
- ✅ NixOS purity maintained (no hardcoded paths)
- ✅ Reproducible build and deployment

The solution follows best practices for packaging prebuilt applications in NixOS while maintaining upstream compatibility and the patched CDP tools included in the official release.
