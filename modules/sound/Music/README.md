# Music Making Module

In this module, we focus on tools and software that facilitate music creation, sound design, and audio processing. Below are some of the key components included in this module.

## CDP - ComposersDesktop

ComposersDesktop (CDP) is a powerful suite of sound processing tools designed for composers and sound designers. This module integrates CDP into your workflow, allowing you to create, manipulate, and transform sounds with ease.


- [GitHub Repository](https://github.com/ComposersDesktop/CDP8)

### Documentation

Comprehensive documentation for the CDP8 NixOS packaging implementation:

- **[CDP8 Step-by-Step Guide](../../documentation/CDP8_STEP_BY_STEP_GUIDE.md)** - Complete execution guide showing all 7 steps taken, from project analysis to final deployment
- **[CDP8 Technical Packaging Guide](../../documentation/CDP8_NIXOS_PACKAGING.md)** - In-depth technical documentation covering build system analysis, dependency mapping, and troubleshooting
- **[CDP8 Implementation Summary](../../documentation/CDP8_IMPLEMENTATION_COMPLETE.md)** - Executive summary with list of all 109 compiled programs and technical solutions

#### What's Included

- ✅ 109 audio processing programs successfully compiled
- ✅ All build issues resolved and documented
- ✅ Nix derivation ready for production use
- ✅ Complete dependency analysis

## SoundThread

Node-based GUI for The Composers Desktop Project (CDP), built with Godot. SoundThread provides an intuitive interface for creating complex audio processing workflows by connecting CDP tools together visually.

- [GitHub Repository](https://github.com/j-p-higgins/SoundThread)

### Documentation

Comprehensive documentation for the SoundThread NixOS packaging implementation with automatic CDP integration:

- **[SoundThread Step-by-Step Guide](../../documentation/SOUNDTHREAD_STEP_BY_STEP_GUIDE.md)** - Complete implementation guide covering all 10 steps, from project analysis to CDP integration solution
- **[SoundThread Technical Packaging Guide](../../documentation/SOUNDTHREAD_NIXOS_PACKAGING.md)** - In-depth technical documentation covering architecture, dependency analysis, and the wrapper-based CDP path integration solution
- **[SoundThread Implementation Summary](../../documentation/SOUNDTHREAD_IMPLEMENTATION_COMPLETE.md)** - Executive summary with design decisions, challenge resolutions, and verification strategies

#### What's Included

- ✅ Prebuilt Godot GUI application
- ✅ Automatic CDP binary path configuration
- ✅ NixOS wrapper-based environment setup
- ✅ All runtime dependencies (graphics, audio, Godot)
- ✅ CDP8 integration (requires CDP8 package)

#### Key Features

- Node-based patching system for complex workflows
- Over 100 popular CDP processes available
- Audio automation with breakpoint files
- Stereo/mono audio support
- Project save/load functionality
- Built-in tutorials

## SuperCollider

SuperCollider is a platform for audio synthesis and algorithmic composition, used by musicians, artists, and researchers working with sound. This module includes SuperCollider along with a collection of useful extensions and libraries.
[GitHub Repository](https://github.com/supercollider/supercollider)

### Installation NixOS

To install SuperCollider on NixOS, you can use the following configuration:

```nix
{ pkgs, ... }:
{
    environment.systemPackages = with pkgs; [
        supercollider-with-plugins
    ];
}
```
