# NixOS Modules Directory Structure

This directory contains reusable NixOS modules organized by functional domain for the Home-lab infrastructure.

## Directory Organization

### `common/`
Core modules shared across all machines in the home lab:
- `base.nix` - Modern CLI tools, aliases, and essential packages
- `tty.nix` - Console configuration and theming
- `nix.nix` - Nix/flakes configuration and optimization settings
- `ssh.nix` - SSH server and security configurations
- `networking.nix` - Basic networking and firewall settings

### `desktop/`
Desktop environment configurations for workstation machines:
- `gnome.nix` - GNOME desktop environment setup
- `cosmic.nix` - System76 COSMIC desktop configuration  
- `sway.nix` - Sway window manager and Wayland setup
- `fonts.nix` - Font packages and configurations
- `audio.nix` - PipeWire/audio system setup

### `development/`
Development tools and environments:
- `editors.nix` - Text editors (Emacs, Neovim, VSCode)
- `languages.nix` - Programming languages and runtimes
- `tools.nix` - Development utilities and CLI tools
- `containers.nix` - Development container tools
- `git.nix` - Git configuration and tools

### `virtualization/`
Virtualization and containerization:
- `podman.nix` - Podman container runtime
- `libvirt.nix` - KVM/QEMU virtualization
- `incus.nix` - System container management
- `docker.nix` - Docker runtime (if needed)

### `services/`
Network services primarily for SleeperService file server:
- `nfs.nix` - Network File System server
- `samba.nix` - SMB/CIFS file sharing
- `backup.nix` - Automated backup services
- `monitoring.nix` - System monitoring and alerting
- `storage.nix` - ZFS and storage management
- `media.nix` - Media server services (Jellyfin/Plex)

### `users/`
User management and shared user configurations:
- `common.nix` - Shared user settings across machines
- `groups.nix` - System groups and permissions
- `security.nix` - User security policies

## Usage

Modules are imported in machine configurations like:

```nix
imports = [
  ../../modules/common/base.nix
  ../../modules/desktop/gnome.nix
  ../../modules/virtualization/podman.nix
];
```

## Design Philosophy

- **Modular**: Each module has a single, clear responsibility
- **Reusable**: Modules work across different machine types
- **Composable**: Mix and match modules for different machine roles
- **Documented**: Each module includes usage examples and options
- **Testable**: Modules can be tested independently

## Machine Profiles

### CongenitalOptimist (Workstation)
- All desktop modules
- Development tools
- Virtualization stack
- User-focused configurations

### sleeper-service (File Server)
- Common base only
- Service modules (NFS, Samba, backup)
- No desktop environment
- Server-focused configurations