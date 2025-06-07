# Packages Directory

This directory contains custom package definitions and overlays for the Home-lab NixOS infrastructure.

## Directory Purpose

The `packages/` directory is used for:
- Custom package derivations not available in nixpkgs
- Modified versions of existing packages
- Home-lab specific applications and utilities
- Package overlays and customizations

## Structure

### Custom Packages
- `my-package/` - Individual package directories
- `default.nix` - Package collection and exports
- `flake-module.nix` - Flake integration for packages

### Package Categories
- `applications/` - Custom applications and GUIs
- `scripts/` - Shell scripts and automation tools
- `configs/` - Configuration packages and templates
- `overlays/` - Package overlays and modifications

## Usage

### In Flake Configuration
```nix
# flake.nix
{
  outputs = { self, nixpkgs, ... }: {
    packages.x86_64-linux = import ./packages { 
      pkgs = nixpkgs.legacyPackages.x86_64-linux; 
    };
    
    overlays.default = import ./packages/overlays;
  };
}
```

### In Machine Configuration
```nix
# machine configuration
{
  nixpkgs.overlays = [ inputs.self.overlays.default ];
  
  environment.systemPackages = with pkgs; [
    # Custom packages from this directory
    my-custom-tool
    home-lab-scripts
  ];
}
```

## Package Development

### Creating New Package
1. Create package directory: `packages/my-package/`
2. Write `default.nix` with package derivation
3. Add to `packages/default.nix` exports
4. Test with `nix build .#my-package`

### Package Template
```nix
{ lib, stdenv, fetchFromGitHub, ... }:

stdenv.mkDerivation rec {
  pname = "my-package";
  version = "1.0.0";
  
  src = fetchFromGitHub {
    owner = "user";
    repo = "repo";
    rev = "v${version}";
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };
  
  meta = with lib; {
    description = "Description of my package";
    homepage = "https://github.com/user/repo";
    license = licenses.mit;
    maintainers = [ "geir" ];
    platforms = platforms.linux;
  };
}
```

## Overlay Examples

### Package Modification
```nix
# overlays/default.nix
final: prev: {
  # Modify existing package
  vim = prev.vim.override {
    features = "huge";
  };
  
  # Add custom package
  home-lab-tools = final.callPackage ../tools { };
}
```

## Home-lab Specific Packages

### Lab Tool (`lab`) - Evolution Roadmap
The `lab` tool is the central infrastructure management utility with planned major enhancements:

**Current Implementation (Shell-based):**
- Multi-machine deployment via SSH/rsync
- Infrastructure status monitoring  
- Color-coded logging and error handling
- Machine health checks and connectivity testing

**Phase 1: deploy-rs Integration**
Research completed - deploy-rs provides production-grade deployment capabilities:
- **Automatic rollback**: Failed deployments revert automatically
- **Parallel deployment**: Deploy to multiple machines simultaneously  
- **Health checks**: Validates deployments before committing
- **Atomic operations**: Either succeeds completely or fails cleanly
- **Flake-native**: Built specifically for NixOS flakes

Implementation approach:
```bash
# Hybrid command structure
lab deploy sleeper-service        # Current SSH/rsync method
lab deploy-rs sleeper-service     # New deploy-rs backend
lab deploy-all --parallel         # Parallel deployment via deploy-rs
```

Configuration integration:
```nix
# flake.nix additions
inputs.deploy-rs.url = "github:serokell/deploy-rs";

deploy.nodes = {
  sleeper-service = {
    hostname = "sleeper-service.tail807ea.ts.net";
    profiles.system = {
      user = "root";
      path = deploy-rs.lib.x86_64-linux.activate.nixos 
        self.nixosConfigurations.sleeper-service;
      sshUser = "sma";
      autoRollback = true;
      magicRollback = true;
      activationTimeout = 180;
    };
  };
};
```

**Phase 2: Enhanced Statistics Engine**
Current `lab status` provides basic connectivity - planned expansion to comprehensive monitoring:

**Rust/Go Implementation for Performance:**
- **System metrics**: CPU, memory, disk usage, network stats
- **Service monitoring**: systemd service status, failed units
- **ZFS statistics**: Pool health, scrub status, capacity usage
- **Network topology**: Tailscale mesh status, latency metrics
- **Historical data**: Trend analysis and performance tracking

**Example enhanced output:**
```bash
$ lab status --detailed
Infrastructure Status (Updated: 2024-01-20 15:30:42)

━━━ congenital-optimist (local) ━━━
✅ Online │ Load: 1.2 │ RAM: 8.4GB/32GB │ Disk: 45% │ Uptime: 7d 2h
🔗 Tailscale: Active (100.81.15.84) │ Latency: local

━━━ sleeper-service (file server) ━━━  
✅ Online │ Load: 0.3 │ RAM: 2.1GB/8GB │ Disk: 67% │ Uptime: 12d 8h
🗄️  ZFS: ONLINE │ Pool: storage (1.8TB, 50% used) │ Last scrub: 3d ago
🔗 Tailscale: Active (100.81.15.85) │ Latency: 2ms
📡 Services: sshd ✅ │ nfs-server ✅ │ zfs-mount ✅

━━━ grey-area (unreachable) ━━━
⚠️  Offline │ Last seen: 2h ago │ SSH: Connection refused
```

**Phase 3: GNU Stow Dotfile Integration**
Research completed - GNU Stow provides excellent dotfile management for server configurations:

**Use cases:**
- **Server user configs**: Simple dotfiles for `sma` user on servers
- **Machine-specific configs**: Different configurations per server role
- **Selective deployment**: Deploy only needed configs per machine

**Integration approach:**
```bash
# Enhanced lab tool commands
lab dotfiles deploy sma@sleeper-service    # Deploy server user configs
lab dotfiles status                         # Show dotfile deployment status
lab dotfiles sync --machine sleeper-service # Sync specific machine configs
```

**Directory structure:**
```
packages/dotfiles/
├── server-common/          # Shared server configurations
│   ├── .zshrc             # Basic shell config
│   ├── .vimrc             # Editor config  
│   └── .gitconfig         # Git configuration
├── sleeper-service/        # NFS server specific
│   └── .config/
│       └── nfs/
├── grey-area/             # Git server specific  
│   └── .gitconfig         # Enhanced git config
└── stow-deploy.nix        # NixOS integration
```

**Hybrid Configuration Strategy:**
- **Keep org-mode** for complex desktop configurations (geir user)
- **Use GNU Stow** for simple server configurations (sma user)
- **Machine-specific packages** for role-based configurations

**Phase 4: Advanced Features**
- **Configuration drift detection**: Compare deployed vs expected state
- **Automated health checks**: Scheduled infrastructure validation
- **Integration APIs**: Metrics export for monitoring systems
- **Web dashboard**: Optional web interface for infrastructure overview
- **Alert system**: Notifications for infrastructure issues

**Implementation Timeline:**
1. **Q1 2024**: deploy-rs integration and testing
2. **Q2 2024**: Enhanced statistics engine in Rust/Go
3. **Q3 2024**: GNU Stow dotfile integration
4. **Q4 2024**: Advanced monitoring and alerting features

### CongenitalOptimist Packages
- Development environment customizations
- Workstation-specific tools
- Desktop application modifications
- `lab` tool and deployment utilities

### sleeper-service Packages
- File server utilities
- ZFS monitoring tools
- NFS service management
- Storage health monitoring
- Backup automation scripts

### Server Infrastructure Packages
- **deploy-rs configurations**: Declarative deployment definitions
- **Dotfile managers**: GNU Stow packages for server user configurations
- **Monitoring utilities**: System health and performance tools
- **Network tools**: Tailscale integration and network diagnostics

## Best Practices

- **Versioning**: Pin package versions for reproducibility
- **Documentation**: Include clear descriptions and usage
- **Testing**: Test packages across target machines
- **Licensing**: Respect upstream licenses and attributions
- **Maintenance**: Keep packages updated and functional

## Integration with Modules

Packages can be integrated with NixOS modules:
```nix
# modules/development/tools.nix
{ config, pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    # Reference custom packages
    home-lab-dev-tools
    custom-editor-config
  ];
}
```

## Flake Outputs

Custom packages are exported as flake outputs:
- `packages.x86_64-linux.package-name`
- `overlays.default`
- `apps.x86_64-linux.script-name`