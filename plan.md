# Home Lab Migration Plan

## Current 
├── machines/
│   ├── congenital-optimist/ (AMD workstation)
│   │   ├── default.nix
│   │   ├── hardware-configuration.nix
│   │   └── About.org
│   ├── sleeper-service/ (Intel Xeon E3-1230 V2 file server)
│       ├── default.nix
│       ├── hardware-configuration.nix
│       └── About.org
│   ├── reverse-proxy/ (edge/gateway server)
│   │   ├── default.nix
│   │   ├── hardware-configuration.nix
│   │   └── About.org
│   └── grey-area/ (application server)
│       ├── default.nix
│       ├── hardware-configuration.nix
│       └── About.orgessment

### CongenitalOptimist Machine
- **Current NixOS Version**: 25.05
- **Hardware**: AMD CPU/GPU, ZFS storage (zpool + stuffpool), NFS mounts
- **Desktop Environments**: GNOME, Cosmic, Sway
- **Virtualization**: libvirt, Incus, Podman
- **Configuration Style**: Traditional NixOS (non-flakes)
- **Dotfiles Approach**: Prefer Emacs org-mode with literate programming (no Home Manager)

### Current Structure
```
Home-lab/
├── Machines/
│   ├── CongenitalOptimist/ (existing - AMD workstation)
│   │   ├── configuration.nix
│   │   ├── hardware-configuration.nix
│   │   └── About.org
│   └── Modules/ (existing modular structure)
│       ├── common/
│       │   ├── base.nix (modern CLI tools & aliases)
│       │   └── tty.nix (console styling)
│       └── virtualization/
│           ├── podman.nix
│           ├── libvirt.nix
│           └── incus.nix
└── Users/
    └── geir/
        └── user.nix (has typo: progtams → programs)
```

### Target Structure (Post-Migration)
```
Home-lab/
├── flake.nix
├── flake.lock
├── machines/
│   ├── congenital-optimist/ (AMD workstation)
│   │   ├── default.nix
│   │   ├── hardware-configuration.nix
│   │   └── About.org
│   ├── sleeper-service/ (Intel Xeon E3-1230 V2 file server)
│       ├── default.nix
│       ├── hardware-configuration.nix
│       └── About.org
│   ├── reverse-proxy/ (edge/gateway server)
│   │   ├── default.nix
│   │   ├── hardware-configuration.nix
│   │   └── About.org
│   └── grey-area/ (application server)
│       ├── default.nix
│       ├── hardware-configuration.nix
│       └── About.org
├── modules/
│   ├── common/
│   ├── desktop/
│   ├── development/
│   ├── virtualization/
│   ├── services/
│   │   ├── nfs.nix
│   │   ├── samba.nix
│   │   ├── backup.nix
│   │   └── monitoring.nix
│   └── users/
│       └── common.nix (shared user configurations)
├── users/
│   └── geir/
│       ├── dotfiles/
│       │   ├── README.org (geir's literate config)
│       │   ├── emacs/
│       │   ├── shell/
│       │   └── editors/
│       └── user.nix (geir's system config)
├── overlays/
├── packages/
└── secrets/ (for future secrets management)

```

## Phase 1: Flakes Migration (Priority: High)

### 1.1 Create Flake Foundation
- [x] Create `flake.nix` at repository root
- [x] Define nixpkgs input pinned to NixOS 25.05
- [x] Add nixpkgs-unstable for bleeding edge packages
- [x] Structure outputs for multiple machines (no Home Manager)
- [x] Fix inconsistent naming convention (machine directories to lowercase)
- [x] Update flake outputs to use correct lowercase paths

### 1.2 Restructure Configuration
- [x] Convert `configuration.nix` to flake-compatible format
- [x] **Keep `system.stateVersion` as "23.11"** (maintains data compatibility)
- [x] Update existing module imports for flake structure
- [x] Integrate existing user configuration properly
- [x] Fix nerd-fonts syntax for 25.05 compatibility
- [x] Fix hostname typo (congenial-optimist → congenital-optimist)

### 1.3 Consolidate User Configuration
- [x] Fix typo in `users/geir/user.nix` (progtams → programs) - Already correct
- [x] Merge duplicate user packages between main config and user module
- [x] Decide on package location strategy (system vs user level)
- [x] Ensure all existing functionality is preserved

### 1.4 Configuration Testing & Validation
- [x] Validate flake syntax with `nix flake check`
- [x] Test build without switching: `nixos-rebuild build --flake`
- [x] Test configuration: `nixos-rebuild test --flake`
- [x] **Successfully tested modularized configuration with virtualization**

### 1.5 Desktop Environment Modularization ✅ NEW
- [x] Split monolithic `environments.nix` into modular components:
  - [x] `common.nix` - Shared desktop configuration (XDG portal, dbus)
  - [x] `gnome.nix` - GNOME desktop environment with extensions
  - [x] `cosmic.nix` - System76 Cosmic desktop environment 
  - [x] `sway.nix` - Sway window manager with Wayland tools
- [x] Update main configuration to use modular desktop imports
- [x] Test modular desktop configuration successfully

### 1.6 Virtualization Stack ✅ NEW  
- [x] Add comprehensive virtualization support:
  - [x] **Incus** - Modern container and VM management (replaces LXD)
  - [x] **Libvirt/QEMU** - Full KVM virtualization with virt-manager
  - [x] **Podman** - Rootless containers with Docker compatibility
- [x] Configure proper user groups (incus-admin, libvirt, podman)
- [x] Enable UEFI/OVMF support for modern VM guests
- [x] Test all virtualization services running successfully
- [ ] Create rollback plan and ZFS snapshots
- [ ] Switch to flake configuration permanently

### 1.7 GitOps Foundation & CI/CD Setup ✅ NEW
- [x] Initialize git repository for infrastructure as code
- [x] Create comprehensive `.gitignore` for NixOS/Nix projects
- [x] Set up initial commit with current modular configuration
- [x] Plan CI/CD pipeline for configuration validation
- [x] Design branch strategy for infrastructure changes
- [x] Create templates for pull request workflows
- [x] Plan automated testing for configuration changes
- [x] Set up secrets management strategy for CI/CD
- [x] Document GitOps workflow for multi-machine deployments

### 1.8 Additional Migration Tasks
- [x] Update all documentation files to use consistent naming
- [x] Update flake descriptions and comments for clarity
- [x] Verify all module imports work correctly in new structure
- [x] Modularize congenital-optimist configuration into logical modules
- [ ] Clean up any remaining references to old PascalCase paths
- [ ] Test that existing aliases and CLI tools still work
- [ ] Verify desktop environments (GNOME, Cosmic, Sway) all function
- [ ] Test virtualization stack (podman, libvirt, incus) functionality
- [ ] Validate ZFS and storage configuration compatibility
- [x] Generate and commit flake.lock file
- [ ] Create backup of current working configuration before final switch

## Phase 2: Configuration Cleanup & Organization

### 2.1 Optimize Current Modular Structure
- [ ] Review and optimize existing `common/base.nix` tools
- [ ] Enhance `common/tty.nix` console configuration
- [ ] Validate virtualization modules are complete
- [ ] Create desktop environment modules (separate GNOME, Cosmic, Sway)
- [ ] Separate development tools into dedicated module

### 2.2 Target Directory Structure
```
Home-lab/
├── flake.nix
├── flake.lock
├── machines/
│   ├── congenital-optimist/ (AMD workstation)
│   │   ├── default.nix (main machine config)
│   │   ├── hardware-configuration.nix
│   │   └── About.org
│   ├── sleeper-service/ (Intel Xeon file server)
│   │   ├── default.nix (file server config)
│   │   ├── hardware-configuration.nix
│   │   └── About.org
│   ├── reverse-proxy/ (edge/gateway server)
│   │   ├── default.nix
│   │   ├── hardware-configuration.nix
│   │   └── About.org
│   └── grey-area/ (application server)
│       ├── default.nix
│       ├── hardware-configuration.nix
│       └── About.org
├── modules/
│   ├── common/
│   │   ├── base.nix (existing modern CLI tools)
│   │   ├── tty.nix (existing console config)
│   │   └── nix.nix (flakes + experimental features)
│   ├── desktop/
│   │   ├── gnome.nix
│   │   ├── cosmic.nix
│   │   └── sway.nix
│   ├── development/
│   │   ├── editors.nix (emacs, neovim, vscode, etc.)
│   │   ├── languages.nix (rust, python, LSPs)
│   │   └── tools.nix
│   ├── virtualization/ (existing)
│   │   ├── podman.nix
│   │   ├── libvirt.nix
│   │   └── incus.nix
│   ├── services/ (for SleeperService + grey-area)
│   │   ├── nfs.nix (network file sharing)
│   │   ├── samba.nix (windows compatibility)
│   │   ├── backup.nix (automated backups)
│   │   ├── monitoring.nix (system monitoring)
│   │   ├── storage.nix (ZFS/RAID management)
│   │   ├── reverse-proxy.nix (nginx/traefik configuration)
│   │   ├── forgejo.nix (git hosting and CI/CD)
│   │   ├── media.nix (jellyfin configuration)
│   │   └── applications.nix (containerized services)
│   └── users/
│       └── common.nix (shared user configurations)
├── users/
│   └── geir/
│       ├── dotfiles/
│       │   ├── README.org (main literate config)
│       │   ├── emacs/
│       │   ├── shell/
│       │   └── editors/
│       └── user.nix (consolidated user config)
├── overlays/
├── packages/
└── secrets/ (for future secrets management)
```

### 2.3 Network Infrastructure Updates
- [x] **Network topology discovery**: Used nmap to map actual network layout
  - **Network Range**: `10.0.0.0/24` (not 192.168.1.x as initially assumed)
  - **Gateway**: `10.0.0.138` (lan.home - router/firewall)
  - **DNS Server**: `10.0.0.14` (pi.hole - Pi-hole ad-blocker)
  - **Current File Server**: `10.0.0.8` (files.home - will be renamed to sleeper-service)
  - **Machine Migration**: sleeper-service is the existing files.home machine, not a new deployment
- [x] **sleeper-service systemd-networkd migration**: Configured for existing file server (files.home → sleeper-service rename)
  - **Current**: files.home at 10.0.0.8 (existing NFS server, will be renamed to sleeper-service)
  - **Configuration**: Static IP 10.0.0.8/24 with gateway 10.0.0.138 (keeping existing IP)
  - **Network Stack**: `networking.useNetworkd = true` with `networking.useDHCP = false`
  - **Interface**: Configured `enp0s25` with static IPv4 addressing  
  - **DNS**: Pi-hole primary (10.0.0.14), router fallback (10.0.0.138), Google DNS (8.8.8.8)
  - **Firewall**: File server ports configured (NFS: 111,2049; SMB: 139,445; NetBIOS: 137,138)
  - **Benefits**: More reliable networking for file server, better integration with NixOS declarative config
- [ ] **Network standardization**: Plan consistent networkd configuration across all server role machines workstation and laptop can use networkmanager
- [x] **IP address allocation**: Document static IP assignments for each service
  - **Local Network (10.0.0.0/24)**:
    - **10.0.0.2**: arlaptop.home (existing laptop)
    - **10.0.0.3**: congenital-optimist (AMD workstation - current machine)
    - **10.0.0.8**: sleeper-service (Intel Xeon file server - rename from files.home)
    - **10.0.0.11**: grey-area (planned application server)
    - **10.0.0.12**: reverse-proxy (planned edge server)
    - **10.0.0.14**: pi.hole (Pi-hole DNS/ad-blocker) maybe move to nixos
    - **10.0.0.90**: wordpresserver.home (existing WordPress server) to be deleted, incus container
    - **10.0.0.117**: webdev.home (existing web development server) to be deleted, incus container
    - **10.0.0.138**: lan.home (router/gateway/dhcp)
  - **Tailscale Network (100.x.x.x/10)**: 
    - **100.109.28.53**: congenital-optimist (current machine)
    - **100.119.86.92**: apps (active server) (rename to grey area)
    - **100.114.185.71**: arlaptop (laptop) (Arch Linux with plans to migrate to NixOS)
    - **100.81.15.84**: files (file server rename to sleeper-service )
    - **100.103.143.108**: pihole (DNS server)
    - **100.96.189.104**: vps1 (external VPS) (rename to reverse proxy)
    - **100.126.202.40**: wordpresserver (WordPress) to be deleted 
    - remind user to update tailsce or find a way to use the cli to do this
- [ ] **VLAN planning**: Consider network segmentation for different service types
- [ ] **DNS configuration**: Plan local DNS resolution for internal services

## Phase 3: System Upgrade & Validation

### 3.1 Pre-upgrade Preparation
- [ ] Backup current system configuration
- [ ] Document current package versions
- [ ] Create ZFS snapshots of all datasets
- [ ] Test flake build without switching
- [ ] Verify all existing modules work in flake context

### 3.2 Upgrade Execution
- [ ] Switch to flake-based configuration
- [ ] Upgrade to NixOS 25.05
- [ ] Validate all services start correctly
- [ ] Test desktop environments functionality
- [ ] Verify virtualization stack
- [ ] Check user environment and packages

### 3.3 Post-upgrade Validation
- [ ] Verify all applications launch
- [ ] Test development tools (editors, LSPs, compilers)
- [ ] Validate container and VM functionality
- [ ] Check ZFS and NFS mount operations
- [ ] Verify shell environment and modern CLI tools work
- [ ] Test console theming and TTY setup

## Phase 4: Literate Dotfiles Setup

### 4.1 Per-User Org-mode Infrastructure
- [ ] Create per-user dotfiles directories (`users/geir/dotfiles/`)
- [ ] Create comprehensive `users/geir/dotfiles/README.org` with auto-tangling
- [ ] Set up Emacs configuration for literate programming workflow
- [ ] Configure automatic tangling on save
- [ ] Create modular sections for different tool configurations
- [ ] Plan for additional users (admin, service accounts, etc.)

### 4.2 Configuration Domains
- [ ] Shell configuration (zsh, starship, aliases)
- [ ] Editor configurations (emacs, neovim, vscode)
- [ ] Development tools and environments
- [ ] System-specific tweaks and preferences
- [ ] Git configuration and development workflow

### 4.3 Integration with NixOS
- [ ] Link org-mode generated configs with NixOS modules where appropriate
- [ ] Document the relationship between system-level and user-level configs
- [ ] Create per-user configuration templates for common patterns
- [ ] Plan user-specific configurations vs shared configurations
- [ ] Consider user isolation and security implications

## Phase 5: Home Lab Expansion Planning

### 5.1 Infrastructure Additions

#### Naming Convention
- **Machine Names**: Culture ship names in PascalCase (e.g., `CongenitalOptimist`, `SleeperService`)
- **Folder Names**: lowercase-with-hyphens (e.g., `congenital-optimist/`, `sleeper-service/`)
- **Flake Outputs**: lowercase-with-hyphens (e.g., `nixosConfigurations.congenital-optimist`)
- **Hostnames**: lowercase-with-hyphens (e.g., `congenital-optimist`, `sleeper-service`)
- **User Names**: Culture character names in lowercase (e.g., `sma`, `geir`)

- [ ] **SleeperService** file server (Intel Xeon E3-1230 V2, 16GB RAM):
  - NFS server for network storage
  - Automated backup services
  - System monitoring and alerting
  - ZFS or software RAID for data redundancy
- [ ] **reverse-proxy** edge server:
  - Nginx/Traefik/caddy reverse proxy
  - SSL/TLS termination with Let's Encrypt
  - External access gateway and load balancing
  - Security protection (Fail2ban, rate limiting)
  - Minimal attack surface, headless operation
- [ ] **grey-area** application server (Culture GCU - versatile, multi-purpose):
  - **Primary**: Forgejo Git hosting (repositories, CI/CD, project management)
  - **Secondary**: Jellyfin media server
  - **Monitoring**: TBD
  - **Infrastructure**: Container-focused (Podman), PostgreSQL database
  - **Integration**: Central Git hosting for all home lab projects
- [ ] Plan for additional users across machines:
  - [x] **geir** - Primary user (development, desktop, daily use)
  - [x] **sma** - Admin user (Diziet Sma, system administration, security oversight)
  - [ ] Service accounts for automation (forgejo-admin, backup-agent)
  - [ ] Guest accounts for temporary access
  - [x] Culture character naming convention established
- [x] **Network infrastructure planning**: Started with sleeper-service systemd-networkd migration
- [ ] Consider hardware requirements for future expansion

### 5.2 Services Architecture
- [ ] Centralized configuration management
- [ ] Per-user secrets management (agenix/sops-nix)
- [ ] User-specific service configurations
- [ ] Monitoring and logging (Prometheus, Grafana)
- [ ] Backup strategy across machines and users
- [ ] Container orchestration planning

### 5.3 Security & Networking
- [x] **systemd-networkd migration**: Completed for sleeper-service with static IP configuration
- [x] **SSH key management centralization**: Implemented two-key strategy
  - **Admin key** (`geir@geokkjer.eu-admin`): For sma user, server administration access
  - **Development key** (`geir@geokkjer.eu-dev`): For geir user, git services, daily development
  - **NixOS module**: `modules/security/ssh-keys.nix` centralizes key management
  - **SSH client config**: Updated with role-based host patterns and key selection
  - **Security benefits**: Principle of least privilege, limited blast radius if compromised
  - **Usage examples**:
    - `ssh geir@sleeper-service.home` - Uses dev key automatically
    - `ssh admin-sleeper` - Uses admin key for sma user access
    - `git clone git@github.com:user/repo` - Uses dev key for git operations
- [ ] VPN configuration (Tailscale expansion)
- [ ] Firewall rules standardization across machines
- [ ] Certificate management (Let's Encrypt)
- [ ] Network segmentation planning (VLANs for services vs. user devices)
- [ ] DNS infrastructure (local DNS server for service discovery)

## Phase 6: Advanced Features

### 6.1 Development Workflow
- [ ] Devshells for different projects
- [ ] Cachix setup for faster builds
- [ ] CI/CD integration
- [ ] Literate dotfiles with org-mode tangling automation

### 6.2 Automation & Maintenance
- [ ] AI integration - development of a mcp server for the cluster
- [ ] Automated system updates
- [ ] Configuration validation tests
- [ ] Deployment automation
- [ ] Monitoring and alerting
### 6.3 Writeup
- [ ] Take all the knowledge we have amassed and make a blog post or a series of blog posts

### Phase 7: goin pro 
- [ ] A plan to generalise this project so it is usable for other people
- [ ] A plan to make dashboard and web interface for the project 

## Timeline Estimates

- **Phase 1**: 1-2 weeks (flakes migration)
- **Phase 2**: 1 week (cleanup and organization)
- **Phase 3**: 2-3 days (upgrade and validation)
- **Phase 4**: 1 week (literate dotfiles setup)
- **Phase 5**: 2-4 weeks (expansion planning and implementation)
- **Phase 6**: Ongoing (advanced features as needed)

## Risk Mitigation

### Critical Risks
1. **Boot failure after upgrade**: ZFS snapshots for quick rollback
2. **Desktop environment issues**: Keep multiple DEs as fallback
3. **Virtualization breakage**: Document current VM configurations
4. **Data loss**: Multiple backup layers (ZFS, external)
5. **User environment regression**: Backup existing dotfiles

### Rollback Strategy
- ZFS snapshot rollback capability
- Keep old configuration.nix as reference
- Maintain emergency boot media
- Document manual recovery procedures
- Preserve current user configuration during migration

## Success Criteria

- [ ] System boots reliably with flake configuration
- [ ] All current functionality preserved
- [ ] NixOS 25.05 running stable
- [ ] Configuration is modular and maintainable
- [ ] User environment fully functional with all packages
- [ ] Modern CLI tools and aliases working
- [ ] Console theming preserved
- [ ] Virtualization stack operational
- [ ] Literate dotfiles workflow established
- [ ] Ready for multi-machine expansion
- [ ] Development workflow improved
- [ ] Documentation complete for future reference

## Infrastructure Notes

### CongenitalOptimist (AMD Workstation)
- Already has excellent modular structure
- Modern CLI tools (eza, bat, ripgrep, etc.) already configured in base.nix
- Console theming with Joker palette already implemented
- User configuration needs cleanup (fix typo, consolidate packages)
- ZFS configuration is solid and shouldn't need changes
- Keep Tailscale configuration as network foundation
- The AMD GPU setup should carry over cleanly to 25.05
- Consider renaming hostname from "work" to "congenital-optimist" for consistency

### SleeperService (Intel Xeon File Server)
- Intel Xeon E3-1230 V2 @ 3.70GHz (4 cores, 8 threads)
- 16GB RAM - adequate for file server operations
- Perfect for reliable, background file serving tasks
- Culture name fits: "massive GSV with reputation for taking unusual tasks"
- Will handle NFS mounts currently served by external "files" server
- Plan for ZFS or software RAID for data redundancy
- Headless operation - no desktop environments needed
- SSH-only access with robust monitoring

### reverse-proxy (Edge Server)
- Lightweight hardware requirements (can be modest specs)
- Primary role: SSL/TLS termination and traffic routing
- External-facing server with minimal attack surface
- Nginx or Traefik for reverse proxy functionality
- Let's Encrypt integration for automated certificate management
- Fail2ban and security hardening
- Routes traffic to internal services (grey-area, sleeper-service)

### grey-area (Application Server - Culture GCU)
- **Hardware**: Intel Xeon E5-2670 v3 (24 cores) @ 3.10 GHz, 31.24 GiB RAM
- **Primary Mission**: Forgejo Git hosting and project management
- **Performance**: Excellent specs for heavy containerized workloads and CI/CD
- Container-focused architecture using Podman
- PostgreSQL database for Forgejo
- Concurrent multi-service deployment capability
- Secondary services: Jellyfin (with transcoding), Nextcloud, Grafana
- Integration hub for all home lab development projects
- Culture name fits: "versatile ship handling varied, ambiguous tasks"
- Central point for CI/CD pipelines and automation

### Home Lab Philosophy
- Emacs org-mode literate programming approach provides better control than Home Manager
- Culture ship names create memorable, characterful infrastructure
- Modular NixOS configuration allows easy machine additions
- Per-user dotfiles structure scales across multiple machines
- Tailscale provides secure network foundation for multi-machine setup
