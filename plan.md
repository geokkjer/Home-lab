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

## Deployment Status & Accomplishments ✅

### sleeper-service Deployment (COMPLETED)
**Date**: Recently completed  
**Status**: ✅ Fully operational  
**Machine**: Intel Xeon E3-1230 V2, 16GB RAM (formerly files.home)

#### Key Achievements:
- **Flake Migration**: Successfully deployed NixOS flake configuration on remote machine
- **ZFS Stability**: Resolved ZFS mounting conflicts causing boot failures
- **Data Preservation**: All 903GB of media data intact and accessible
- **Network Integration**: Added Pi-hole DNS (10.0.0.14) for package resolution
- **SSH Infrastructure**: Implemented centralized SSH key management
- **Boot Performance**: Clean boot in ~1 minute with ZFS auto-mounting enabled
- **Remote Deployment**: Established rsync + SSH deployment workflow
- **NFS Server**: Configured NFS exports for both local (10.0.0.0/24) and Tailscale (100.64.0.0/10) networks
- **Network Configuration**: Updated to use Tailscale IPs for reliable mesh connectivity

#### Technical Solutions:
- **ZFS Native Mounting**: Migrated from legacy mountpoints to ZFS native paths
- **Hardware Configuration**: Removed conflicting ZFS filesystem entries
- **Graphics Compatibility**: Added `nomodeset` kernel parameter, disabled NVIDIA drivers
- **DNS Configuration**: Multi-tier DNS with Pi-hole primary, router and Google fallback
- **Deployment Method**: Remote deployment via rsync + SSH instead of direct nixos-rebuild
- **NFS Exports**: Resolved dataset conflicts by commenting out conflicting tmpfiles rules
- **Network Access**: Added Tailscale interface (tailscale0) as trusted interface in firewall

#### Data Verified:
- **Storage Pool**: 903GB used, 896GB available
- **Media Content**: Films (184GB), Series (612GB), Audiobooks (94GB), Music (9.1GB), Books (3.5GB)
- **Mount Points**: `/mnt/storage` and `/mnt/storage/media` with proper ZFS auto-mounting
- **NFS Access**: Both datasets exported with proper permissions for network access

### grey-area Deployment (COMPLETED) ✅ NEW
**Date**: June 2025  
**Status**: ✅ Fully operational  
**Machine**: Intel Xeon E5-2670 v3 (24 cores) @ 3.10 GHz, 31.24 GiB RAM

#### Key Achievements:
- **Flake Configuration**: Successfully deployed NixOS flake-based configuration
- **NFS Client**: Configured reliable NFS mount to sleeper-service media storage via Tailscale
- **Service Stack**: Deployed comprehensive application server with multiple services
- **Network Integration**: Integrated with centralized extraHosts module using Tailscale IPs
- **User Management**: Resolved UID conflicts and implemented consistent user configuration
- **Firewall Configuration**: Properly configured ports for all services

#### Services Deployed:
- **Jellyfin**: ✅ Media server with access to NFS-mounted content from sleeper-service
- **Calibre-web**: ✅ E-book management and reading interface
- **Forgejo**: ✅ Git hosting server (git.geokkjer.eu) with reverse proxy integration
- **Audiobook Server**: ✅ Audiobook streaming and management

#### Technical Implementation:
- **NFS Mount**: `/mnt/remote/media` successfully mounting `sleeper-service:/mnt/storage/media`
- **Network Path**: Using Tailscale mesh (100.x.x.x) for reliable connectivity
- **Mount Options**: Configured with automount, soft mount, and appropriate timeouts
- **Firewall Ports**: 22 (SSH), 3000 (Forgejo), 23231 (other services)
- **User Configuration**: Fixed UID consistency with centralized sma user module

#### Data Access Verified:
- **Movies**: 38 films accessible via NFS
- **TV Series**: 29 series collections
- **Music**: 9 music directories  
- **Audiobooks**: 79 audiobook collections
- **Books**: E-book collection
- **Media Services**: All content accessible through Jellyfin and other services

### reverse-proxy Integration (COMPLETED) ✅ NEW
**Date**: June 2025  
**Status**: ✅ Fully operational  
**Machine**: External VPS (46.226.104.98)

#### Key Achievements:
- **Nginx Configuration**: Successfully configured reverse proxy for Forgejo
- **Hostname Resolution**: Fixed hostname mapping from incorrect "apps" to correct "grey-area"
- **SSL/TLS**: Configured ACME Let's Encrypt certificate for git.geokkjer.eu
- **SSH Forwarding**: Configured SSH proxy on port 1337 for Git operations
- **Network Security**: Implemented DMZ-style security with Tailscale-only SSH access

#### Technical Configuration:
- **HTTP Proxy**: `git.geokkjer.eu` → `http://grey-area:3000` (Forgejo)
- **SSH Proxy**: Port 1337 → `grey-area:22` for Git SSH operations
- **Network Path**: External traffic → reverse-proxy → Tailscale → grey-area
- **Security**: SSH restricted to Tailscale network, fail2ban protection
- **DNS**: Proper hostname resolution via extraHosts module

### Centralized Network Configuration (COMPLETED) ✅ NEW
**Date**: June 2025  
**Status**: ✅ Fully operational  

#### Key Achievements:
- **extraHosts Module**: Created centralized hostname resolution using Tailscale IPs
- **Network Consistency**: All machines use same IP mappings for reliable mesh connectivity
- **SSH Configuration**: Updated IP addresses in ssh-keys.nix module  
- **User Management**: Resolved user configuration conflicts between modules

#### Network Topology:
- **Tailscale Mesh IPs**:
  - `100.109.28.53` - congenital-optimist (workstation)
  - `100.81.15.84` - sleeper-service (NFS file server)
  - `100.119.86.92` - grey-area (application server)  
  - `100.96.189.104` - reverse-proxy (external VPS)
  - `100.103.143.108` - pihole (DNS server)
  - `100.126.202.40` - wordpresserver (legacy)

#### Module Integration:
- **extraHosts**: Added to all machine configurations for consistent hostname resolution
- **SSH Keys**: Updated IP addresses (grey-area: 10.0.0.12, reverse-proxy: 46.226.104.98)
- **User Modules**: Fixed conflicts between sma user definitions in different modules

### Home Lab Deployment Tool (COMPLETED) ✅ NEW
**Date**: Recently completed  
**Status**: ✅ Fully operational  
**Tool**: `lab` command - Custom deployment management system

#### Key Achievements:
- **Custom Package Creation**: Developed `home-lab-tools.nix` package with comprehensive deployment functionality
- **System Integration**: Added lab command to system packages via `modules/system/applications.nix`
- **Conflict Resolution**: Resolved shell alias conflict by renaming "lab" alias to "home-lab"
- **Multi-Machine Support**: Deployment capabilities for sleeper-service, grey-area, and reverse-proxy
- **Status Monitoring**: Infrastructure connectivity checking with color-coded output
- **Deployment Modes**: Support for boot, test, and switch deployment modes

#### Technical Implementation:
- **Package Structure**: Custom Nix package using `writeShellScriptBin` with proper dependencies
- **Color-Coded Logging**: Blue info, green success, yellow warnings, red errors for clear output
- **SSH Infrastructure**: Leverages existing SSH key management for secure remote deployment
- **Rsync Deployment**: Efficient configuration syncing to target machines
- **Error Handling**: Comprehensive error checking and validation throughout deployment process
- **Service Detection**: Proper Tailscale service monitoring with `tailscaled` detection

#### Available Commands:
- **`lab status`**: Check connectivity to all infrastructure machines
- **`lab deploy <machine> [mode]`**: Deploy configuration to specific machine
  - **Machines**: sleeper-service, grey-area, reverse-proxy
  - **Modes**: boot (default), test (temporary), switch (permanent)
- **Help System**: Built-in usage documentation and examples

#### Deployment Workflow:
1. **Configuration Sync**: Uses rsync to transfer entire Home-lab directory to target machine
2. **Remote Execution**: SSH into target machine and execute `nixos-rebuild` with flake
3. **Validation**: Checks deployment success and provides clear feedback
4. **Status Verification**: Can verify deployment results with status command

#### Infrastructure Status Integration:
- **Local Machine**: Checks Tailscale service status on congenital-optimist
- **Remote Machines**: SSH connectivity testing with timeout handling
- **Network Topology**: Integrates with existing Tailscale mesh network
- **Service Monitoring**: Foundation for future comprehensive monitoring system

#### Usage Examples:
```bash
lab status                          # Check all machine connectivity
lab deploy sleeper-service boot     # Deploy and set for next boot
lab deploy grey-area switch         # Deploy and activate immediately
lab deploy reverse-proxy test       # Deploy temporarily for testing
```

#### Technical Benefits:
1. **Centralized Deployment**: Single command interface for all home lab machines
2. **Consistent Process**: Standardized deployment workflow across infrastructure
3. **Error Prevention**: Validation and safety checks prevent deployment failures
4. **Operational Visibility**: Clear status reporting for infrastructure state
5. **Extensibility**: Modular design allows easy addition of new machines and features
6. **Integration**: Seamless integration with existing SSH and Tailscale infrastructure

---

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
- [x] **sleeper-service systemd-networkd migration**: ✅ **COMPLETED and DEPLOYED**
  - [x] **Hostname transition**: Successfully renamed from files.home to sleeper-service
  - [x] **Static IP preserved**: Maintained 10.0.0.8/24 with gateway 10.0.0.138
  - [x] **DNS integration**: Pi-hole primary (10.0.0.14), router fallback (10.0.0.138), Google DNS (8.8.8.8)
  - [x] **Network stack**: `networking.useNetworkd = true` with `networking.useDHCP = false`
  - [x] **Interface configuration**: `enp0s25` configured with declarative static IPv4
  - [x] **Service ports**: File server ports configured (NFS: 111,2049; SMB: 139,445; NetBIOS: 137,138)
  - [x] **Production validation**: Network configuration tested and operational
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

## Phase 4: Dotfiles & Configuration Management

### 4.1 GNU Stow Infrastructure for Regular Dotfiles ✅ DECIDED
**Approach**: Use GNU Stow for traditional dotfiles, literate programming for Emacs only

#### GNU Stow Setup
- [ ] Create `~/dotfiles/` directory structure with package-based organization
- [ ] Set up core packages: `zsh/`, `git/`, `tmux/`, `starship/`, etc.
- [ ] Configure selective deployment per machine (workstation vs servers)
- [ ] Create stow deployment scripts for different machine profiles
- [ ] Document stow workflow and package management

#### Package Structure
```
~/dotfiles/              # Stow directory (target: $HOME)
├── zsh/                # Shell configuration
│   ├── .zshrc
│   ├── .zshenv
│   └── .config/zsh/
├── git/                # Git configuration
│   ├── .gitconfig
│   └── .config/git/
├── starship/           # Prompt configuration
│   └── .config/starship.toml
├── tmux/               # Terminal multiplexer
│   └── .tmux.conf
├── emacs/              # Basic Emacs bootstrap (points to literate config)
│   └── .emacs.d/early-init.el
└── machine-specific/   # Per-machine configurations
    ├── workstation/
    └── server/
```

### 4.2 Literate Programming for Emacs Configuration ✅ DECIDED
**Approach**: Comprehensive org-mode literate configuration for Emacs only

#### Emacs Literate Setup
- [ ] Create `~/dotfiles/emacs/.emacs.d/configuration.org` as master config
- [ ] Set up automatic tangling on save (org-babel-tangle-on-save)
- [ ] Modular org sections: packages, themes, keybindings, workflows
- [ ] Bootstrap early-init.el to load tangled configuration
- [ ] Create machine-specific customizations within org structure

#### Literate Configuration Structure
```
~/dotfiles/emacs/.emacs.d/
├── early-init.el       # Bootstrap (generated by Stow)
├── configuration.org   # Master literate config
├── init.el            # Tangled from configuration.org
├── modules/           # Tangled module files
│   ├── base.el
│   ├── development.el
│   ├── org-mode.el
│   └── ui.el
└── machine-config/    # Machine-specific overrides
    ├── workstation.el
    └── server.el
```

### 4.3 Integration Strategy
- [ ] **System-level**: NixOS modules provide system packages and environment
- [ ] **User-level**: GNU Stow manages dotfiles and application configurations  
- [ ] **Emacs-specific**: Org-mode literate programming for comprehensive Emacs setup
- [ ] **Per-machine**: Selective stow packages + machine-specific customizations
- [ ] **Version control**: Git repository for dotfiles with separate org documentation

### 4.4 Deployment Workflow
- [ ] Create deployment scripts for different machine types:
  - **Workstation**: Full package deployment (zsh, git, tmux, starship, emacs)
  - **Server**: Minimal package deployment (zsh, git, basic emacs)
  - **Development**: Additional packages (language-specific tools, IDE configs)
- [ ] Integration with existing `lab` deployment tool
- [ ] Documentation for new user onboarding across machines

## Phase 5: Home Lab Expansion Planning

### 5.1 Infrastructure Additions

#### Naming Convention
- **Machine Names**: Culture ship names in PascalCase (e.g., `CongenitalOptimist`, `SleeperService`)
- **Folder Names**: lowercase-with-hyphens (e.g., `congenital-optimist/`, `sleeper-service/`)
- **Flake Outputs**: lowercase-with-hyphens (e.g., `nixosConfigurations.congenital-optimist`)
- **Hostnames**: lowercase-with-hyphens (e.g., `congenital-optimist`, `sleeper-service`)
- **User Names**: Culture character names in lowercase (e.g., `sma`, `geir`)

- [x] **SleeperService** file server (Intel Xeon E3-1230 V2, 16GB RAM): ✅ **COMPLETED**
  - [x] NFS server for network storage (903GB ZFS pool operational)
  - [x] ZFS storage with native mounting configuration
  - [x] Flake-based NixOS deployment successful
  - [x] SSH key management implemented
  - [x] Network configuration with Pi-hole DNS integration
  - [x] System boots cleanly in ~1 minute with ZFS auto-mounting
  - [x] Data preservation verified (Films: 184GB, Series: 612GB, etc.)
  - [x] NFS exports configured for both local and Tailscale networks
  - [x] Resolved dataset conflicts and tmpfiles rule conflicts
  - [ ] Automated backup services (future enhancement)
  - [ ] System monitoring and alerting (future enhancement)
- [x] **reverse-proxy** edge server: ✅ **COMPLETED**
  - [x] Nginx reverse proxy with proper hostname mapping (grey-area vs apps)
  - [x] SSL/TLS termination with Let's Encrypt for git.geokkjer.eu
  - [x] External access gateway with DMZ security configuration
  - [x] SSH forwarding on port 1337 for Git operations
  - [x] Fail2ban protection and Tailscale-only SSH access
  - [x] Minimal attack surface, headless operation
- [x] **grey-area** application server (Culture GCU - versatile, multi-purpose): ✅ **COMPLETED**
  - [x] **Primary**: Forgejo Git hosting (git.geokkjer.eu) with reverse proxy integration
  - [x] **Secondary**: Jellyfin media server with NFS-mounted content
  - [x] **Additional**: Calibre-web e-book server and audiobook streaming
  - [x] **Infrastructure**: Container-focused (Podman), NFS client for media storage
  - [x] **Integration**: Central Git hosting accessible externally via reverse proxy
  - [x] **Network**: Integrated with Tailscale mesh and centralized hostname resolution
  - [x] **User Management**: Resolved UID conflicts with centralized sma user configuration
  - [ ] **Monitoring**: TBD (future enhancement)
  - [ ] **PostgreSQL**: Plan database services for applications requiring persistent storage
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
- [x] **SSH key management centralization**: ✅ **IMPLEMENTED and DEPLOYED**
  - [x] **Admin key** (`geir@geokkjer.eu-admin`): For sma user, server administration access
  - [x] **Development key** (`geir@geokkjer.eu-dev`): For geir user, git services, daily development
  - [x] **NixOS module**: `modules/security/ssh-keys.nix` centralizes key management
  - [x] **SSH client config**: Updated with role-based host patterns and key selection
  - [x] **Production deployment**: Successfully deployed on sleeper-service
  - [x] **Security benefits**: Principle of least privilege, limited blast radius if compromised
  - [x] **Usage examples**:
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

### 6.3 Advanced Deployment Strategies ✅ RESEARCH COMPLETED

#### Deploy-rs Migration (Priority: High) 📋 RESEARCHED
- [x] **Research deploy-rs capabilities** ✅ COMPLETED
  - [x] Rust-based deployment tool specifically designed for NixOS flakes
  - [x] Features: parallel deployment, automatic rollback, health checks, SSH-based
  - [x] Advanced capabilities: atomic deployments, magic rollback on failure
  - [x] Profile management: system, user, and custom profiles support
  - [x] Integration potential: Works with existing SSH keys and Tailscale network

- [ ] **Migration Planning**: Transition from custom `lab` script to deploy-rs
  - [ ] Create deploy-rs configuration in flake.nix for all 4 machines
  - [ ] Configure nodes: sleeper-service, grey-area, reverse-proxy, congenital-optimist
  - [ ] Set up health checks for critical services (NFS, Forgejo, Jellyfin, nginx)
  - [ ] Test parallel deployment capabilities across infrastructure
  - [ ] Implement automatic rollback for failed deployments
  - [ ] Document migration benefits and new deployment workflow

#### Deploy-rs Configuration Structure
```nix
# flake.nix additions
deploy.nodes = {
  sleeper-service = {
    hostname = "100.81.15.84";  # Tailscale IP
    profiles.system.path = deploy-rs.lib.x86_64-linux.activate.nixos 
      self.nixosConfigurations.sleeper-service;
    profiles.system.user = "root";
  };
  grey-area = {
    hostname = "100.119.86.92";
    profiles.system.path = deploy-rs.lib.x86_64-linux.activate.nixos
      self.nixosConfigurations.grey-area;
    # Health checks for Forgejo, Jellyfin services
  };
  reverse-proxy = {
    hostname = "100.96.189.104";
    profiles.system.path = deploy-rs.lib.x86_64-linux.activate.nixos
      self.nixosConfigurations.reverse-proxy;
    # Health checks for nginx, SSL certificates
  };
};
```

#### Migration Benefits
- **Atomic deployments**: Complete success or automatic rollback
- **Parallel deployment**: Deploy to multiple machines simultaneously  
- **Health checks**: Validate services after deployment
- **Connection resilience**: Better handling of SSH/network issues
- **Flake-native**: Designed specifically for NixOS flake workflows
- **Safety**: Magic rollback prevents broken deployments

#### Alternative: Guile Scheme Exploration (Priority: Low)
- [ ] **Research Guile Scheme for system administration**
  - [ ] Evaluate functional deployment scripting patterns
  - [ ] Compare with current shell script and deploy-rs approaches
  - [ ] Consider integration with GNU Guix deployment patterns
  - [ ] Assess learning curve vs. practical benefits for home lab use case
### 6.4 Writeup
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

## Current Status Overview (Updated December 2024)

### Infrastructure Deployment Status ✅ MAJOR MILESTONE ACHIEVED
✅ **PHASE 1**: Flakes Migration - **COMPLETED**  
✅ **PHASE 2**: Configuration Cleanup - **COMPLETED**  
✅ **PHASE 3**: System Upgrade & Validation - **COMPLETED**  
✅ **PHASE 5**: Home Lab Expansion - **4/4 MACHINES FULLY OPERATIONAL** 🎉

### Machine Status
- ✅ **congenital-optimist**: Development workstation (fully operational)
- ✅ **sleeper-service**: NFS file server with 903GB media library (fully operational) 
- ✅ **grey-area**: Application server with Forgejo, Jellyfin, Calibre-web, audiobook server (fully operational)
- ✅ **reverse-proxy**: External gateway with nginx, SSL termination, SSH forwarding (fully operational)

### Network Architecture Status
- ✅ **Tailscale Mesh**: All machines connected via secure mesh network (100.x.x.x addresses)
- ✅ **Hostname Resolution**: Centralized extraHosts module deployed across all machines  
- ✅ **NFS Storage**: Reliable media storage access via Tailscale network (sleeper-service → grey-area)
- ✅ **External Access**: Public services accessible via git.geokkjer.eu with SSL
- ✅ **SSH Infrastructure**: Centralized key management with role-based access patterns
- ✅ **Firewall Configuration**: Service ports properly configured across all machines

### Services Status - FULLY OPERATIONAL STACK 🚀
- ✅ **Git Hosting**: Forgejo operational at git.geokkjer.eu with SSH access on port 1337
- ✅ **Media Streaming**: Jellyfin with NFS-mounted content library (38 movies, 29 TV series)
- ✅ **E-book Management**: Calibre-web for book collections 
- ✅ **Audiobook Streaming**: Audiobook server with 79 audiobook collections
- ✅ **File Storage**: NFS server with 903GB media library accessible across network
- ✅ **Web Gateway**: Nginx reverse proxy with Let's Encrypt SSL and proper hostname mapping
- ✅ **User Management**: Consistent UID/GID configuration across machines (sma user: 1001/992)

### Infrastructure Achievements - COMPREHENSIVE DEPLOYMENT ✅
- ✅ **NFS Mount Resolution**: Fixed grey-area `/mnt/storage` → `/mnt/storage/media` dataset access
- ✅ **Network Exports**: Updated sleeper-service NFS exports for Tailscale network (100.64.0.0/10)
- ✅ **Service Discovery**: Corrected reverse-proxy hostname mapping from "apps" to "grey-area"  
- ✅ **Firewall Management**: Added port 3000 for Forgejo service accessibility
- ✅ **SSH Forwarding**: Configured SSH proxy on port 1337 for Git operations
- ✅ **SSL Termination**: Let's Encrypt certificates working for git.geokkjer.eu
- ✅ **Data Verification**: All media content accessible (movies, TV, music, audiobooks, books)
- ✅ **Deployment Tools**: Custom `lab` command operational for infrastructure management

### Current Operational Status
**🟢 ALL CORE INFRASTRUCTURE DEPLOYED AND OPERATIONAL**
- **4/4 machines deployed** with full service stack
- **External access verified**: `curl -I https://git.geokkjer.eu` returns HTTP/2 200
- **NFS connectivity confirmed**: Media files accessible across network via Tailscale
- **Service integration complete**: Forgejo, Jellyfin, Calibre-web, audiobook server running
- **Network mesh stable**: All machines connected via Tailscale with centralized hostname resolution

### Next Phase Priorities
- [ ] **PHASE 4**: GNU Stow + Literate Emacs Setup
  - [ ] Set up GNU Stow infrastructure for regular dotfiles (zsh, git, tmux, starship)
  - [ ] Create comprehensive Emacs literate configuration with org-mode
  - [ ] Implement selective deployment per machine type (workstation vs server)
  - [ ] Integration with existing NixOS system-level configuration
- [ ] **PHASE 6**: Advanced Features & Deploy-rs Migration
  - [ ] Migrate from custom `lab` script to deploy-rs for improved deployment
  - [ ] Implement system monitoring and alerting infrastructure  
  - [ ] Set up automated backup services for critical data
  - [ ] Create health checks and deployment validation
- [ ] **Documentation & Knowledge Sharing**
  - [ ] Comprehensive blog post series documenting the full home lab journey
  - [ ] User guides for GNU Stow + literate Emacs configuration workflow
  - [ ] Deploy-rs migration guide and lessons learned
- [ ] **Future Enhancements**
  - [ ] User ID consistency cleanup (sma user UID alignment across machines)
  - [ ] CI/CD integration with Forgejo for automated testing and deployment

---

## Success Criteria

### Core Infrastructure ✅ FULLY ACHIEVED 🎉
- [x] System boots reliably with flake configuration
- [x] All current functionality preserved  
- [x] NixOS 25.05 running stable across all machines
- [x] Configuration is modular and maintainable
- [x] User environment fully functional with all packages
- [x] Modern CLI tools and aliases working
- [x] Console theming preserved
- [x] Virtualization stack operational
- [x] **Multi-machine expansion completed (4/4 machines deployed)**
- [x] Development workflow improved with Git hosting

### Service Architecture ✅ FULLY ACHIEVED 🚀  
- [x] NFS file server operational with reliable network access via Tailscale
- [x] Git hosting with external access via reverse proxy (git.geokkjer.eu)
- [x] Media services with shared storage backend (Jellyfin + 903GB library)
- [x] E-book and audiobook management services operational
- [x] Secure external access with SSL termination and SSH forwarding
- [x] Network mesh connectivity with centralized hostname resolution
- [x] **All services verified operational and accessible externally**

### Network Integration ✅ FULLY ACHIEVED 🌐
- [x] Tailscale mesh network connecting all infrastructure machines
- [x] Centralized hostname resolution via extraHosts module
- [x] NFS file sharing working reliably over network
- [x] SSH key management with role-based access patterns
- [x] Firewall configuration properly securing all services
- [x] **External domain (git.geokkjer.eu) with SSL certificates working**

### Outstanding Enhancement Goals 🔄
- [ ] Literate dotfiles workflow established with org-mode  
- [ ] Documentation complete for future reference and blog writeup
- [ ] System monitoring and alerting infrastructure (Prometheus/Grafana)
- [ ] Automated deployment and maintenance improvements
- [ ] Automated backup services for critical data
- [ ] User ID consistency cleanup across machines

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
- **Container-focused architecture** using Podman
- **PostgreSQL database** for Forgejo
- **Concurrent multi-service deployment capability**
- **Secondary services**: Jellyfin (with transcoding), Nextcloud, Grafana
- Integration hub for all home lab development projects
- Culture name fits: "versatile ship handling varied, ambiguous tasks"
- Central point for CI/CD pipelines and automation

### Home Lab Philosophy
- Emacs org-mode literate programming approach provides better control than Home Manager
- Culture ship names create memorable, characterful infrastructure
- Modular NixOS configuration allows easy machine additions
- Per-user dotfiles structure scales across multiple machines
- Tailscale provides secure network foundation for multi-machine setup

#### Recent Critical Issue Resolution (December 2024) 🔧

**NFS Mount and Service Integration Issues - RESOLVED**

1. **NFS Dataset Structure Resolution**:
   - **Problem**: grey-area couldn't access media files via NFS mount
   - **Root Cause**: ZFS dataset structure confusion - mounting `/mnt/storage` vs `/mnt/storage/media`
   - **Solution**: Updated grey-area NFS mount from `sleeper-service:/mnt/storage` to `sleeper-service:/mnt/storage/media`
   - **Result**: All media content now accessible (38 movies, 29 TV series, 9 music albums, 79 audiobooks)

2. **NFS Network Export Configuration**:
   - **Problem**: NFS exports only configured for local network (10.0.0.0/24)
   - **Root Cause**: Missing Tailscale network access in NFS exports
   - **Solution**: Updated sleeper-service NFS exports to include Tailscale network (100.64.0.0/10)
   - **Result**: Reliable NFS connectivity over Tailscale mesh network

3. **Conflicting tmpfiles Rules**:
   - **Problem**: systemd tmpfiles creating conflicting directory structures for NFS exports
   - **Root Cause**: tmpfiles.d rules interfering with ZFS dataset mounting
   - **Solution**: Commented out conflicting tmpfiles rules in sleeper-service configuration
   - **Result**: Clean NFS export structure without mounting conflicts

4. **Forgejo Service Accessibility**:
   - **Problem**: git.geokkjer.eu returning connection refused errors
   - **Root Cause**: Multiple issues - firewall ports, hostname mapping, SSH forwarding
   - **Solutions Applied**:
     - Added port 3000 to grey-area firewall configuration
     - Fixed reverse-proxy nginx configuration: `http://apps:3000` → `http://grey-area:3000`
     - Updated SSH forwarding: `apps:22` → `grey-area:22` for port 1337
   - **Result**: External access verified - `curl -I https://git.geokkjer.eu` returns HTTP/2 200

5. **Hostname Resolution Consistency**:
   - **Problem**: Inconsistent hostname references across configurations ("apps" vs "grey-area")
   - **Root Cause**: Legacy hostname references in reverse-proxy configuration
   - **Solution**: Updated all configurations to use consistent "grey-area" hostname
   - **Result**: Proper service discovery and reverse proxy routing

6. **User ID Consistency Challenge**:
   - **Current State**: sma user has UID 1003 on grey-area vs 1001 on sleeper-service
   - **Workaround**: NFS access working via group permissions (users group: GID 100)
   - **Future Fix**: Implement centralized UID management across all machines

#### Recent Troubleshooting & Solutions (June 2025):
8. **NFS Dataset Structure**: Proper understanding of ZFS dataset hierarchy crucial for NFS exports
   - `/mnt/storage` vs `/mnt/storage/media` dataset mounting differences
   - NFS exports must match actual ZFS dataset structure, not subdirectories
   - Client mount paths must align with server export paths for data access
9. **Network Transition Management**: Tailscale vs local network connectivity during deployment
   - NFS exports need both local (10.0.0.0/24) and Tailscale (100.64.0.0/10) network access
   - extraHosts module provides consistent hostname resolution across network changes
   - Firewall configuration must accommodate service ports for external access
10. **Reverse Proxy Configuration**: Hostname consistency critical for proxy functionality  
    - nginx upstream configuration must use correct hostnames (grey-area not apps)
    - Service discovery relies on centralized hostname resolution modules
    - SSL certificate management works seamlessly with proper nginx configuration
11. **Service Integration**: Multi-machine service architecture requires coordinated configuration
    - Forgejo deployment spans grey-area (service) + reverse-proxy (gateway) + DNS (domain)  
    - NFS client/server coordination requires matching export/mount configurations
    - User ID consistency across machines essential for NFS file access permissions
12. **Firewall Management**: Service-specific port configuration essential for functionality
    - Application servers need service ports opened (3000 for Forgejo, etc.)
    - Reverse proxy needs external ports (80, 443, 1337) and internal connectivity  
    - SSH access coordination between local and Tailscale networks for security
