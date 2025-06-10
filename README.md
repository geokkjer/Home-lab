<div align="center">
  <img src="assets/nixos_logo.svg" alt="NixOS Logo" width="120" height="150">
  
  # NixOS Home Lab Infrastructure
  
  [![NixOS](https://img.shields.io/badge/NixOS-25.05-blue.svg)](https://nixos.org/)
  [![Flakes](https://img.shields.io/badge/Nix-Flakes-green.svg)](https://nixos.wiki/wiki/Flakes)
  [![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
</div>

Modular NixOS flake configuration for multi-machine home lab infrastructure. Features declarative system configuration, centralized user management, and scalable service deployment across development workstations and server infrastructure.

# Vibe DevSecOpsing with claude-sonnet 4 and github-copilot
A project about handling pets. If you want to handle sheep, look elsewhere :-) 

## Quick Start

```bash
# Clone repository
git clone <repository-url> Home-lab
cd Home-lab

# Validate configuration
nix flake check

# Test configuration (temporary, reverts on reboot)
sudo nixos-rebuild test --flake .#<machine-name>

# Apply configuration permanently  
sudo nixos-rebuild switch --flake .#<machine-name>
```

## Architecture Overview

### Machine Types
- **Development Workstation** - High-performance development environment with desktop environments
- **File Server** - ZFS storage with NFS services and media management
- **Application Server** - Containerized services (Git hosting, media server, web applications)
- **Reverse Proxy** - External gateway with SSL termination and service routing

### Technology Stack
- **Base OS**: <img src="assets/nixos_logo.svg" alt="NixOS" width="20" height="25" style="vertical-align: middle; margin-right: 5px;">NixOS 25.05 with Nix Flakes
- **Configuration**: Modular, declarative system configuration
- **Virtualization**: Incus containers, Libvirt/QEMU VMs, Podman containers
- **Desktop**: GNOME, Cosmic, Sway window managers
- **Storage**: ZFS with snapshots, automated mounting, NFS network storage
- **Network**: Tailscale mesh VPN with centralized hostname resolution

## Project Structure

Modular configuration organized for scalability and maintainability:

```
Home-lab/
├── flake.nix              # Main flake configuration
├── flake.lock             # Dependency lock file
├── machines/              # Machine-specific configurations
│   ├── workstation/       # Development machine config
│   ├── file-server/       # NFS storage server
│   ├── app-server/        # Containerized services
│   └── reverse-proxy/     # External gateway
├── modules/               # Reusable NixOS modules
│   ├── common/           # Base system configuration
│   ├── desktop/          # Desktop environment modules
│   ├── development/      # Development tools
│   ├── services/         # Service configurations
│   ├── users/            # User management
│   └── virtualization/   # Container and VM setup
├── packages/             # Custom packages and tools
└── research/             # Documentation and analysis
```

## Configuration Philosophy

<img src="assets/nixos_logo.svg" alt="NixOS" width="30" height="37" style="float: left; margin-right: 10px; margin-top: 5px;">

### Modular Design
- **Single Responsibility**: Each module handles one aspect of system configuration
- **Composable**: Modules can be mixed and matched per machine requirements
- **Testable**: Individual modules can be validated independently
- **Documented**: Clear documentation for module purpose and configuration

### User Management Strategy
- **Role-based Users**: Separate users for desktop vs server administration
- **Centralized Configuration**: Consistent user setup across all machines
- **Security Focus**: SSH key management and privilege separation
- **Literate Dotfiles**: Org-mode documentation for complex configurations

### Network Architecture
- **Mesh VPN**: Tailscale for secure inter-machine communication
- **Service Discovery**: Centralized hostname resolution
- **Firewall Management**: Service-specific port configuration
- **External Access**: Reverse proxy with SSL termination

## Development Workflow

### Local Testing
```bash
# Validate configuration syntax
nix flake check

# Build without applying changes
nix build .#nixosConfigurations.<machine>.config.system.build.toplevel

# Test configuration (temporary)
sudo nixos-rebuild test --flake .#<machine>

# Apply configuration permanently
sudo nixos-rebuild switch --flake .#<machine>
```

### Git Workflow
1. **Feature Branch**: Create branch for configuration changes
2. **Local Testing**: Validate changes with `nixos-rebuild test`
3. **Pull Request**: Submit changes for review
4. **Deploy**: Apply configuration to target machines

### Remote Deployment
- **SSH-based**: Remote deployment via secure shell
- **Atomic Updates**: Complete success or automatic rollback
- **Health Checks**: Service validation after deployment
- **Centralized Management**: Single repository for all infrastructure

## Service Architecture

### Core Services
- **Git Hosting**: Self-hosted Git with CI/CD capabilities
- **Media Server**: Streaming with transcoding support
- **File Storage**: NFS network storage with ZFS snapshots
- **Web Gateway**: Reverse proxy with SSL and external access
- **Container Platform**: Podman for containerized applications

### Service Discovery
- **Internal DNS**: Tailscale for mesh network resolution
- **External DNS**: Public domain with SSL certificates
- **Service Mesh**: Inter-service communication via secure network
- **Load Balancing**: Traffic distribution and failover

### Data Management
- **ZFS Storage**: Copy-on-write filesystem with snapshots
- **Network Shares**: NFS for cross-machine file access
- **Backup Strategy**: Automated snapshots and external backup
- **Data Integrity**: Checksums and redundancy

## Security Model

### Network Security
- **VPN Mesh**: All inter-machine traffic via Tailscale
- **Firewall Rules**: Service-specific port restrictions
- **SSH Hardening**: Key-based authentication only
- **Fail2ban**: Automated intrusion prevention

### User Security
- **Role Separation**: Administrative vs daily-use accounts
- **Key Management**: Centralized SSH key distribution
- **Privilege Escalation**: Sudo access only where needed
- **Service Accounts**: Dedicated accounts for automated services

### Infrastructure Security
- **Configuration as Code**: All changes tracked in version control
- **Atomic Deployments**: Rollback capability for failed changes
- **Secret Management**: Encrypted secrets with controlled access
- **Security Updates**: Regular dependency updates

## Testing Strategy

### Automated Testing
- **Syntax Validation**: Nix flake syntax checking
- **Build Testing**: Configuration build verification
- **Module Testing**: Individual component validation
- **Integration Testing**: Full system deployment tests

### Manual Testing
- **Boot Validation**: System startup verification
- **Service Health**: Application functionality checks
- **Network Connectivity**: Inter-service communication tests
- **User Environment**: Desktop and development tool validation

## Deployment Status

### Infrastructure Maturity
- ✅ **Multi-machine Configuration**: 4 machines deployed
- ✅ **Service Integration**: Git hosting, media server, file storage
- ✅ **Network Mesh**: Secure VPN with service discovery
- ✅ **External Access**: Public services with SSL termination
- ✅ **Centralized Management**: Single repository for all infrastructure

### Current Capabilities
- **Development Environment**: Full IDE setup with multiple desktop options
- **File Services**: Network storage with 900GB+ media library
- **Git Hosting**: Self-hosted with external access
- **Media Streaming**: Movie and TV series streaming with transcoding
- **Container Platform**: Podman-based containerized services

## Documentation

- **[Migration Plan](plan.md)**: Detailed implementation roadmap
- **[Development Workflow](DEVELOPMENT_WORKFLOW.md)**: Contribution guidelines
- **[Branching Strategy](BRANCHING_STRATEGY.md)**: Git workflow and conventions
- **[AI Instructions](instruction.md)**: Agent guidance for system management


## License

MIT License - see [LICENSE](LICENSE) for details.

---

<div align="center">
  <img src="assets/nixos_logo.svg" alt="NixOS Logo" width="40" height="50">
  <br>
  <em>Infrastructure designed for reliability, security, and maintainability.</em>
</div>
