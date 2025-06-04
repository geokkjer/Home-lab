# 🏠 NixOS Home Lab Infrastructure

[![NixOS](https://img.shields.io/badge/NixOS-25.05-blue.svg)](https://nixos.org/)
[![Flakes](https://img.shields.io/badge/Nix-Flakes-green.svg)](https://nixos.wiki/wiki/Flakes)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Infrastructure as Code for a multi-machine NixOS home lab environment using flakes and modular configuration.

## 🚀 Quick Start

```bash
# Clone the repository
git clone <repository-url> Home-lab
cd Home-lab

# Check flake configuration
nix flake check

# Build configuration (test)
sudo nixos-rebuild test --flake .#congenital-optimist

# Apply configuration (permanent)
sudo nixos-rebuild switch --flake .#congenital-optimist
```

## 🏗️ Architecture Overview

### Machines
- **`congenital-optimist`** - AMD Threadripper workstation (primary development)
- **`sleeper-service`** - Intel Xeon E3-1230 V2 file server (network storage)

### Technology Stack
- **OS**: NixOS 25.05 (Warbler)
- **Configuration**: Nix Flakes with modular structure
- **Virtualization**: Incus, Libvirt/QEMU, Podman
- **Desktop**: GNOME, Cosmic, Sway
- **Storage**: ZFS with snapshots and NFS
- **Network**: Tailscale mesh networking

## 📁 Repository Structure

```
Home-lab/
├── flake.nix              # Main flake configuration
├── flake.lock             # Locked dependency versions
├── machines/              # Machine-specific configurations
│   ├── congenital-optimist/  # AMD workstation
│   └── sleeper-service/      # Intel file server
├── modules/               # Reusable NixOS modules
│   ├── common/           # Shared system configuration
│   ├── desktop/          # Desktop environment modules
│   ├── development/      # Development tools and editors
│   ├── hardware/         # Hardware-specific configurations
│   ├── services/         # Service configurations
│   ├── system/           # Core system modules
│   ├── users/            # User configurations
│   └── virtualization/   # Container and VM setup
├── users/                # User-specific configurations
│   └── geir/            # Primary user configuration
│       ├── dotfiles/    # Literate configuration with org-mode
│       └── user.nix     # System-level user config
├── overlays/             # Nix package overlays
├── packages/             # Custom package definitions
└── secrets/              # Encrypted secrets (future)
```

## 🔧 Configuration Management

### Modular Design
Each aspect of the system is organized into focused modules:

- **Desktop Environments**: Separate modules for GNOME, Cosmic, and Sway
- **Virtualization**: Independent Incus, Libvirt, and Podman configurations
- **Development**: Modular tool configurations for different workflows
- **Hardware**: Hardware-specific optimizations and drivers

### Literate Programming
User configurations use Emacs org-mode for literate programming:
- Self-documenting configuration files
- Automatic tangling from `.org` to configuration files
- Version-controlled documentation alongside code

## 🚀 Deployment Workflow

### Local Development
```bash
# Check configuration syntax
nix flake check

# Test configuration without switching
sudo nixos-rebuild test --flake .#<machine-name>

# Build configuration
sudo nixos-rebuild build --flake .#<machine-name>

# Apply configuration
sudo nixos-rebuild switch --flake .#<machine-name>
```

### GitOps Workflow
1. **Feature Branch**: Create branch for configuration changes
2. **Local Testing**: Test changes with `nixos-rebuild test`
3. **Pull Request**: Submit PR with configuration validation
4. **Automated Testing**: CI pipeline validates configuration
5. **Review & Merge**: Code review and merge to main
6. **Deployment**: Automated or manual deployment to machines

## 🔐 Security & Secrets

### Current Approach
- No secrets in git repository
- Manual secret management during initial setup
- ZFS encryption for sensitive data

### Planned Improvements
- **agenix** or **sops-nix** for encrypted secrets in git
- **age** keys for secret encryption/decryption
- **CI/CD** integration with secret management

## 🎯 Machine Specifications

### CongenitalOptimist (AMD Workstation)
- **CPU**: AMD Threadripper (details in hardware-configuration.nix)
- **GPU**: AMD (with proper drivers and virtualization passthrough)
- **Storage**: ZFS pools (zpool + stuffpool)
- **Use Case**: Primary development, virtualization, desktop environments
- **Services**: Development tools, desktop environments, VM host

### SleeperService (Intel File Server)
- **CPU**: Intel Xeon E3-1230 V2 @ 3.70GHz (4 cores, 8 threads)
- **Memory**: 16GB RAM
- **Storage**: ZFS with redundancy
- **Use Case**: Network storage, file sharing, backup services
- **Services**: NFS, Samba, automated backups, monitoring

## 🧪 Testing Strategy

### Automated Testing (Planned)
- **Configuration Validation**: `nix flake check` in CI
- **Build Testing**: Test builds for all machines
- **Module Testing**: Individual module validation
- **Integration Testing**: Full system build verification

### Manual Testing Checklist
- [ ] System boots successfully
- [ ] Desktop environments functional
- [ ] Virtualization stack operational
- [ ] Network services accessible
- [ ] User environment complete
- [ ] Development tools working

## 📈 Monitoring & Maintenance

### Health Checks
- System generation switching
- Service status monitoring
- ZFS pool health
- Network connectivity
- Resource utilization

### Backup Strategy
- **ZFS Snapshots**: Automatic filesystem snapshots
- **Configuration Backups**: Git repository with full history
- **Data Backups**: Automated backup services on SleeperService
- **Recovery Procedures**: Documented rollback processes

## 🔄 CI/CD Pipeline (Planned)

### Validation Pipeline
```yaml
# Planned GitHub Actions workflow
- Syntax Check: nix flake check
- Build Test: nix build .#nixosConfigurations.<machine>
- Security Scan: Nix security auditing
- Documentation: Update system documentation
```

### Deployment Pipeline
```yaml
# Planned deployment automation
- Staging: Deploy to test environment
- Integration Tests: Automated system testing
- Production: Deploy to production machines
- Rollback: Automatic rollback on failure
```

## 🤝 Contributing

### Development Workflow
1. Fork/clone repository
2. Create feature branch
3. Make configuration changes
4. Test locally with `nixos-rebuild test`
5. Submit pull request
6. Address review feedback
7. Merge after approval

### Module Development Guidelines
- Keep modules focused and single-purpose
- Document module options and usage
- Test modules independently when possible
- Follow consistent naming conventions
- Include example configurations

## 📖 Documentation

- **[Plan](plan.md)**: Detailed migration and development plan
- **[Instructions](instruction.md)**: Step-by-step setup instructions
- **[Machine Documentation](machines/)**: Per-machine documentation
- **[Module Documentation](modules/)**: Module-specific documentation
- **[User Documentation](users/)**: User configuration documentation

## 🎯 Roadmap

### Phase 1: Flakes Migration ✅
- [x] Convert to flake-based configuration
- [x] Modularize desktop environments
- [x] Add comprehensive virtualization
- [x] Set up GitOps foundation

### Phase 2: Configuration Cleanup
- [ ] Optimize modular structure
- [ ] Enhance documentation
- [ ] Standardize module interfaces

### Phase 3: Multi-Machine Expansion
- [ ] Add SleeperService configuration
- [ ] Implement service modules
- [ ] Set up network storage

### Phase 4: Automation & CI/CD
- [ ] Implement automated testing
- [ ] Set up deployment pipelines
- [ ] Add monitoring and alerting

### Phase 5: Advanced Features
- [ ] Secrets management
- [ ] Advanced monitoring
- [ ] Backup automation

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **NixOS Community** for excellent documentation and support
- **Culture Ship Names** for inspiring machine nomenclature
- **Emacs Community** for literate programming inspiration
- **Home Lab Community** for sharing knowledge and experience

---

*"The ship had decided to call itself the Arbitrary, presumably for much the same reason."*
