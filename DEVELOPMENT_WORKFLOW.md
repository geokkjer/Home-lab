# 🔄 Development Workflow for Home Lab Infrastructure

## Overview

This document outlines the development workflow for the NixOS Home Lab infrastructure, emphasizing GitOps principles, automated testing, and safe configuration management.

## 🚀 Quick Start for Contributors

### Prerequisites
- NixOS system with flakes enabled
- Git configured with your credentials
- Access to the home lab machines (if deploying)

### Initial Setup
```bash
# Clone the repository
git clone <repository-url> Home-lab
cd Home-lab

# Verify flake configuration
nix flake check

# Test configuration locally (safe, non-persistent)
sudo nixos-rebuild test --flake .#congenital-optimist
```

## 🔄 Development Cycle

### 1. Feature Development
```bash
# Start from develop branch
git checkout develop
git pull origin develop

# Create feature branch
git checkout -b feature/improve-desktop-config

# Make your changes
# ... edit configuration files ...

# Validate configuration
nix flake check

# Test configuration (non-persistent)
sudo nixos-rebuild test --flake .#congenital-optimist

# Stage and commit changes
git add .
git commit -m "feat(desktop): improve GNOME configuration

- Add GNOME extensions for better workflow
- Configure custom keybindings
- Optimize performance settings"

# Push to remote
git push origin feature/improve-desktop-config
```

### 2. Pull Request Process
1. **Create PR** from feature branch to `develop`
2. **Automated Testing** runs via GitHub Actions:
   - Flake syntax validation
   - Build tests for all machines
   - Security audit
   - Documentation checks
3. **Code Review** by team members
4. **Local Testing** by reviewers (optional but recommended)
5. **Merge** to develop after approval

### 3. Production Deployment
```bash
# Merge develop to main (after testing)
git checkout main
git merge develop

# Tag release
git tag -a v1.1.0 -m "Release v1.1.0: Desktop improvements"

# Deploy to production
sudo nixos-rebuild switch --flake .#congenital-optimist
```

## 🧪 Testing Strategy

### Automated Testing (CI/CD)
Our GitHub Actions pipeline automatically tests:

- **Syntax Validation**: `nix flake check --all-systems`
- **Build Testing**: Full system build for each machine
- **Security Scanning**: Check for secrets and vulnerabilities
- **Module Validation**: Individual module testing
- **Documentation**: Ensure docs are up to date

### Manual Testing Levels

#### Level 1: Basic Validation
```bash
# Quick syntax check
nix flake check

# Build without applying
nix build .#nixosConfigurations.congenital-optimist.config.system.build.toplevel
```

#### Level 2: Safe Testing
```bash
# Test configuration (temporary, reverts on reboot)
sudo nixos-rebuild test --flake .#congenital-optimist

# Verify services
systemctl status incus libvirtd podman.socket
```

#### Level 3: Persistent Testing
```bash
# Apply configuration permanently
sudo nixos-rebuild switch --flake .#congenital-optimist

# Create rollback point
sudo nixos-rebuild test --flake .#congenital-optimist --rollback
```

## 🏗️ Configuration Architecture

### Module Organization
```
modules/
├── common/          # Shared base configuration
├── desktop/         # Desktop environment modules
├── development/     # Development tools and editors
├── hardware/        # Hardware-specific configurations
├── services/        # Service configurations
├── system/          # Core system modules
├── users/           # User configurations
└── virtualization/  # Container and VM setup
```

### Best Practices

#### Module Design
- **Single Responsibility**: Each module handles one aspect
- **Configurable**: Use options for customization
- **Documented**: Include comments and documentation
- **Testable**: Design for easy testing

#### Configuration Changes
- **Small Commits**: Make focused, atomic changes
- **Test First**: Always test before committing
- **Document Impact**: Explain what changes and why
- **Consider Rollback**: Ensure changes can be reverted

## 🔐 Security Considerations

### Configuration Security
- **No Secrets in Git**: Never commit passwords, keys, or certificates
- **Proper Permissions**: Set correct file permissions in configuration
- **Firewall Rules**: Review network access changes
- **User Privileges**: Validate user group memberships

### Development Security
- **Branch Protection**: Main branch requires reviews
- **Signed Commits**: Consider GPG signing for production
- **Access Control**: Limit who can deploy to production
- **Audit Trail**: All changes tracked in git history

## 🚨 Emergency Procedures

### Rollback Procedures

#### Git-based Rollback
```bash
# Find last known good commit
git log --oneline

# Rollback to specific commit
git checkout <commit-hash>
sudo nixos-rebuild switch --flake .#congenital-optimist

# Or revert specific changes
git revert <bad-commit-hash>
```

#### NixOS Generation Rollback
```bash
# List available generations
sudo nixos-rebuild list-generations

# Rollback to previous generation
sudo nixos-rebuild switch --rollback

# Or switch to specific generation
sudo /nix/var/nix/profiles/system-<generation>-link/bin/switch-to-configuration switch
```

#### ZFS Snapshot Rollback
```bash
# List snapshots
zfs list -t snapshot

# Rollback to snapshot (destructive!)
zfs rollback zpool/root@pre-update-snapshot
```

### Recovery Boot
If system fails to boot:
1. Boot from NixOS installation media
2. Import ZFS pools if needed
3. Chroot into system
4. Rollback to working configuration
5. Rebuild and reboot

## 📊 Monitoring and Maintenance

### Health Checks
Regular monitoring should include:

- **System Generations**: Track configuration changes
- **Service Status**: Monitor critical services
- **Storage Health**: ZFS pool status
- **Security Updates**: Regular dependency updates
- **Performance**: System resource utilization

### Automated Maintenance
Our CI/CD pipeline handles:

- **Weekly Dependency Updates**: Automated flake.lock updates
- **Security Scanning**: Regular vulnerability checks
- **Documentation Updates**: Auto-generated documentation
- **Build Verification**: Continuous build testing

### Manual Maintenance Tasks

#### Weekly
- Review open pull requests
- Check system logs for errors
- Verify backup operations
- Update development environment

#### Monthly
- Security audit of configurations
- Clean up old git branches
- Review and update documentation
- Performance optimization review

#### Quarterly
- Major dependency updates
- Infrastructure planning review
- Security policy updates
- Disaster recovery testing

## 🎯 Performance Optimization

### Build Performance
- **Nix Binary Cache**: Use shared cache for faster builds
- **Parallel Builds**: Configure appropriate build parallelism
- **Module Organization**: Optimize module import structure
- **Build Dependencies**: Minimize unnecessary dependencies

### Runtime Performance
- **Service Configuration**: Optimize service settings
- **Hardware Configuration**: Tune for specific hardware
- **Resource Allocation**: Configure appropriate limits
- **Monitoring**: Track performance metrics

## 📝 Documentation Standards

### Code Documentation
- **Module Comments**: Explain complex configurations
- **Option Documentation**: Document custom options
- **Example Usage**: Provide usage examples
- **Change Rationale**: Explain why changes were made

### Process Documentation
- **Workflow Updates**: Keep process docs current
- **Decision Records**: Document architectural decisions
- **Troubleshooting**: Maintain troubleshooting guides
- **Knowledge Transfer**: Document tribal knowledge

## 🤝 Collaboration Guidelines

### Communication
- **Clear Commit Messages**: Use conventional commit format
- **Descriptive PRs**: Explain what and why
- **Review Feedback**: Provide constructive feedback
- **Documentation**: Update docs with changes

### Code Review Focus Areas
1. **Correctness**: Does the configuration work?
2. **Security**: Any security implications?
3. **Performance**: Impact on system performance?
4. **Maintainability**: Is the code maintainable?
5. **Documentation**: Are docs updated?

## 🔗 Related Documentation

- [**README.md**](README.md): Project overview and quick start
- [**BRANCHING_STRATEGY.md**](BRANCHING_STRATEGY.md): Git workflow details
- [**plan.md**](plan.md): Migration and development plan
- [**CI/CD Pipeline**](.github/workflows/ci.yml): Automated testing configuration

---

*"The important thing is not to panic."* - Douglas Adams

This workflow ensures reliable, tested, and maintainable infrastructure while enabling rapid development and safe deployments.
