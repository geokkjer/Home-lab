# User Configurations

This directory contains modular user configurations for the home lab infrastructure.

## Philosophy

Following the Culture ship naming convention and Emacs org-mode literate programming approach, user configurations are organized to be:

- **Modular**: Each user has their own configuration module
- **Shared**: Common settings are in `common.nix`
- **Character-driven**: User names follow Culture character names
- **Functional**: Focus on practical daily use and system administration

## User Accounts

### Primary Users

#### `geir` - Primary User Account
- **Role**: Development, desktop use, daily computing
- **Access**: Full desktop environments (GNOME, Cosmic, Sway)
- **Focus**: Development tools, creative applications, multimedia
- **Groups**: wheel, networkmanager, libvirt, incus-admin, podman, audio, video, render

#### `sma` - System Administrator  
- **Full Name**: Named after Diziet Sma (Special Circumstances agent)
- **Role**: System administration, security oversight, maintenance
- **Access**: SSH-only, command-line focused
- **Focus**: Monitoring, containers, security, infrastructure management
- **Groups**: wheel, networkmanager, libvirt, incus-admin, podman
- **Security**: SSH key authentication only, passwordless sudo

### Service Accounts (Future)
- Consider adding service-specific users for:
  - `forgejo-admin`: Forgejo administration
  - `media-admin`: Jellyfin/media server management
  - `backup-agent`: Automated backup operations

## File Structure

```
modules/users/
├── common.nix       # Shared user settings and packages
├── geir.nix         # Primary user configuration
├── sma.nix          # Admin user configuration
└── README.md        # This documentation
```

## Design Principles

### Security
- SSH key-based authentication for admin users
- Principle of least privilege
- Separate admin and daily-use accounts
- No root login allowed

### Convenience
- Modern CLI tools and aliases
- Development-focused package selection
- Shell enhancements (zsh, starship, syntax highlighting)
- Container and virtualization integration

### Consistency
- Common aliases and environment variables
- Shared shell configuration
- Standardized directory permissions
- Culture-inspired naming convention

## Integration Points

### With System Configuration
- Desktop environment modules automatically enable GUI applications
- Virtualization modules grant appropriate group memberships
- Network modules configure user network access

### With User Configs
- Literate configurations stored in `/home/geir/Home-lab/user_configs/`
- Emacs org-mode files for complex configurations
- Automatic tangling of configuration files
- Version control integration

### With Services
- User accounts automatically configured for enabled services
- Container runtime access for development users
- Monitoring and administration access for admin users

## Usage Examples

### Adding a New User
1. Create new module file: `modules/users/new-username.nix`
2. Choose appropriate Culture character name
3. Define role-specific packages and groups
4. Import in machine configuration
5. Document in this README

### Modifying User Access
- Edit `extraGroups` for service access
- Update `packages` for new tools
- Modify shell aliases for workflow improvements
- Adjust sudo rules for administrative access

### Security Considerations
- Regular audit of user accounts and permissions
- SSH key rotation schedule
- Monitor sudo usage and administrative actions
- Review group memberships quarterly

## Culture Character Reference

- **Diziet Sma**: Pragmatic SC agent, perfect for system administration
- **Cheradenine Zakalwe**: Complex SC agent, high-capability operations  
- **Jernau Morat Gurgeh**: Strategic game player, systematic thinking
- **Perosteck Balveda**: Professional SC agent, reliable operations

Choose character names that reflect the user's role and personality within the home lab infrastructure.
