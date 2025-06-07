# Users Directory Structure

This directory contains per-user configurations and dotfiles for the Home-lab infrastructure, organized to support multiple users across multiple machines.

## Directory Organization

### `geir/`
Primary user configuration for geir:
- `user.nix` - NixOS user configuration (packages, groups, shell)
- `dotfiles/` - Literate programming dotfiles using org-mode
  - `README.org` - Main literate configuration file
  - `emacs/` - Emacs-specific configurations
  - `shell/` - Shell configurations (zsh, bash, etc.)
  - `editors/` - Editor configurations (neovim, vscode)

### Future Users
Additional user directories will follow the same pattern:
- `admin/` - Administrative user for system management
- `service/` - Service accounts for automation
- `guest/` - Temporary/guest user configurations

## User Configuration Philosophy

### NixOS Integration
Each user has a `user.nix` file that defines:
- User account settings (shell, groups, home directory)
- User-specific packages
- System-level user configurations
- Integration with home lab services

### Literate Dotfiles
Each user's `dotfiles/README.org` serves as:
- Single source of truth for all user configurations
- Self-documenting setup with rationale
- Auto-tangling to generate actual dotfiles
- Version-controlled configuration history

### Multi-Machine Consistency
User configurations are designed to work across machines:
- congenital-optimist: Full development environment
- sleeper-service: Minimal server access
- Future machines: Consistent user experience

## Dotfiles Structure

### `dotfiles/README.org`
Main literate configuration file containing:
- Shell configuration (zsh, starship, aliases)
- Editor configurations (emacs, neovim)
- Development tool settings
- Git configuration
- Machine-specific customizations

### Subdirectories
- `emacs/` - Generated Emacs configuration files
- `shell/` - Generated shell configuration files
- `editors/` - Generated editor configuration files

## Usage Examples

### Importing User Configuration
```nix
# In machine configuration
imports = [
  ../../users/geir/user.nix
];
```

### Adding New User
1. Create user directory: `users/newuser/`
2. Copy and adapt `user.nix` template
3. Create `dotfiles/README.org` with user-specific configs
4. Import in machine configurations as needed

### Tangling Dotfiles
```bash
# From user's dotfiles directory
cd users/geir/dotfiles
emacs --batch -l org --eval "(org-babel-tangle-file \"README.org\")"
```

## Design Principles

- **User Isolation**: Each user's configs are self-contained
- **Machine Agnostic**: Configs work across different machines
- **Literate Programming**: All configs are documented and explained
- **Version Control**: Full history of configuration changes
- **Automation**: Auto-tangling and deployment workflows

## Security Considerations

- User-specific secrets managed separately
- Limited cross-user access
- Machine-appropriate privilege levels
- Service account isolation

## Naming Convention

- **User Directories**: lowercase (e.g., `geir/`, `admin/`)
- **Configuration Files**: descriptive names (e.g., `user.nix`, `README.org`)
- **Generated Files**: follow target application conventions