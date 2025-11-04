# GNU Stow Research Summary

## Overview

**GNU Stow** is a symlink farm manager that helps organize and deploy dotfiles and software packages. It creates symbolic links from a source directory to a target directory, making it ideal for managing dotfiles across multiple systems without copying files.

**Repository**: https://www.gnu.org/software/stow/
**Language**: Perl
**License**: GPL-3.0+
**Status**: Mature, stable, widely adopted

## Core Concept

Stow works by creating symbolic links from a "stow directory" (source) to a "target directory" (typically `$HOME`). Each subdirectory in the stow directory represents a "package" that can be independently stowed or unstowed.

```
dotfiles/               # Stow directory
├── zsh/               # Package: zsh configuration
│   └── .zshrc         # Will link to ~/.zshrc
├── emacs/             # Package: emacs configuration  
│   └── .emacs.d/      # Will link to ~/.emacs.d/
│       └── init.el
└── git/               # Package: git configuration
    └── .gitconfig     # Will link to ~/.gitconfig
```

## Key Features

### 🔗 **Symlink Management**
- **Non-destructive**: Creates symlinks without overwriting existing files
- **Conflict detection**: Warns about file conflicts before creating links
- **Tree folding**: Optimizes symlink structure for efficiency
- **Partial deployment**: Deploy only specific packages (e.g., just zsh config)

### 📦 **Package-Based Organization**
- **Modular structure**: Each application gets its own package directory
- **Selective deployment**: Install only needed configurations per machine
- **Easy maintenance**: Add/remove configurations without affecting others
- **Version control friendly**: Git can track each package independently

### 🛡️ **Safety Features**
- **Dry-run mode**: Preview what would be done without making changes
- **Conflict resolution**: Handles existing files and directories gracefully
- **Rollback capability**: Easy to unstow (remove) configurations
- **Target verification**: Ensures target directory exists and is writable

## Command Examples

```bash
# Basic stow operations
cd ~/dotfiles
stow zsh              # Deploy zsh configuration
stow emacs git        # Deploy multiple packages
stow --target=/opt/local mypackage  # Custom target directory

# Management operations
stow --delete zsh     # Remove zsh symlinks (unstow)
stow --restow emacs   # Restow (unstow then stow again)
stow --simulate zsh   # Dry run - show what would happen

# Advanced usage
stow --verbose zsh    # Verbose output
stow --no-folding emacs  # Disable tree folding optimization
stow --ignore='\.DS_Store' zsh  # Ignore certain files
```

## Directory Structure Example

```
~/dotfiles/                    # Stow directory
├── zsh/                      # Package: ZSH
│   ├── .zshrc               # → ~/.zshrc
│   └── .config/             # → ~/.config/
│       └── zsh/             # → ~/.config/zsh/
│           └── aliases.zsh  # → ~/.config/zsh/aliases.zsh
├── emacs/                   # Package: Emacs
│   ├── .emacs.d/           # → ~/.emacs.d/
│   │   ├── init.el         # → ~/.emacs.d/init.el
│   │   └── config.org      # → ~/.emacs.d/config.org
│   └── .config/            # → ~/.config/
│       └── emacs/          # → ~/.config/emacs/
├── git/                    # Package: Git
│   ├── .gitconfig         # → ~/.gitconfig
│   └── .gitignore_global  # → ~/.gitignore_global
└── nixos/                 # Package: NixOS-specific
    └── .config/           # → ~/.config/
        └── nixos/         # → ~/.config/nixos/
            └── user.nix   # → ~/.config/nixos/user.nix
```

## Comparison with Current Org-Mode Approach

| Feature | Current Org-Mode | GNU Stow |
|---------|------------------|----------|
| **Configuration format** | Literate programming | Traditional dotfiles |
| **Deployment method** | Tangle + copy/symlink | Automatic symlinking |
| **Documentation** | Embedded in code | Separate documentation |
| **Version control** | Single org file | Multiple files in packages |
| **Learning curve** | Steep (Emacs/Org) | Gentle (simple concept) |
| **Flexibility** | Very high | Moderate |
| **Maintenance** | Manual tangling | Automatic linking |
| **Cross-platform** | Excellent | Excellent |
| **NixOS integration** | Native | External tool |

## Advantages of GNU Stow

### ✅ **Simplicity & Reliability**
- **Minimal learning curve**: Understand symlinks = understand Stow
- **No dependencies**: Works on any Unix-like system with Perl
- **Predictable behavior**: Simple, well-defined symlink creation rules
- **Mature and stable**: Decades of development and testing

### ✅ **Flexible Organization**
- **Package-based**: Logical separation of application configurations
- **Selective deployment**: Deploy only needed configs per machine
- **Easy experimentation**: Test new configs without affecting others
- **Conflict handling**: Safe deployment with conflict detection

### ✅ **Version Control Benefits**
- **Git-friendly**: Each package can be tracked separately
- **Branching**: Different branches for different machine types
- **History**: Track changes to individual application configs
- **Collaboration**: Easy to share specific application configurations

### ✅ **Multi-Machine Management**
```bash
# Different configs for different machine types
stow --target=/home/geir desktop  # Desktop-specific configs
stow --target=/home/sma server    # Server-specific configs

# Machine-specific packages
dotfiles/
├── desktop-geir/     # Only for desktop user geir
├── server-sma/       # Only for server user sma
└── common/           # Shared configurations
```

## Disadvantages & Limitations

### ❌ **Compared to Org-Mode Approach**
- **No literate programming**: Configuration and documentation separate
- **Less integration**: Not as tightly integrated with Emacs workflow
- **File-based**: No ability to generate configs from templates
- **Static configuration**: Less dynamic than org-mode tangling

### ❌ **General Limitations**
- **Symlink visibility**: Some applications don't handle symlinks well
- **Permission issues**: Target directory permissions must be correct
- **Perl dependency**: Requires Perl runtime (usually not an issue)
- **No config generation**: Can't generate configs based on system state

## Integration with Current NixOS Setup

### 🎯 **Hybrid Approach Options**

**Option 1: Stow for Traditional Dotfiles**
```bash
# Keep org-mode for complex Emacs config
# Use Stow for simple dotfiles
dotfiles/
├── emacs.org          # Keep current literate approach
├── zsh/               # Simple dotfiles via Stow
│   └── .zshrc
└── git/
    └── .gitconfig
```

**Option 2: User-Specific Deployment**
```bash
# Different approaches per user
~/dotfiles-geir/       # Org-mode for desktop user
~/dotfiles-sma/        # Stow for server user (simpler configs)
```

**Option 3: NixOS + Stow Integration**
```nix
# In NixOS configuration
environment.systemPackages = [ pkgs.stow ];

# User activation script
system.userActivationScripts.stow-dotfiles = ''
  cd /home/geir/dotfiles
  ${pkgs.stow}/bin/stow --target=/home/geir common zsh git
'';
```

### 🔧 **Implementation Strategies**

**For Server Configurations (sma user):**
- Simple, minimal dotfiles via Stow
- Basic shell, git, and utility configurations
- No complex literate programming needed

**For Desktop Configurations (geir user):**
- Keep org-mode for Emacs (complex, well-integrated)
- Consider Stow for simpler applications (git, zsh aliases)
- Hybrid approach based on complexity

## Practical Examples

### 📝 **Basic Setup**
```bash
# Initialize dotfiles repository
mkdir ~/dotfiles
cd ~/dotfiles

# Create package structure
mkdir -p zsh git emacs

# Add configurations
echo 'alias ll="ls -la"' > zsh/.zshrc
echo '[user]
    name = Geir Okkenhaug Jerstad
    email = geir@geokkjer.eu' > git/.gitconfig

# Deploy configurations  
stow zsh git
```

### 🏠 **Home Lab Specific Usage**
```bash
# Machine-specific dotfiles
dotfiles/
├── server-common/     # Shared server configs
│   ├── .zshrc
│   └── .vimrc
├── sleeper-service/   # NFS-specific configs
│   └── .config/
│       └── nfs/
└── grey-area/         # Git server specific
    └── .gitconfig
    
# Deploy on sleeper-service
stow server-common sleeper-service

# Deploy on grey-area  
stow server-common grey-area
```

### 🔄 **Migration Strategy**
```bash
# Phase 1: Extract simple configs from org-mode
emacs config.org
# Tangle simple configs to separate files
# Create Stow packages for extracted configs

# Phase 2: Test Stow deployment
stow --simulate zsh    # Test before applying
stow zsh               # Deploy if tests pass

# Phase 3: Maintain hybrid approach
# Keep org-mode for complex configs (Emacs)
# Use Stow for simple configs (shell, git)
```

## Integration with Current Workflow

### 🎯 **Recommended Approach for Home Lab**

**Keep Current Org-Mode For:**
- Emacs configuration (complex, well-integrated)
- Complex, documented configurations
- Configurations that benefit from literate programming

**Use GNU Stow For:**
- Server user (sma) configurations
- Simple shell configurations
- Git configurations
- Application-specific dotfiles that don't need documentation

**Implementation Plan:**
1. **Create stow repository** for server configs
2. **Test on sleeper-service** with sma user
3. **Gradually migrate** simple configs from org-mode
4. **Maintain hybrid approach** based on complexity needs

## Conclusion & Recommendation

### 🎯 **For Our Home Lab Context**

**GNU Stow is excellent for:**
- Server configurations (sma user)
- Simple, stable dotfiles
- Multi-machine deployments
- Configurations that don't need complex documentation

**Current org-mode approach excels for:**
- Complex Emacs configurations
- Documented, literate configurations
- Generated configurations
- Desktop user (geir) complex setups

### 📋 **Action Plan**

1. **Immediate**: Create stow-based dotfiles repository for server users
2. **Short-term**: Deploy simple sma user configs via Stow on sleeper-service
3. **Medium-term**: Evaluate migration of simple configs from org-mode
4. **Long-term**: Maintain hybrid approach - Stow for simple, org-mode for complex

**Verdict**: GNU Stow is an excellent complement to our current org-mode approach. Use Stow for simple, server-side configurations while keeping org-mode for complex desktop configurations. This provides the best of both worlds - simplicity where appropriate, power where needed.