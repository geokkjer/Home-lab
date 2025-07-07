# Home Manager Refactoring Plan

## Executive Summary

This document outlines a comprehensive plan to refactor the current user configuration management system from a system-centric NixOS approach to a user-centric Home Manager approach. The refactoring will provide better separation of concerns, improved dotfile management, user-level service management, and enhanced maintainability while preserving the existing sophisticated Emacs configuration system.

## Current Architecture Analysis

### Strengths of Current System
- **Declarative Configuration**: All configurations defined in Nix modules
- **Sophisticated Emacs Integration**: Profile-based Emacs system with Nix tool integration
- **Multi-machine Consistency**: Centralized configuration repository
- **Modular Design**: Clear separation between machines, modules, and users
- **Nix Tool Integration**: Excellent integration with Nix-provided tools and language servers

### Current Limitations
- **System-level User Configuration**: User preferences mixed with system configuration
- **No User Service Management**: Cannot manage user-level systemd services
- **Manual Dotfile Deployment**: Limited systematic dotfile management
- **Monolithic User Modules**: Large, complex user configuration files
- **No Per-user Package Management**: User packages defined at system level

## Refactoring Goals

### Primary Objectives
1. **Separate User from System Configuration**: Move user-specific settings to Home Manager
2. **Enable User Service Management**: Leverage Home Manager's systemd user service support
3. **Systematic Dotfile Management**: Implement proper dotfile deployment and management
4. **Preserve Emacs Excellence**: Maintain current sophisticated Emacs configuration
5. **Improve Maintainability**: Reduce complexity and improve modularity

### Secondary Objectives
1. **Multi-user Support**: Better support for multiple users with different preferences
2. **Development Environment Isolation**: User-level development environments
3. **Profile-based Configuration**: Different user profiles for different contexts
4. **Improved Package Management**: User-level package installation and management

## Migration Strategy

### Phase 1: Foundation Setup
**Timeline**: Week 1-2
**Risk Level**: Low

#### Tasks:
1. **Initialize Home Manager Integration**
   - Add Home Manager to flake.nix inputs
   - Configure basic Home Manager module in NixOS
   - Create initial home.nix for user 'geir'

2. **Create Home Manager Directory Structure**
   ```
   home-manager/
   ├── users/
   │   ├── geir/
   │   │   ├── home.nix              # Main user configuration
   │   │   ├── profiles/
   │   │   │   ├── development.nix   # Development profile
   │   │   │   ├── workstation.nix   # Full workstation profile
   │   │   │   └── minimal.nix       # Minimal profile
   │   │   ├── programs/
   │   │   │   ├── emacs.nix         # Emacs configuration
   │   │   │   ├── zsh.nix           # Shell configuration
   │   │   │   ├── git.nix           # Git configuration
   │   │   │   └── development.nix   # Development tools
   │   │   └── services/
   │   │       ├── emacs-daemon.nix  # User-level Emacs daemon
   │   │       └── desktop.nix       # Desktop services
   │   └── sma/
   │       └── home.nix              # Admin user configuration
   ├── modules/
   │   ├── emacs/                    # Shared Emacs modules
   │   ├── development/              # Development environment modules
   │   └── desktop/                  # Desktop environment modules
   └── dotfiles/
       ├── emacs/                    # Emacs configuration files
       ├── shell/                    # Shell configuration files
       └── desktop/                  # Desktop configuration files
   ```

3. **Basic User Configuration Migration**
   - Migrate user packages from system to Home Manager
   - Set up basic shell configuration
   - Implement minimal dotfile management

#### Deliverables:
- Working Home Manager integration
- Basic user configuration in Home Manager
- Preserved system functionality

### Phase 2: Emacs Migration
**Timeline**: Week 3-4
**Risk Level**: Medium

#### Tasks:
1. **Migrate Emacs Configuration to Home Manager**
   - Preserve current profile-based system
   - Migrate from `/etc/emacs/` to Home Manager file management
   - Maintain Nix tool integration
   - Convert systemd service to user service

2. **Enhanced Emacs Module Structure**
   ```nix
   programs.emacs = {
     enable = true;
     profile = "development"; # or "workstation", "minimal"
     package = emacsWithProfile;
     
     profiles = {
       minimal = {
         packages = [...];
         extraConfig = "...";
       };
       development = {
         packages = [...];
         extraConfig = "...";
       };
       workstation = {
         packages = [...];
         extraConfig = "...";
       };
     };
     
     nixIntegration = {
       enableLspServers = true;
       enableFormatters = true;
       tools = {
         ripgrep = true;
         fd = true;
         sqlite = true;
       };
     };
   };
   ```

3. **Dotfile Management for Emacs**
   - Use `xdg.configFile` for Emacs configuration deployment
   - Maintain modular structure
   - Preserve profile-based loading

#### Deliverables:
- Emacs fully managed by Home Manager
- User-level Emacs daemon service
- Preserved profile-based functionality
- Improved dotfile deployment

### Phase 3: Complete User Migration
**Timeline**: Week 5-6
**Risk Level**: Medium

#### Tasks:
1. **Shell Configuration Migration**
   - Move zsh configuration to Home Manager
   - Implement user-specific aliases and functions
   - Preserve current shell enhancement integration

2. **Development Environment Setup**
   - User-level development tools
   - Language server configurations
   - Project-specific environments

3. **Desktop Environment Integration**
   - User-level desktop services
   - Application-specific configurations
   - Theme and appearance management

#### Deliverables:
- Complete user environment in Home Manager
- System packages reduced to essential only
- User-specific development environments

### Phase 4: Multi-user and Profiles
**Timeline**: Week 7-8
**Risk Level**: Low

#### Tasks:
1. **Multi-user Support**
   - Migrate 'sma' user configuration
   - Shared modules for common functionality
   - User-specific customizations

2. **Profile System Enhancement**
   - Machine-specific user profiles
   - Context-aware configurations
   - Easy profile switching

3. **Documentation and Testing**
   - Comprehensive documentation
   - Migration testing on different machines
   - Rollback procedures

#### Deliverables:
- Complete multi-user Home Manager setup
- Profile-based user configurations
- Comprehensive documentation

## Technical Implementation Details

### Home Manager Integration Pattern

```nix
# flake.nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }: {
    nixosConfigurations = {
      little-rascal = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/little-rascal/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.geir = import ./home-manager/users/geir/home.nix;
            home-manager.users.sma = import ./home-manager/users/sma/home.nix;
          }
        ];
      };
    };
  };
}
```

### Emacs Configuration Structure

```nix
# home-manager/users/geir/programs/emacs.nix
{ config, lib, pkgs, ... }:

let
  emacsProfile = config.programs.emacs.profile or "development";
  
  # Package sets remain the same as current implementation
  packageSets = {
    essential = epkgs: with epkgs; [ use-package diminish bind-key which-key ];
    minimal = epkgs: with epkgs; [ /* minimal packages */ ];
    development = epkgs: with epkgs; [ /* development packages */ ];
    workstation = epkgs: with epkgs; [ /* full packages */ ];
  };

  emacsWithProfile = profile: 
    (if profile == "nox" then pkgs.emacs-nox else pkgs.emacs-pgtk).pkgs.withPackages
      (epkgs: packageSets.essential epkgs ++ 
              (if profile == "nox" 
               then packageSets.minimal epkgs
               else packageSets.development epkgs ++ packageSets.workstation epkgs));

in {
  options.programs.emacs.profile = lib.mkOption {
    type = lib.types.enum [ "gui" "nox" ];
    default = "gui";
  };

  config = {
    programs.emacs = {
      enable = true;
      package = emacsWithProfile emacsProfile;
    };

    services.emacs = {
      enable = true;
      package = config.programs.emacs.package;
    };

    xdg.configFile = {
      "emacs/init.el".source = ../../../dotfiles/emacs/init-nix.el;
      "emacs/modules/ui.el".source = ../../../dotfiles/emacs/modules/ui.el;
      "emacs/modules/completion.el".source = ../../../dotfiles/emacs/modules/completion.el;
      # ... other modules
    };

    home.sessionVariables = {
      EMACS_PROFILE = emacsProfile;
      RG_PATH = "${pkgs.ripgrep}/bin/rg";
      FD_PATH = "${pkgs.fd}/bin/fd";
      # ... other tool paths
    };
  };
}
```

### Dotfile Management Strategy

```nix
# home-manager/users/geir/home.nix
{
  # XDG-compliant configuration files
  xdg.configFile = {
    "niri/config.kdl".source = ../../dotfiles/niri/config.kdl;
    "git/config".source = ../../dotfiles/git/config;
  };

  # Home directory files
  home.file = {
    ".zshrc".source = ../../dotfiles/shell/zshrc;
    ".tmux.conf".source = ../../dotfiles/tmux/tmux.conf;
  };

  # Direct content management
  xdg.configFile."starship.toml".text = ''
    # Starship configuration
    format = "$all$character"
  '';
}
```

## Risk Assessment and Mitigation

### High-Risk Areas
1. **Emacs Configuration Migration**
   - **Risk**: Breaking existing sophisticated Emacs setup
   - **Mitigation**: 
     - Preserve exact current functionality first
     - Test on non-critical machine first
     - Maintain parallel system during transition

2. **User Service Management**
   - **Risk**: Service startup/shutdown issues
   - **Mitigation**:
     - Test user services thoroughly
     - Provide fallback to system services
     - Document service debugging procedures

### Medium-Risk Areas
1. **Package Dependency Changes**
   - **Risk**: Missing packages or version conflicts
   - **Mitigation**:
     - Careful package migration tracking
     - Test on all machine types
     - Maintain system packages temporarily

2. **Dotfile Deployment**
   - **Risk**: Configuration file conflicts or missing files
   - **Mitigation**:
     - Backup existing configurations
     - Gradual migration approach
     - Collision detection and resolution

### Low-Risk Areas
1. **Shell Configuration**
   - **Risk**: Minor shell behavior changes
   - **Mitigation**: Preserve exact current shell configuration

2. **Git Configuration**
   - **Risk**: Git workflow disruption
   - **Mitigation**: Migrate settings exactly as-is

## Testing Strategy

### Phase Testing
1. **Per-phase Testing**: Each phase tested independently
2. **Machine-specific Testing**: Test on each machine type
3. **User-specific Testing**: Test for both geir and sma users
4. **Rollback Testing**: Verify rollback procedures work

### Test Environments
1. **Development VM**: Safe testing environment
2. **Non-critical Machine**: Real hardware testing
3. **Critical Machines**: Final validation

### Test Cases
1. **Emacs Functionality**: All profiles, modules, and integrations
2. **Development Workflow**: Complete development environment
3. **Service Management**: User services start/stop correctly
4. **Multi-machine**: Configuration works across all machines
5. **Package Management**: All user packages available

## Migration Timeline

| Week | Phase | Focus | Risk Level |
|------|-------|-------|------------|
| 1-2  | Foundation | Home Manager setup, basic migration | Low |
| 3-4  | Emacs | Emacs configuration migration | Medium |
| 5-6  | Complete User | Full user environment migration | Medium |
| 7-8  | Polish | Multi-user, profiles, documentation | Low |

## Success Criteria

### Functional Requirements
- [ ] All current functionality preserved
- [ ] Emacs profiles work exactly as before
- [ ] User services managed by Home Manager
- [ ] Dotfiles systematically deployed
- [ ] Development environment fully functional

### Quality Requirements
- [ ] Configuration is more modular and maintainable
- [ ] Clear separation between user and system configuration
- [ ] Comprehensive documentation
- [ ] Easy rollback capability
- [ ] Multi-user support functional

### Performance Requirements
- [ ] No degradation in system startup time
- [ ] Emacs startup time maintained or improved
- [ ] Home Manager switch time acceptable (<30 seconds)

## Rollback Plan

### Emergency Rollback
1. **Git Revert**: Revert to pre-migration commit
2. **System Rebuild**: `sudo nixos-rebuild switch --flake .`
3. **Service Restart**: Restart affected services

### Partial Rollback
1. **Disable Home Manager**: Comment out Home Manager in machine configuration
2. **Restore System Packages**: Move packages back to system level
3. **Manual Service Management**: Manually manage services during transition

### Validation Steps
1. **Emacs Functionality**: Verify Emacs starts and works correctly
2. **Development Environment**: Verify development tools work
3. **User Services**: Verify essential services are running
4. **System Stability**: Verify system is stable and responsive

## Documentation Deliverables

### User Documentation
1. **Migration Guide**: Step-by-step migration instructions
2. **Home Manager Usage**: How to use Home Manager for daily workflow
3. **Profile Management**: How to switch and manage user profiles
4. **Troubleshooting Guide**: Common issues and solutions

### Developer Documentation
1. **Architecture Documentation**: New system architecture
2. **Module Documentation**: How modules are structured and work
3. **Extension Guide**: How to add new functionality
4. **Testing Procedures**: How to test changes

## Conclusion

This refactoring plan provides a comprehensive roadmap for migrating from the current system-centric user configuration to a modern Home Manager-based approach. The phased approach minimizes risk while ensuring all current functionality is preserved and enhanced. The end result will be a more maintainable, flexible, and user-centric configuration management system that better separates concerns between system and user configuration.

The plan prioritizes preserving the excellent current Emacs configuration while adding the benefits of proper user-level service management, systematic dotfile deployment, and improved modularity. The migration can be executed with confidence due to the comprehensive testing strategy and rollback procedures.