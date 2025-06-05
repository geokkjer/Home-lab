# AI Agent General Instructions

## Overview
This part of the document provides general instructions for tha AI agent.

## General instructions
This document is to be treated as an iterative work and a collaberation between the user and AI agent.

## Goal for this file
Top part reusable instructions that can be transferred to other project and as away to iterativey make ai agent behave more like I prefer.
Bottom part should have information specific to the project.
The Plan.md file should have the project information and steps.


## Programming Languages and styles
- Prefer functional style
- Guille scheme for 
- Python for ai and when guile 
- Bash only for short scripts
- Typescript and javascript for web 
- Rust for binary tools etc

## Written language and style
use notes.md to take notes . 
Use a casual but knowledgeable tone. This is not a corporate project there are no audits or compliance to adhere to.
More like an open source project, more like a hobby/passion project

# AI Agent Instructions: NixOS Flakes Migration for CongenitalOptimist
## Overview
This part of the document provides step-by-step instructions for AI agents to help migrate the CongenitalOptimist machine from traditional NixOS configuration to flakes-based configuration and upgrade to NixOS 25.05. The system already has excellent modular structure that we'll preserve and enhance.
Itreative about the project, update often
## Current System Information
- **Hostname**: work → congenital-optimist (migration in progress)
- **Current Version**: NixOS 25.05 (migrated from 23.11)
- **Target Version**: NixOS 25.05 ✅ 
- **Architecture**: x86_64-linux
- **Storage**: ZFS (zpool for system, stuffpool for data)
- **Hardware**: AMD CPU/GPU
- **Users**: geir (primary), sma (admin)
- **Dotfiles Approach**: Literate programming with Emacs org-mode (no Home Manager)

## Current Module Structure
```
Home-lab/
├── machines/
│   ├── congenital-optimist/
│   │   ├── configuration.nix (main system config)
│   │   ├── hardware-configuration.nix
│   │   └── About.org
│   ├── sleeper-service/
│   ├── reverse-proxy/
│   └── grey-area/
├── modules/
│   ├── common/
│   │   ├── base.nix (modern CLI tools & aliases)
│   │   ├── tty.nix (console styling with Joker theme)
│   │   └── nix.nix (flakes configuration)
│   ├── desktop/
│   │   ├── common.nix, gnome.nix, cosmic.nix, sway.nix
│   ├── development/
│   │   └── tools.nix (editors, LSPs, languages)
│   ├── hardware/
│   │   └── amd-workstation.nix
│   ├── system/
│   │   ├── applications.nix, fonts.nix, network.nix
│   ├── users/
│   │   ├── common.nix, geir.nix, sma.nix
│   └── virtualization/
│       ├── podman.nix, libvirt.nix, incus.nix
└── users/
    └── geir/
        └── dotfiles/ (literate org-mode configs)
```

## Prerequisites Check
Before starting, verify:
1. Current system is bootable and stable
2. ZFS pools are healthy (`zpool status`)
3. All referenced modules exist and are working
4. User has sudo/root access
5. Git repository is initialized and up to date

## Step 1: Fix Existing Configuration Issues

### Fix typo in user.nix
Edit `Home-lab/Users/geir/user.nix` and change:
```nix
progtams.zsh.enableCompletion = true;
```
to:
```nix
programs.zsh.enableCompletion = true;
```

## Step 2: Create Root Flake Structure

Create `Home-lab/flake.nix`:

```nix
{
  description = "CongenitalOptimist Home Lab NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, ... }@inputs: {
    nixosConfigurations = {
      congenital-optimist = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { 
          inherit inputs;
          unstable = import nixpkgs-unstable { 
            system = "x86_64-linux"; 
            config.allowUnfree = true; 
          };
        };
        modules = [
          ./machines/congenital-optimist
        ];
      };
    };
  };
}
```

## Step 3: Create Target Directory Structure

Execute these commands in the Home-lab directory:
```bash
mkdir -p machines/congenital-optimist
mkdir -p modules/{common,desktop,development,virtualization,users}
mkdir -p users/geir/dotfiles/{emacs,shell,editors}
mkdir -p overlays
mkdir -p packages
```

## Step 4: Convert Main Configuration

Create `machines/CongenitalOptimist/default.nix` by adapting the current `configuration.nix`:

Key changes needed:
1. Change function signature from `{pkgs, ...}:` to `{ config, pkgs, inputs, unstable, ... }:`
2. **Keep `system.stateVersion` as "23.11"** (maintains compatibility with existing data)
3. Fix nerd-fonts syntax (changed in 25.05)
4. Remove the experimental-features setting (handled by flake)
5. Update module import paths for new structure
6. Consolidate user package management

### Nerd Fonts Fix for 25.05

Replace this section in fonts.packages:
```nix
# Old format (will break in 25.05)
nerd-fonts.meslo-lg
nerd-fonts.jetbrains-mono
nerd-fonts.fira-code
nerd-fonts.droid-sans-mono
nerd-fonts.zed-mono
nerd-fonts.iosevka
nerd-fonts.iosevka-term
nerd-fonts.hack
```

With:
```nix
# New format for 25.05
(nerdfonts.override { 
  fonts = [ 
    "Meslo" 
    "JetBrainsMono" 
    "FiraCode" 
    "DroidSansMono" 
    "Hack" 
    "Iosevka" 
    "IosevkaTerm" 
  ]; 
})
```

## Step 5: Migrate Existing Modules

### Copy existing modules to new structure:
```bash
# Copy common modules
cp Home-lab/Machines/Modules/common/base.nix modules/common/
cp Home-lab/Machines/Modules/common/tty.nix modules/common/

# Copy virtualization modules
cp Home-lab/Machines/Modules/virtualization/*.nix modules/virtualization/
```

### Create additional common modules:

#### `modules/common/nix.nix`:
```nix
{ config, pkgs, ... }: {
  # Enable flakes system-wide
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  
  # Optimize nix settings
  nix.settings = {
    auto-optimise-store = true;
    substituters = [
      "https://cache.nixos.org/"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
  };
}
```

## Step 6: Consolidate User Configuration

Create `modules/users/geir.nix` by merging the existing user config:

```nix
{ config, pkgs, ... }: {
  users.users.geir = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "libvirtd" "podman" "incus-admin" ];
    shell = pkgs.zsh;
    packages = with pkgs; [
      # Browsers
      chromium
      vivaldi
      vivaldi-ffmpeg-codecs
      nyxt
      firefox

      # Shell & development tools
      starship
      fastfetch
      hyfetch
      nerdfetch
      nix-direnv
      gh
      github-copilot-cli

      # Media & graphics
      gimp
      obs-studio
      vesktop
      koodo-reader

      # System tools
      ncpamixer
      virt-manager
      pavucontrol
      gnome-tweaks
      beauty-line-icon-theme
      
      # Terminal multiplexer and fun
      zellij
      neo-cowsay
      fortune
      clolcat
      
      # Emacs integration
      emacsPackages.vterm
    ];
  };

  # System-wide ZSH configuration
  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;
    autosuggestions = {
      enable = true;
      historySearch = true;
    };
    history = {
      enable = true;
      shareHistory = true;
      saveOnExit = true;
    };
  };

  environment.systemPackages = with pkgs; [
    zsh
    zsh-completions
    nix-zsh-completions
    zsh-autocomplete
    zsh-autosuggestions
    zsh-syntax-highlighting
  ];
}
```

## Step 7: Create Desktop Environment Modules

### `modules/desktop/gnome.nix`:
```nix
{ config, pkgs, ... }: {
  services.xserver = {
    enable = true;
    desktopManager.gnome.enable = true;
    xkb.layout = "no";
  };
  
  # XDG portal configuration
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };
}
```

### `modules/desktop/cosmic.nix`:
```nix
{ config, pkgs, ... }: {
  services.desktopManager.cosmic.enable = true;
  services.displayManager.cosmic-greeter.enable = true;
  services.desktopManager.cosmic.xwayland.enable = true;
}
```

### `modules/desktop/sway.nix`:
```nix
{ config, pkgs, ... }: {
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };
  
  services.dbus.enable = true;
  
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  environment.systemPackages = with pkgs; [
    swaylock
    swayidle
    swaybg
    waybar
    fuzzel
    gammastep
    mako
    flameshot
  ];
}
```

## Step 8: Set Up Per-User Literate Dotfiles

Create `users/geir/dotfiles/README.org`:
```org
#+TITLE: CongenitalOptimist Dotfiles Configuration
#+DESCRIPTION: Literate programming approach to dotfiles using Emacs org-mode
#+PROPERTY: header-args :tangle yes
#+STARTUP: overview

* Introduction
This file contains all dotfiles configuration using literate programming.
Each configuration block can be tangled to its respective file using C-c C-v t.

The approach allows for:
- Self-documenting configuration with rationale
- Easy sharing and explanation of setup decisions
- Version control of configuration with full context
- Modular organization of different tool configurations

* Shell Configuration
** Zsh Configuration
#+BEGIN_SRC sh :tangle ~/.zshrc
# Generated from dotfiles/README.org - CongenitalOptimist configuration
export EDITOR="emacs"
export BROWSER="firefox"
export SHELL="/run/current-system/sw/bin/zsh"

# Enable starship prompt (configured in NixOS)
eval "$(starship init zsh)"

# Enable zoxide for smart cd
eval "$(zoxide init zsh)"

# Custom aliases (complementing those in base.nix)
alias gc='git commit'
alias gp='git push'
alias gs='git status'
alias nrs='sudo nixos-rebuild switch --flake .'
alias nrt='sudo nixos-rebuild test --flake .'
alias reload='source ~/.zshrc'

# CongenitalOptimist specific shortcuts
alias lab='cd ~/Home-lab'
alias tangle='cd ~/Home-lab/users/geir/dotfiles && emacs --batch -l org --eval "(org-babel-tangle-file \"README.org\")"'
alias dotfiles='cd ~/Home-lab/users/geir/dotfiles'
#+END_SRC

** Starship Configuration  
#+BEGIN_SRC toml :tangle ~/.config/starship.toml
# Starship prompt configuration for CongenitalOptimist
format = """
$username\
$hostname\
$directory\
$git_branch\
$git_state\
$git_status\
$cmd_duration\
$line_break\
$character"""

[character]
success_symbol = "[➜](bold green)"
error_symbol = "[➜](bold red)"

[directory]
truncation_length = 3
fish_style_pwd_dir_length = 1

[git_branch]
symbol = "🌱 "

[hostname]
ssh_only = false
format = "[@$hostname](bold blue) "

[username]
show_always = true
format = "[$user](bold yellow)"
#+END_SRC

* Editor Configuration
** Emacs Configuration
#+BEGIN_SRC emacs-lisp :tangle ~/.emacs.d/init.el
;; Generated from dotfiles/README.org - CongenitalOptimist
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; Enable line numbers
(global-display-line-numbers-mode 1)

;; Enable org-babel for literate programming
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   (nix . t)))

;; Auto-tangle on save for literate config files
(defun auto-tangle-config-files ()
  "Auto tangle config files when saving."
  (when (string-match-p "users/.*/dotfiles.*\\.org$" (buffer-file-name))
    (org-babel-tangle)))

(add-hook 'after-save-hook 'auto-tangle-config-files)

;; Better defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(show-paren-mode 1)
(electric-pair-mode 1)

;; CongenitalOptimist theme setup
(when (display-graphic-p)
  (load-theme 'deeper-blue t))
#+END_SRC

* Development Tools
** Git Configuration
#+BEGIN_SRC conf :tangle ~/.gitconfig
[user]
    name = geir
    email = geir@congenital-optimist.local

[init]
    defaultBranch = main

[core]
    editor = emacs
    pager = bat

[pull]
    rebase = false

[alias]
    st = status
    co = checkout
    br = branch
    unstage = reset HEAD --
    last = log -1 HEAD
    visual = !gitk
#+END_SRC

* Home Lab Specific Configuration
** NixOS Rebuild Aliases
These aliases make working with the flake-based configuration easier:

#+BEGIN_SRC sh :tangle ~/.config/shell/nixos-aliases
# NixOS CongenitalOptimist specific aliases
alias nix-build='cd ~/Home-lab && nix build .#nixosConfigurations.congenital-optimist.config.system.build.toplevel'
alias nix-check='cd ~/Home-lab && nix flake check'
alias nix-update='cd ~/Home-lab && nix flake update'
alias nix-clean='sudo nix-collect-garbage -d'
alias edit-dotfiles='cd ~/Home-lab/users/geir/dotfiles && emacs README.org'
#+END_SRC
```

## Step 9: Update Main Configuration Import Structure

Create the main machine configuration in `machines/CongenitalOptimist/default.nix`:

```nix
{ config, pkgs, inputs, unstable, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../modules/common/base.nix
    ../../modules/common/tty.nix
    ../../modules/common/nix.nix
    ../../modules/virtualization/podman.nix
    ../../modules/virtualization/libvirt.nix
    ../../modules/virtualization/incus.nix
    ../../modules/desktop/gnome.nix
    ../../modules/desktop/cosmic.nix
    ../../modules/desktop/sway.nix
    ../../modules/users/geir.nix
  ];

  # Boot configuration
  boot.loader.grub = {
    enable = true;
    zfsSupport = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
    mirroredBoots = [
      {
        devices = ["nodev"];
        path = "/boot";
      }
    ];
  };
  
  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };

  # Hardware
  services.fwupd.enable = true;
  hardware.enableRedistributableFirmware = true;
  hardware.amdgpu.initrd.enable = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  # System settings
  nixpkgs.config.allowUnfree = true;
  networking.nftables.enable = true;
  networking.hostName = "work"; # TODO: consider changing to "congenital-optimist"
  services.tailscale.enable = true;
  networking.networkmanager.enable = true;
  networking.hostId = "8425e349";
  time.timeZone = "Europe/Oslo";
  i18n.defaultLocale = "en_US.UTF-8";

  # Services
  services.flatpak.enable = true;
  services.emacs.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  programs.steam.enable = true;

  # Fonts (updated for 25.05)
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    liberation_ttf
    dina-font
    proggyfonts
    mona-sans
    hubot-sans
    inter-nerdfont
    (nerdfonts.override { 
      fonts = [ 
        "Meslo" 
        "JetBrainsMono" 
        "FiraCode" 
        "DroidSansMono" 
        "Hack" 
        "Iosevka" 
        "IosevkaTerm" 
      ]; 
    })
  ];

  # System packages
  environment.systemPackages = with pkgs; [
    # Terminal applications
    kitty
    terminator
    rio
    dbus
    greetd.tuigreet
    wayland
    xdg-utils

    # System monitoring
    glances
    inxi
    htop
    bottom
    wget
    curl
    git
    mc
    systemctl-tui

    # Development tools
    guile
    rustup
    nixd
    zls
    alejandra
    python3Packages.python-lsp-server
    gopls
    luajitPackages.lua-lsp
    nodePackages.bash-language-server
    vimPlugins.cmp-nvim-lsp
    ccls
    gdb
    marksman

    # Editors
    zed-editor
    neovim
    emacs
    vscode
    vscodium-fhs
  ];

  # Network and security
  services.openssh.enable = true;
  services.zfs.autoScrub.enable = true;
  services.zfs.trim.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 ];
  networking.firewall.allowedUDPPorts = [ 22 ];
  networking.firewall.enable = true;

  system.copySystemConfiguration = true;
  system.stateVersion = "23.11"; # DO NOT CHANGE - maintains data compatibility
}
```

## Step 10: Copy Hardware Configuration

Copy the existing hardware configuration:
```bash
cp Home-lab/Machines/CongenitalOptimist/hardware-configuration.nix machines/CongenitalOptimist/
```

## Step 11: Test Configuration

Before applying changes:

1. Test flake evaluation:
```bash
cd Home-lab
nix flake check
```

2. Build configuration without switching:
```bash
sudo nixos-rebuild build --flake .#congenital-optimist
```

3. If successful, test the configuration:
```bash
sudo nixos-rebuild test --flake .#congenital-optimist
```

4. If everything works, switch permanently:
```bash
sudo nixos-rebuild switch --flake .#congenital-optimist
```

## Step 12: Set Up Per-User Literate Dotfiles Workflow

1. Create your main org-mode configuration file:
```bash
cd users/geir/dotfiles
emacs README.org
```

2. Use org-babel to tangle your configurations:
```bash
# In Emacs, use C-c C-v t to tangle all code blocks
# Or from command line:
cd users/geir/dotfiles
emacs --batch -l org --eval "(org-babel-tangle-file \"README.org\")"
```

3. The provided Emacs config includes auto-tangle on save for any user's dotfiles
4. Test that dotfiles are generated correctly in the user's home directory
5. For additional users, create similar structure under `users/<username>/dotfiles/`

## Step 13: Lock and Commit

1. Generate flake.lock:
```bash
nix flake lock
```

2. Commit changes:
```bash
git add .
git commit -m "Migrate CongenitalOptimist to flakes with literate dotfiles"
```

## Verification Steps

After switching:
1. Verify system boots correctly
2. Check all services are running: `systemctl --failed`
3. Test all desktop environments launch (GNOME, Cosmic, Sway)
4. Verify virtualization: `sudo systemctl status libvirtd podman`
5. Check ZFS status: `zpool status`
6. Test network connectivity and Tailscale
7. Verify user environment and all packages available
8. Test modern CLI tools and aliases from base.nix
9. Check console theming and TTY configuration
10. Verify Emacs and literate programming workflow

## Error Resolution

### Common Issues:

1. **Boot failure**: Boot from previous generation in GRUB
2. **Package not found**: Check if package name changed in 25.05
3. **Service fails**: Check journalctl: `journalctl -u <service-name>`
4. **Desktop environment issues**: Switch to TTY (Ctrl+Alt+F2) and debug
5. **Nerd fonts issues**: Verify the new syntax is applied correctly

### Emergency Recovery:
- Boot from previous NixOS generation in GRUB
- Use ZFS snapshots if available: `zfs rollback zpool/root@snapshot-name`
- Keep live USB available for emergency repairs

## Final Validation Checklist

- [ ] System boots to desktop
- [ ] All user applications launch (browsers, editors, terminals)
- [ ] Network and Tailscale functional
- [ ] Virtualization stack operational (podman, libvirt, incus)
- [ ] ZFS and NFS mounts working
- [ ] Development tools functional (editors, LSPs, languages)
- [ ] Audio system working (pipewire)
- [ ] Modern CLI tools and aliases from base.nix working
- [ ] Console theming with Joker palette preserved
- [ ] Bluetooth functional if needed
- [ ] Literate dotfiles workflow established
- [ ] Auto-tangling in Emacs working

## Post-Migration Tasks

1. Consider updating hostname from "work" to "congenital-optimist"
2. Expand literate dotfiles in org-mode with more tools and configurations
3. Create additional machine configurations in the flake
4. Implement secrets management (agenix/sops-nix)
5. Set up automatic updates
6. Add system monitoring
8. Document your literate programming workflow per user
9. Consider using org-mode for documenting the entire home lab setup
10. Set up org-export to generate beautiful documentation from your configs
11. Create templates for common user configuration patterns
12. Plan for additional users with their own dotfiles directories
13. Consider shared vs user-specific configuration strategies

## Notes for AI Agents

1. Always preserve existing functionality - don't remove working features
2. The system already has excellent modular structure - build on it
3. Modern CLI tools are already configured in base.nix - don't duplicate
4. Console theming is already set up - preserve the Joker palette
5. Fix the typo in user.nix before proceeding
6. Keep system.stateVersion as "23.11" - never change this
7. Test thoroughly before committing to permanent changes
8. Prioritize system stability over new features
9. The literate programming approach should complement, not replace, the modular NixOS structure