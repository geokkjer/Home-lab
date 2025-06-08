{ config, pkgs, ... }:
{
  # Base system packages and aliases shared across all machines
  # This module consolidates common CLI tools to reduce duplication
  # across user configurations and machine-specific configs
  
  environment.systemPackages = with pkgs; [
    # Modern CLI tools (rust-based replacements) 
    tldr          # Better man pages
    eza           # Better ls
    bat           # Better cat  
    ripgrep       # Better grep
    du-dust       # Better du
    bottom        # Better top
    fd            # Better find
    fzf           # Fuzzy finder
    zoxide        # Better cd
    uutils-coreutils-noprefix  # Modern coreutils
    
    # Environment management
    direnv        # Directory-based environment management
    nix-direnv    # Nix integration for direnv
    
    # Essential system tools
    curl          # HTTP client
    wget          # Download utility
    git           # Version control
    htop          # Process viewer
    tree          # Directory tree viewer
    file          # File type detection
    unzip         # Archive extraction
    zip           # Archive creation
    fastfetch
    zellij
    glances
    systemctl-tui

    # Text processing and utilities
    jq            # JSON processor
    yq            # YAML processor
    
    # Network utilities
    nmap          # Network mapper
    
    # System monitoring and diagnostics
    lsof          # List open files
    strace        # System call tracer
    ncdu          # Disk usage analyzer
    
    # Development basics
    github-cli    # GitHub CLI
  ];
  environment.shellAliases = {
    vi = "nvim";
    vim = "nvim";
    h = "tldr";
    # Modern CLI tool replacements
    ls = "eza -l";
    cat = "bat";
    grep = "rg";
    top = "btm";
    du = "dust";
    find = "fd";
    # Common git shortcuts (basic ones)
    gs = "git status";
    ga = "git add";
    gc = "git commit";
    gp = "git push";
    gpa = "git remote | xargs -L1 git push";
    gl = "git log --oneline -10";
  };
}