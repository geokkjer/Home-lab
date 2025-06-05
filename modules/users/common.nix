# Common User Configuration
# Shared settings for all users in the home lab
{ config, pkgs, ... }:

{
  # Common user settings
  users = {
    # Use mutable users for flexibility
    mutableUsers = true;
    
    # Default shell for all users
    defaultUserShell = pkgs.zsh;
  };

  # Enable zsh system-wide
  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    
    # Common aliases for all users
    shellAliases = {
      # Modern CLI tool replacements
      "ls" = "eza --color=auto --group-directories-first";
      "ll" = "eza -l --color=auto --group-directories-first";
      "la" = "eza -la --color=auto --group-directories-first";
      "tree" = "eza --tree";
      
      # Git shortcuts
      "gs" = "git status";
      "ga" = "git add";
      "gc" = "git commit";
      "gp" = "git push";
      "gl" = "git log --oneline -10";
      
      # System shortcuts
      "grep" = "rg";
      "find" = "fd";
      "cat" = "bat";
      "top" = "btop";
      
      # Network
      "ping" = "ping -c 5";
      "myip" = "curl -s ifconfig.me";
      
      # Safety
      "rm" = "rm -i";
      "mv" = "mv -i";
      "cp" = "cp -i";
    };
    
    # Common environment variables
    sessionVariables = {
      EDITOR = "emacs";
      BROWSER = "firefox";
      TERMINAL = "kitty";
    };
  };

  # Common packages for all users
  environment.systemPackages = with pkgs; [
    # Essential CLI tools (already configured in base.nix)
    # Adding user-specific tools here
    
    # Communication
    firefox
    
    # Development (basic)
    git
    curl
    wget
    
    # Media
    celluloid
    
    # Utilities
    file
    unzip
    zip
  ];

  # Common security settings
  security = {
    # Require password for sudo (can be overridden per user)
    sudo.wheelNeedsPassword = true;
    
    # Polkit for desktop users
    polkit.enable = true;
  };

  # Common services
  services = {
    # Enable SSH for remote management
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false; # Key-based auth only
        PermitRootLogin = "no";         # No root login
        X11Forwarding = true;           # For GUI applications over SSH
      };
    };
    
    
    # Enable sound
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };
  };

  # XDG portal for desktop integration
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-gnome
    ];
  };
}
