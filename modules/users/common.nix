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
    
    # direnv integration
    interactiveShellInit = ''
      eval "$(direnv hook zsh)"
    '';
    
    # Common aliases for all users
    shellAliases = {
      # Modern CLI tool replacements (basic ones moved to base.nix)
      "ll" = "eza -l --color=auto --group-directories-first";
      "la" = "eza -la --color=auto --group-directories-first";
      "tree" = "eza --tree";
      
      # Git shortcuts (basic ones moved to base.nix)
      
      # System shortcuts (some moved to base.nix)
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
    # Essential CLI tools moved to base.nix
    # Adding user-specific tools here
    
    # Communication
    firefox
    
    # Development (basic tools moved to base.nix)
    # Additional utilities not in base.nix
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
