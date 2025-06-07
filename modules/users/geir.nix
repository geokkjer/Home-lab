# Primary User Configuration - geir
# Main user account for development and desktop use
{ config, pkgs, ... }:
let
  # Import custom packages from the flake
  homeLabPackages = import ../../packages { inherit pkgs; };
in
{
  users.users.geir = {
    description = "Geir Okkenhaug Jerstad - Primary User";
    isNormalUser = true;
    
    # User groups for development and desktop use
    extraGroups = [ 
      "wheel"          # sudo access
      "networkmanager" # network management
      "libvirt"        # virtualization
      "incus-admin"    # container management
      "podman"         # container runtime
      "audio"          # audio devices
      "video"          # video devices
      "render"         # GPU access
    ];
    
    shell = pkgs.zsh;
    
    # SSH access with development keys
    openssh.authorizedKeys.keys = [
      # Current key (keep for continuity during transition)
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHeOvTHIw+hZOAiWkIrz9t11UeGwxAMx7jN/1IIdgq7O geokkjer@gmail.com"
      # New development key
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHukJK0Kc1YexvzF8PdqaqWNZdVffGoM6ePPMecrU6dM geir@geokkjer.eu-dev"
    ];
    
    # User-specific packages
    packages = with pkgs; [
      # Home lab management tools
      homeLabPackages.lab

      # Terminal applications & system monitoring
      kitty
      terminator
      starship
      fastfetch
      hyfetch
      zellij
      glances
      htop
      bottom
      systemctl-tui

      # Essential system tools
      wget
      curl
      mc
      
      # Browsers & Communication
      firefox
      chromium
      vesktop
      
      # Shell Enhancement & Fun
      nerdfetch
      neo-cowsay
      fortune
      clolcat
      
      # Audio & System Control
      ncpamixer
      pavucontrol
      
      # Productivity
      libreoffice
      koodo-reader
      
      # Development & System Management
      neovim
      vscode
      git-credential-manager
      github-cli
      nodejs
      nodePackages.npm
      virt-manager
      
      # Creative Tools (optional - remove if not needed)
      gimp
      obs-studio
      
      # File Management
      nautilus
      file-roller
      
      # Containers
      podman-compose
      podman-desktop
      
      # Media
      celluloid
      
      # Emacs Integration
      emacsPackages.vterm
      
      # Desktop integration (moved from system)
      dbus
      wayland
      xdg-utils
    ];
  };

  # User-specific services and configurations
  
  # Enable automatic login for primary user (optional, can be disabled for security)
  # services.xserver.displayManager.autoLogin = {
  #   enable = true;
  #   user = "geir";
  # };

  # User-specific environment variables
  environment.sessionVariables = {
    # Development preferences
    EDITOR = "emacs";
    BROWSER = "firefox";
    TERMINAL = "kitty";
    
    # Git configuration
    GIT_EDITOR = "emacs";
  };

  # Comprehensive zsh configuration for geir
  programs.zsh = {
    enable = true;
    
    # Shell aliases
    shellAliases = {
      # Development workflow
      "home-lab" = "z /home/geir/Home-lab";
      "configs" = "z /home/geir/Home-lab/user_configs/geir";
      "emacs-config" = "emacs /home/geir/Home-lab/user_configs/geir/emacs.org";
      
      # Quick system management
      "rebuild-test" = "sudo nixos-rebuild test --flake /home/geir/Home-lab";
      "rebuild" = "sudo nixos-rebuild switch --flake /home/geir/Home-lab";
      "collect" = "sudo nix-collect-garbage --d";
      "optimise" = "sudo nix-store --optimise";

      # Git shortcuts for multi-remote workflow
      "git-push-all" = "git push origin main && git push github main";
      "git-status-all" = "git status && echo '--- Checking origin ---' && git log origin/main..HEAD --oneline && echo '--- Checking github ---' && git log github/main..HEAD --oneline";

      # Container shortcuts
      "pdm" = "podman";
      "pdc" = "podman-compose";
      
      # Media shortcuts
      "youtube-dl" = "yt-dlp";
    };
    
    # History configuration
    histSize = 10000;
    histFile = "$HOME/.histfile";
    
    # Shell options
    setOptions = [ "autocd" "extendedglob" ];
    
    # Interactive shell initialization
    interactiveShellInit = ''
      # Emacs-style keybindings
      bindkey -e
      
      # Disable annoying shell options
      unsetopt beep nomatch
      
      # Completion configuration
      zstyle ':completion:*' completer _expand _complete _ignored
      zstyle ':completion:*' matcher-list ""
      autoload -Uz compinit
      compinit
      
      # Initialize shell enhancements
      eval "$(starship init zsh)"
      eval "$(direnv hook zsh)"
      eval "$(zoxide init zsh)"
    '';
  };
}