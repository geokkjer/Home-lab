# Primary User Configuration - geir
# Main user account for development and desktop use
{ config, pkgs, ... }:

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
    
    # User-specific packages
    packages = with pkgs; [
      # Browsers & Communication
      chromium
      vivaldi
      vivaldi-ffmpeg-codecs
      nyxt
      firefox
      vesktop
      
      # Terminal & Shell Enhancement
      starship
      fastfetch
      hyfetch
      nerdfetch
      zellij
      neo-cowsay
      fortune
      lolcat
      
      # Audio & System Control
      ncpamixer
      pavucontrol
      
      # Creative & Productivity
      gimp
      obs-studio
      koodo-reader
      libreoffice
      
      # Development & System Management
      virt-manager
      gnome-tweaks
      
      # Themes & Appearance
      beauty-line-icon-theme
      
      # Emacs Integration
      emacsPackages.vterm
      
      # Media & Entertainment
      vlc
      mpv
      
      # File Management
      nautilus
      file-roller
      
      # Text Editors (alternatives to Emacs)
      neovim
      vscode
      
      # Development Tools
      git-credential-manager
      github-cli
      
      # Containers & Cloud
      podman-compose
      podman-desktop
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
    TERMINAL = "alacritty";
    
    # Git configuration
    GIT_EDITOR = "emacs";
  };

  # Geir-specific shell configuration
  programs.zsh.shellAliases = {
    # Development workflow
    "lab" = "cd /home/geir/Home-lab";
    "configs" = "cd /home/geir/Home-lab/user_configs/geir";
    "emacs-config" = "emacs /home/geir/Home-lab/user_configs/geir/emacs.org";
    
    # Quick system management
    "rebuild-test" = "sudo nixos-rebuild test --flake /home/geir/Home-lab";
    "rebuild" = "sudo nixos-rebuild switch --flake /home/geir/Home-lab";
    
    # Container shortcuts
    "pdm" = "podman";
    "pdc" = "podman-compose";
    
    # Media shortcuts
    "youtube-dl" = "yt-dlp";
  };
}