# Primary User Configuration - geir
# Main user account for development and desktop use
{
  config,
  pkgs,
  ...
}: let
  # Import custom packages from the flake
  homeLabPackages = import ../../packages {inherit pkgs;};
in {
  imports = [
    ./media-group.nix
  ];

  users.users.geir = {
    description = "Geir Okkenhaug Jerstad - Primary User";
    isNormalUser = true;

    extraGroups = [
      "wheel"
      "networkmanager"
      "libvirt"
      "incus-admin"
      "podman"
      "audio"
      "video"
      "render"
      "media"
    ];

    shell = pkgs.zsh;

    # Ensure home directory is created so SSH authorized keys are applied
    createHome = true;

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

      # Terminal
      starship
      ghostty
      wezterm
      slides

      # Essential system tools (moved duplicates to base.nix)
      mc

      # Browsers & Communication
      firefox
      chromium
      vesktop
      vivaldi
      vivaldi-ffmpeg-codecs

      # Shell Enhancement & Fun
      nerdfetch
      neo-cowsay
      fortune
      clolcat

      # Audio & System Control
      ncpamixer
      pavucontrol

      # Productivity
      koodo-reader

      # Development & System Management
      git-credential-manager
      jujutsu
      nodejs
      virt-manager

      # Creative Tools (optional - remove if not needed)
      gimp
      obs-studio

      # Container tools
      podman-compose
      #podman-desktop

      # Media
      celluloid
      ytmdesktop

      # Gaming
      steam
      # Desktop integration (moved from system)
      dbus
      wayland
      xwayland
      xwayland-satellite
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
    TERMINAL = "wezterm";

    # Git configuration
    GIT_EDITOR = "nano";
  };

  # Comprehensive zsh configuration for geir
  programs.zsh = {
    enable = true;

    # Shell aliases (user-specific only, common ones in shell-aliases.nix)
    shellAliases = {
      # Development workflow - geir specific
      "home-lab" = "z /home/geir/Home-lab";
      "configs" = "z /home/geir/Home-lab/user_configs/geir";
      "emacs-config" = "emacs /home/geir/Home-lab/user_configs/geir/emacs.org";

      # Flake-specific rebuilds (geir has access to local flake directory)
      "rebuild-local" = "sudo nixos-rebuild switch --flake /home/geir/Home-lab";
      "rebuild-local-test" = "sudo nixos-rebuild test --flake /home/geir/Home-lab";

      # Git shortcuts for multi-remote workflow
      "git-status-all" = "git status && echo '--- Checking origin ---' && git log origin/main..HEAD --oneline && echo '--- Checking github ---' && git log github/main..HEAD --oneline";
    };

    # History configuration
    histSize = 10000;
    histFile = "$HOME/.histfile";

    # Shell options
    setOptions = ["autocd" "extendedglob"];

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
