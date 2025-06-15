# Common User Configuration
# Shared settings for all users in the home lab
{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./shell-aliases.nix
  ];
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
      eval "$(starship init zsh)"
      eval "$(direnv hook zsh)"
    '';

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
        PermitRootLogin = "no"; # No root login
        X11Forwarding = true; # For GUI applications over SSH
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
