# Admin User Configuration - sma
# Named after Diziet Sma, pragmatic Special Circumstances agent
# Role: System administration, security oversight, maintenance
{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./media-group.nix
  ];

  users.users.sma = {
    description = "Diziet Sma - System Administrator";
    isNormalUser = true;
    uid = 1001; # Fixed UID for consistency across machines
    group = "sma"; # Primary group

    # Admin privileges
    extraGroups = [
      "wheel" # sudo access
      "networkmanager" # network management
      "libvirt" # virtualization management
      "incus-admin" # container management
      "podman" # container runtime
      "docker" # docker compatibility (if needed)
      "media" # shared media access for NFS shares
    ];

    # Security-focused shell setup
    shell = pkgs.zsh;

    # SSH key-based authentication only (no password login)
    openssh.authorizedKeys.keys = [
      # Admin key for server administration
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPgzKS1N7+7+N1/8U8++1pl4hapDm6TOy0QhrfrYA8mz geir@geokkjer.eu-admin"
    ];

    # Essential admin packages
    packages = with pkgs; [
      # System monitoring and diagnostics (htop, lsof, strace moved to base.nix)
      iotop
      nethogs

      # Network tools (nmap moved to base.nix)
      tcpdump
      wireshark-cli

      # File and disk utilities (tree, fd, ripgrep, fzf, ncdu moved to base.nix)

      # Text processing (jq, yq moved to base.nix)

      # Version control (git moved to base.nix)

      # Container management
      podman-compose

      # Backup and sync
      rsync
      rclone

      # Security tools
      age
      sops

      # NixOS specific tools
      nixos-rebuild
      nix-tree
      nix-diff
    ];
  };

  # Admin-specific shell configuration
  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;

    # Admin-specific aliases (common ones in shell-aliases.nix)
    shellAliases = {
      # Flake management from remote deployments (sma uses temp directory)
      "rebuild-remote" = "cd /tmp/home-lab-config && sudo nixos-rebuild switch --flake .";
      "rebuild-remote-test" = "cd /tmp/home-lab-config && sudo nixos-rebuild test --flake .";
      "rebuild-remote-boot" = "cd /tmp/home-lab-config && sudo nixos-rebuild boot --flake .";
    };
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
    '';
  };

  # Sudo configuration for admin user
  security.sudo.extraRules = [
    {
      users = ["sma"];
      commands = [
        {
          command = "ALL";
          options = ["NOPASSWD"]; # Allow passwordless sudo for admin tasks
        }
      ];
    }
  ];

  # Admin user home directory permissions
  systemd.tmpfiles.rules = [
    "d /home/sma 0755 sma sma -"
    "d /home/sma/.ssh 0700 sma sma -"
  ];

  # Create the sma group
  users.groups.sma = {
    gid = 992; # Fixed GID for consistency across machines
  };
}
