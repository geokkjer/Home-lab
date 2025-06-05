# Admin User Configuration - sma
# Named after Diziet Sma, pragmatic Special Circumstances agent
# Role: System administration, security oversight, maintenance
{ config, pkgs, ... }:

{
  users.users.sma = {
    description = "Diziet Sma - System Administrator";
    isNormalUser = true;
    
    # Admin privileges
    extraGroups = [
      "wheel"          # sudo access
      "networkmanager" # network management
      "libvirt"        # virtualization management
      "incus-admin"    # container management
      "podman"         # container runtime
      "docker"         # docker compatibility (if needed)
    ];

    # Security-focused shell setup
    shell = pkgs.zsh;
    
    # SSH key-based authentication only (no password login)
    openssh.authorizedKeys.keys = config.security.ssh-keys.admin or [
      # Admin keys will be populated from security module
    ];

    # Essential admin packages
    packages = with pkgs; [
      # System monitoring and diagnostics
      htop
      iotop
      nethogs
      lsof
      strace
      
      # Network tools
      nmap
      tcpdump
      wireshark-cli
      curl
      wget
      
      # File and disk utilities
      tree
      fd
      ripgrep
      fzf
      ncdu
      
      # Text processing
      jq
      yq
      
      # Version control (for system configs)
      git
      
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
    
    # Admin-focused aliases
    shellAliases = {
      # System management
      "rebuild" = "sudo nixos-rebuild switch --flake /home/geir/Home-lab";
      "rebuild-test" = "sudo nixos-rebuild test --flake /home/geir/Home-lab";
      "rebuild-boot" = "sudo nixos-rebuild boot --flake /home/geir/Home-lab";
      
      # Container management
      "pods" = "podman ps -a";
      "images" = "podman images";
      "logs" = "podman logs";
      
      # System monitoring
      "disk-usage" = "df -h";
      "mem-usage" = "free -h";
      "processes" = "ps aux | head -20";
      
      # Network
      "ports" = "ss -tulpn";
      "connections" = "ss -tuln";
      
      # Git for infrastructure
      "lab" = "cd /home/geir/Home-lab";
      "lab-status" = "cd /home/geir/Home-lab && git status";
      "lab-pull" = "cd /home/geir/Home-lab && git pull";
      
      # Security
      "audit-users" = "cat /etc/passwd | grep -E '/bin/(bash|zsh|fish)'";
      "audit-sudo" = "cat /etc/sudoers.d/*";
    };
  };

  # Sudo configuration for admin user
  security.sudo.extraRules = [
    {
      users = [ "sma" ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" ]; # Allow passwordless sudo for admin tasks
        }
      ];
    }
  ];

  # Admin user home directory permissions
  systemd.tmpfiles.rules = [
    "d /home/sma 0755 sma users -"
    "d /home/sma/.ssh 0700 sma users -"
  ];
}
