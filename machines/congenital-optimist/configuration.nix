{
  config,
  pkgs,
  inputs,
  unstable,
  ...
}: {
  imports = [
    ./hardware-co.nix
    ./disk-ram-co.nix
    ./network-congenital-optimist.nix

    # Security modules
    ../../modules/security/ssh-keys.nix

    # Network modules
    ../../modules/network/extraHosts.nix

    # Services
    ../../modules/services/nfs-client.nix
    #../../modules/services/seatd.nix

    # Desktop environments
    ../../modules/desktop/common.nix
    ../../modules/desktop/gnome.nix
    #../../modules/desktop/cosmic.nix
    ../../modules/desktop/sway.nix
    ../../modules/desktop/niri.nix
    ../../modules/desktop/steam-xwayland-satellite.nix
    # Music production environment
    ../../modules/sound/Music/music-production.nix

    # Fonts
    ../../modules/desktop/fonts.nix

    # Development tools
    ../../modules/development/tools.nix
    ../../modules/development/emacs.nix

    # AI Tools
    ../../modules/ai-tools/antigravity.nix

    # User configuration
    ../../modules/users/geir.nix
    ../../modules/users/sma.nix

    # Virtualization configuration
    #../../modules/virtualization/incus.nix # Re-enabled with LTS version
    ../../modules/virtualization/libvirt.nix
    ../../modules/virtualization/podman.nix
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

  # Emacs workstation configuration
  services.emacs-profiles = {
    enable = true;
    profile = "gui";
    enableDaemon = true;
    user = "geir";
  };

  # Antigravity IDE
  services.antigravity.enable = true;

  # Enable clean seatd/greetd login
  #services.seatd-clean.enable = true;
  # GDM (use new option path)
  services.displayManager.gdm.enable = true;

  # ZFS services for this machine
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };

  # System packages - handled by specialized modules (music-production, etc.)
  # Additional system packages can be added here if needed
  environment.systemPackages = with pkgs; [
    # Add any other system packages here as needed
  ];

  # Basic system configuration
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "23.11"; # DO NOT CHANGE - maintains data compatibility
}
