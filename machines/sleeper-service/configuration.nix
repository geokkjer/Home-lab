{
  config,
  lib,
  pkgs,
  inputs,
  unstable,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    # Security modules
    ../../modules/security/ssh-keys.nix
    # Network configuration
    ./network-sleeper-service.nix
    ../../modules/network/extraHosts.nix
    # Services
    ./nfs.nix
    ./services/transmission.nix

    # Development (minimal for server)
    ../../modules/development/emacs.nix

    # User modules - server only needs sma user
    ../../modules/users/sma.nix
  ];

  # Boot configuration with ZFS support
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

  boot.supportedFilesystems = ["zfs"];
  boot.loader.grub.memtest86.enable = true;

  # Add nomodeset for graphics compatibility
  boot.kernelParams = ["nomodeset"];

  # ZFS services for file server
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };

  # Emacs server configuration (minimal)
  services.emacs-profiles = {
    enable = true;
    profile = "server";
    enableDaemon = false; # Don't run daemon on server
    user = "sma";
  };

  # Enable ZFS auto-mounting since we're using ZFS native mountpoints
  # systemd.services.zfs-mount.enable = lib.mkForce false;

  # Disable graphics for server use - comment out NVIDIA config for now
  # hardware.graphics = {
  #   enable = true;
  # };
  # hardware.nvidia = {
  #   modesetting.enable = true;
  #   open = false;
  #   package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  # };

  # Comment out NVIDIA kernel modules for now
  # boot.kernelModules = [ "nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];

  # Comment out NVIDIA utilities for now
  # environment.systemPackages = with pkgs; [
  #   config.boot.kernelPackages.nvidiaPackages.legacy_470
  # ];

  # Create mount directories early in boot process
  # systemd.tmpfiles.rules = [
  #   "d /mnt/storage 0755 root root -"
  #   "d /mnt/storage/media 0755 root root -"
  # ];

  # Network configuration - using working setup from old config
  # networking.hostName = "sleeper-service";
  # services.tailscale.enable = true;
  # networking.networkmanager.enable = true;
  # networking.hostId = "8425e349";

  # Time and locale
  time.timeZone = "Europe/Oslo";
  i18n.defaultLocale = "en_US.UTF-8";

  # Console configuration
  console = {
    font = "Lat2-Terminus16";
    keyMap = "no";
  };

  # Enable unfree packages
  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.nvidia.acceptLicense = true;  # Commented out for now

  programs.zsh.enable = true;

  # Enable SSH
  services.openssh.enable = true;

  # Firewall configuration - disable for simplicity like old config
  # networking.firewall.enable = false;

  # DO NOT CHANGE - maintains data compatibility
  system.stateVersion = "23.11";
}
