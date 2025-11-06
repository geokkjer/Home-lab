# Little Rascal - Development Laptop Configuration
{
  config,
  pkgs,
  lib,
  inputs,
  unstable,
  ...
}: {
  imports = [
    ./hardware-configuration.nix

    # Common modules
    ../../modules/common/base.nix
    ../../modules/common/nix.nix
    ../../modules/common/tty.nix

    # Desktop
    ../../modules/desktop/niri.nix
    # ../../modules/desktop/waybar.nix
    ../../modules/desktop/gnome.nix
    ../../modules/desktop/cosmic.nix
    ../../modules/desktop/fonts.nix
    ../../modules/desktop/input.nix

    # Development
    ../../modules/development/tools.nix
    ../../modules/development/emacs.nix

    # Users
    ../../modules/users/geir.nix
    ../../modules/users/sma.nix
    ../../modules/users/common.nix
    ../../modules/users/shell-aliases.nix

    # Virtualization
    ../../modules/virtualization/podman.nix

    # Audio
    ../../modules/sound/pipewire.nix

    # Network
    ../../modules/network/common.nix
    ../../modules/network/extraHosts.nix

    # Security
    ../../modules/security/ssh-keys.nix

    # Services
    ../../modules/services/seatd.nix
  ];

  networking = {
    hostName = "little-rascal";
    networkmanager.enable = true;

    firewall = {
      enable = true;
      allowedUDPPorts = [41641]; # Tailscale
      allowedTCPPorts = [22]; # SSH
    };
  };

  # Boot configuration
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 3;
    };

    kernelModules = ["kvm-amd" "zram"];
    tmp.cleanOnBoot = true;

    kernel.sysctl."vm.swappiness" = 180;
  };

  # Emacs GUI configuration
  services.emacs-profiles = {
    enable = true;
    profile = "gui";
    enableDaemon = true;
    user = "geir";
  };

  # zram configuration
  zramSwap = {
    enable = true;
    algorithm = "zstd";
    memoryPercent = 25; # Use 25% of RAM for zram
  };

  # Hardware - minimal for laptop
  hardware = {
    bluetooth.enable = true;
    graphics.enable = true;
  };

  # Laptop-specific services
  services = {
    #xserver.displayManager.gdm.enable = true;
    #xserver.displayManager.gdm.wayland = true; # Enable Wayland support
    # Enable clean seatd/greetd login (seat management for Wayland compositors)
    seatd-clean.enable = true;

    #    power-profiles-daemon.enable = true;
    #    upower.enable = true;

    tailscale.enable = true;
    # Bluetooth manager GUI
    #    blueman.enable = true;

    # Firmware updates via fwupd
    fwupd.enable = true;

    # Location services (used for automatic time zone, etc.)
    #   geoclue2.enable = true;
  };

  # Localization
  time.timeZone = "Europe/Oslo";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "no";
  };

  # System version
  system.stateVersion = "25.05";
}
