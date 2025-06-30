# Little Rascal - Development Laptop Configuration
# Based on congenital-optimist with laptop-specific adjustments
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
    ../../modules/common/emacs.nix

    # Desktop
    ../../modules/desktop/niri.nix
    ../../modules/desktop/cosmic.nix
    ../../modules/desktop/fonts.nix

    # Development
    ../../modules/development/tools.nix
    ../../modules/ai/claude-code.nix

    # Users
    ../../modules/users/geir.nix
    ../../modules/users/common.nix
    ../../modules/users/shell-aliases.nix

    # Virtualization
    ../../modules/virtualization/libvirt.nix
    ../../modules/virtualization/incus.nix
    ../../modules/virtualization/podman.nix

    # Audio
    ../../modules/sound/pipewire.nix

    # Network
    ../../modules/network/common.nix
    ../../modules/network/extraHosts.nix

    # Security
    ../../modules/security/ssh-keys.nix
  ];

  networking = {
    hostName = "little-rascal";
    networkmanager.enable = true;

    # Tailscale for home lab access
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

    # zram swap like other machines
    kernel.sysctl."vm.swappiness" = 180;
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
    # Power management for laptop
    power-profiles-daemon.enable = true;
    upower.enable = true;

    # Display manager
    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd ${pkgs.zsh}/bin/zsh";
          user = "greeter";
        };
      };
    };

    # Essential services
    tailscale.enable = true;
    blueman.enable = true;
    printing.enable = true;

    # Location services for time zone
    geoclue2.enable = true;
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
