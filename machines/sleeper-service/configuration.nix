{ config, pkgs, inputs, unstable, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../modules/network/network-sleeper-service.nix

    # Security modules
    ../../modules/security/ssh-keys.nix

    # Services
    ../../modules/services/nfs.nix
    ../../modules/system/transmission.nix

    # User modules - server only needs sma user
    ../../modules/users/sma.nix
  ];

  # Boot configuration
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
    devices = [ "nodev" ];
  };

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

  # Basic system packages
  environment.systemPackages = with pkgs; [
    wget
    curl
    git
    htop
    eza
    bat
    ripgrep
    du-dust
    fd
    ncdu
    tree
  ];

  programs.zsh.enable = true;

  # Firewall configuration
  networking.firewall.allowedTCPPorts = [ 22 ]; # SSH only (Transmission disabled temporarily)

  system.stateVersion = "25.05";
}