{ config, pkgs, inputs, unstable, ... }: {
  imports = [
    ./hardware-configuration.nix
  ];

  # Boot configuration
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
    devices = [ "nodev" ];
  };

  # Network configuration
  networking.hostName = "sleeper-service";
  networking.networkmanager.enable = true;
  services.tailscale.enable = true;
  networking.firewall.enable = true;

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

  # SSH access (headless server)
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

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

  # Users
  users.users.geir = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      # Add SSH public keys here
    ];
  };

  programs.zsh.enable = true;

  # Firewall configuration
  networking.firewall.allowedTCPPorts = [ 22 ];

  system.stateVersion = "25.05";
}