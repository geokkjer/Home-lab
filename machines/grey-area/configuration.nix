{
  config,
  pkgs,
  ...
}: {
  imports = [
    # Hardware configuration
    ./hardware-configuration.nix

    # Shared modules
    ../../modules/common/base.nix
    ../../modules/network/common.nix
    ../../modules/network/extraHosts.nix
    ../../modules/virtualization/podman.nix
    ../../modules/virtualization/libvirt.nix
    #../../modules/virtualization/incus.nix
    ../../modules/users/sma.nix

    # Development (minimal for services host)
    ../../modules/development/emacs.nix

    # NFS client with ID mapping
    ../../modules/services/nfs-client.nix

    # Services
    ./services/jellyfin.nix
    ./services/calibre-web.nix
    ./services/audiobook.nix
    ./services/forgejo.nix
    ./services/ollama.nix
    ./services/nextcloud.nix
    ../../modules/services/open-webui.nix
  ];

  # Swap zram
  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.efi.efiSysMountPoint = "/boot/";
  boot.loader.grub.device = "nodev";

  # Disks and Updates
  services.fstrim.enable = true;

  # Emacs server configuration (minimal for services host)
  services.emacs-profiles = {
    enable = true;
    profile = "nox";
    enableDaemon = false;
    user = "sma";
  };

  # Mount remote filesystem
  fileSystems."/mnt/remote/media" = {
    device = "sleeper-service:/mnt/storage/media";
    fsType = "nfs";
    options = [
      "x-systemd.automount"
      "x-systemd.idle-timeout=60"
      "x-systemd.device-timeout=10"
      "x-systemd.mount-timeout=10"
      "noauto"
      "soft"
      "intr"
      "timeo=10"
      "retrans=3"
      "_netdev"
    ];
  };

  # Enable all unfree hardware support.
  hardware.firmware = with pkgs; [firmwareLinuxNonfree];
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
  nixpkgs.config.allowUnfree = true;
  services.fwupd.enable = true;

  # Networking
  networking.hostName = "grey-area";
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  # Text mode configuration (headless server)
  services.xserver.enable = false;
  services.displayManager.defaultSession = "none";
  boot.kernelParams = ["systemd.unit=multi-user.target"];
  systemd.targets.graphical.enable = false;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "no";
  };

  environment.systemPackages = with pkgs; [
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "no";
  services.openssh.settings.PasswordAuthentication = true;

  # Firewall
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [22 3000 23231];
  networking.firewall.allowedUDPPorts = [22 23231];
  networking.nftables.enable = true;
  system.stateVersion = "23.05"; # Do not change this, it maintains data compatibility.
}
