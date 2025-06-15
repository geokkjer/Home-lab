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

    # Desktop environments
    ../../modules/desktop/common.nix
    ../../modules/desktop/gnome.nix
    ../../modules/desktop/cosmic.nix
    ../../modules/desktop/sway.nix
    ../../modules/desktop/niri.nix
    ../../modules/desktop/steam-xwayland-satellite.nix

    # Fonts
    ../../modules/desktop/fonts.nix

    # Development tools
    ../../modules/development/tools.nix

    # User configuration
    ../../modules/users/geir.nix
    ../../modules/users/sma.nix

    # Virtualization configuration
    ../../modules/virtualization/incus.nix
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
  }; # ZFS services for this machine
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };
  # Basic system configuration
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "23.11"; # DO NOT CHANGE - maintains data compatibility
}
