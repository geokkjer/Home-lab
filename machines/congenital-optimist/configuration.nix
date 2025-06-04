{
  config,
  pkgs,
  inputs,
  unstable,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    
    # System modules
    ../../modules/system/fonts.nix
    ../../modules/system/network.nix
    ../../modules/system/applications.nix
    
    # Hardware modules
    ../../modules/hardware/amd-workstation.nix
    
    # Desktop environments
    ../../modules/desktop/common.nix
    ../../modules/desktop/gnome.nix
    ../../modules/desktop/cosmic.nix
    ../../modules/desktop/sway.nix
    
    # Development tools
    ../../modules/development/tools.nix
    
    # User configuration
    ../../modules/users/geir.nix

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
  };

  # Basic system configuration
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "23.11"; # DO NOT CHANGE - maintains data compatibility
}
