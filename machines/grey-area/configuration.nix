{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./starship.nix
      ./aliases.nix
      ./podman.nix
      ./libvirt.nix
      ./incus.nix
      ./jellyfin.nix
      ./tailscale.nix
      ./calibre-web.nix
      ./audiobook.nix
      #./ollama.nix
      ./forgejo.nix
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

  # Mount remote filesystem
  fileSystems."/mnt/remote/media" = {
    device = "sleeper-service:/mnt/storage";
    fsType = "nfs";
    options = [ "x-systemd.automount" ];
  };

  # Enable all unfree hardware support.
  hardware.firmware = with pkgs; [ firmwareLinuxNonfree ];
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
  nixpkgs.config.allowUnfree = true;
  services.fwupd.enable = true;

  # Networking
  networking.hostName = "grey-area"; 
  networking.networkmanager.enable = true;  

  # Set your time zone.
  time.timeZone = "Europe/Oslo";


  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "no";
  };

  users.users.geir = {
    isNormalUser = true;
    extraGroups = [ "wheel"
                    "networkmanager"
                    "libvirt"
                    "podman"
		    "incus-admin"
                  ];
    packages = with pkgs; [
        bottom fastfetch nerdfetch
    ];
  };

  environment.systemPackages = with pkgs; [
    neovim emacs nano curl htop glances kitty 
    wget git inxi nethogs fastfetch
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "no";
  services.openssh.settings.PasswordAuthentication = true; 


  # Enable Netdata
  services.netdata.enable = true;

  # Firewall
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 19999 23231];
  networking.firewall.allowedUDPPorts = [ 22 23231 ];
  networking.nftables.enable = true;
  system.stateVersion = "23.05"; # Do not change this, it maintains data compatibility.

}
