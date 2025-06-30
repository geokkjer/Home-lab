# Minimal NixOS Configuration for little-rascal
# This is for initial installation - use deploy-rs to apply full config afterwards
{
  config,
  pkgs,
  lib,
  ...
}: {
  # Enable flakes for configuration deployment
  nix.settings.experimental-features = ["nix-command" "flakes"];

  # Boot configuration
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 3;
    };

    # Import hardware scan results
    initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod"];
    kernelModules = ["kvm-amd"];

    # Clean tmp on boot
    tmp.cleanOnBoot = true;
  };

  # Minimal file systems - you'll need to adjust this based on your disk setup
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  # Basic networking
  networking = {
    hostName = "little-rascal";
    networkmanager.enable = true;

    # Open SSH port
    firewall = {
      enable = true;
      allowedTCPPorts = [22];
      allowedUDPPorts = [41641]; # Tailscale
    };
  };

  # Essential services
  services = {
    # SSH for remote deployment
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        PermitRootLogin = "prohibit-password";
        PubkeyAuthentication = true;
      };
    };

    # Tailscale for secure home lab access
    tailscale.enable = true;
  };

  # Create admin user for deployment
  users = {
    mutableUsers = false;
    users = {
      root = {
        # Add your SSH public key here for initial access
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPgzKS1N7+7+N1/8U8++1pl4hapDm6TOy0QhrfrYA8mz geir@geokkjer.eu-admin" # Admin key
        ];
      };

      geir = {
        isNormalUser = true;
        extraGroups = ["wheel" "networkmanager" "sudo"];
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHukJK0Kc1YexvzF8PdqaqWNZdVffGoM6ePPMecrU6dM geir@geokkjer.eu-dev" # Dev key
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPgzKS1N7+7+N1/8U8++1pl4hapDm6TOy0QhrfrYA8mz geir@geokkjer.eu-admin" # Admin key for backup access
        ];
      };
    };
  };

  # Sudo configuration
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Basic packages for system management
  environment.systemPackages = with pkgs; [
    git
    vim
    htop
    curl
    wget
  ];
  # System version
  system.stateVersion = "25.05";
}
