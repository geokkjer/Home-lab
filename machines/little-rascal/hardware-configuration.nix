# Hardware Configuration for Little Rascal
# Lenovo Yoga Slim 7 14ARE05 - AMD Ryzen 7 4700U
{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Boot configuration for AMD Ryzen 7 4700U
  boot = {
    initrd = {
      availableKernelModules = [
        "nvme"
        "xhci_pci"
        "usb_storage"
        "sd_mod"
        "sdhci_pci"
        # I2C modules for touchpad support
        "i2c_hid"
        "i2c_hid_acpi"
        "i2c_piix4"
      ];
      kernelModules = [];
    };

    kernelModules = [
      "kvm-amd" # AMD Ryzen system
      # HID and input modules for touchpad
      "hid_generic"
      "hid_multitouch"
      "i2c_hid"
      "i2c_hid_acpi"
      # Additional HID drivers that might help
      "hid_input"
      "evdev"
    ];

    # Use LTS kernel for better hardware compatibility
    kernelPackages = pkgs.linuxPackages_6_1;

    extraModulePackages = [];
  };

  # Filesystem configuration - TEMPLATE
  # Update these paths and UUIDs after running nixos-generate-config
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-label/BOOT";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };
  };

  # Swap configuration - TEMPLATE
  # Uncomment and update if using swap partition
  # swapDevices = [
  #   { device = "/dev/disk/by-uuid/REPLACE-WITH-SWAP-UUID"; }
  # ];

  # Hardware-specific configuration for Lenovo Yoga Slim 7 14ARE05
  hardware = {
    # CPU configuration - AMD Ryzen 7 4700U
    cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

    # Enable firmware updates
    enableRedistributableFirmware = true;

    # Graphics configuration - AMD Radeon Vega (integrated)
    # Using open source driver without ROCm and 32-bit support
    graphics = {
      enable = true;
      enable32Bit = false; # Disabled 32-bit support

      # AMD open source graphics drivers only
      extraPackages = with pkgs; [
        amdvlk # AMD Vulkan driver
        # Removed ROCm packages for simpler configuration
      ];

      # No 32-bit support packages needed
    };

    # Bluetooth support for Intel AX200
    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };

  # Power management for AMD Ryzen 7 4700U
  powerManagement = {
    enable = true;
    powertop.enable = true; # Power optimization
    cpuFreqGovernor = "powersave"; # Better battery life
  };

  # Network hardware - Intel Wi-Fi 6 AX200
  networking = {
    # Enable NetworkManager for WiFi management
    networkmanager.enable = true;

    # Disable wpa_supplicant (using NetworkManager)
    wireless.enable = false;
  };

  # Firmware for Intel WiFi and Bluetooth
  hardware.firmware = with pkgs; [
    linux-firmware
  ];

  # AMD-specific optimizations
  boot.kernelParams = [
    # Enable AMD graphics performance
    "amdgpu.ppfeaturemask=0xffffffff"
    # I2C HID touchpad parameters
    "i2c_hid.debug=1"
    # Ensure ACPI devices are properly detected
    "acpi_enforce_resources=lax"
    # Force ITE touchpad to be recognized as input device
    "i2c_hid_acpi.probe_defer=1"
  ];

  # TLP for better power management (alternative to power-profiles-daemon)
  services.tlp = {
    enable = false; # Using power-profiles-daemon instead
    settings = {
      # Would be configured here if enabled
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    };
  };

  # Notes for this specific hardware:
  # - Lenovo Yoga Slim 7 14ARE05
  # - AMD Ryzen 7 4700U with Radeon Vega Graphics
  # - 16GB LPDDR4 RAM (soldered, not upgradeable)
  # - Intel Wi-Fi 6 AX200 + Bluetooth
  # - 128GB SSD storage
  # - Currently running btrfs filesystem
}
