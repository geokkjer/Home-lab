{ config, pkgs, ... }: {
  # AMD GPU configuration
  hardware.amdgpu.initrd.enable = true;

  # Firmware updates and proprietary firmware
  services.fwupd.enable = true;
  hardware.enableRedistributableFirmware = true;

  # Bluetooth configuration
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  # ZRAM swap configuration
  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };

  # Audio system (PipeWire)
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  # Gaming support
  programs.steam.enable = true;
}