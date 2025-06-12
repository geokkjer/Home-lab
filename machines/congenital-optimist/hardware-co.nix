{
  config,
  pkgs,
  lib,
  ...
}: {
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

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  # Audio system (PipeWire)
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
}
