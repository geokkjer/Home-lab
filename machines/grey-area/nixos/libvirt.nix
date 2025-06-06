{ config, pkgs, ... }:
{
  virtualisation.libvirtd.enable = true;
  environment.systemPackages = with pkgs; [
    qemu_kvm
    libvirt
    polkit
  ];
}
