{
  config,
  pkgs,
  ...
}: {
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = true;
      swtpm.enable = true;
      # OVMF images are provided by QEMU by default now; remove explicit OVMF configuration.
    };
  };

  environment.systemPackages = with pkgs; [
    qemu_kvm
    libvirt
    virt-manager
    virt-viewer
  ];
}
