{ pkgs, ... }: {
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    dockerSocket.enable = true;
    defaultNetwork.settings.dns_enabled = true;
  };

  environment.systemPackages = with pkgs; [
    podman-tui
    podman-compose
    buildah
    skopeo
  ];
  
  # Enable container runtime for desktop integration
  virtualisation.containers.enable = true;
}
