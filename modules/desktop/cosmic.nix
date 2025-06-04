{ config, pkgs, ... }: {
  # Cosmic Desktop Environment (System76's new Rust-based DE)
  services.desktopManager.cosmic.enable = true;
  services.displayManager.cosmic-greeter.enable = true;
  services.desktopManager.cosmic.xwayland.enable = true;
  
  # Cosmic-specific packages
  environment.systemPackages = with pkgs; [
    # Cosmic is still in development, most packages come with the DE
  ];
}