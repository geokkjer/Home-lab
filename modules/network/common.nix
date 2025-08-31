# Common Network Configuration
# Minimal shared networking settings across all machines
{
  config,
  pkgs,
  unstable,
  ...
}: {
  # Common networking settings
  networking = {
    # Enable nftables by default for all machines
    nftables.enable = true;

    # Basic firewall settings (SSH handled by security/ssh-keys.nix)
    firewall = {
      enable = true;
      # SSH port is configured in modules/security/ssh-keys.nix
    };
  };

  # Common services available on all machines
  services = {
    # Tailscale VPN for secure remote access
    tailscale = {
      enable = true;
      useRoutingFeatures = "client";
      package = unstable.tailscale; # Use unstable version for better binary cache coverage
    };

    # Note: SSH configuration is handled by modules/security/ssh-keys.nix
  };
}
