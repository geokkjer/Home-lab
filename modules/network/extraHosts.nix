# Network hostname resolution module
# Provides consistent hostname-to-IP mapping across all home lab machines
# Uses Tailscale IPs for reliable connectivity across the mesh network
{
  config,
  lib,
  unstable,
  ...
}: {
  # Add hostname entries for all home lab machines using Tailscale IPs
  networking.extraHosts = ''
    # Home Lab Infrastructure (Tailscale mesh network)
    100.109.28.53   congenital-optimist
    100.81.15.84    sleeper-service
    100.119.86.92   grey-area
    100.96.189.104  reverse-proxy vps1

    # Additional network devices
    100.103.143.108 pihole
  '';
}
