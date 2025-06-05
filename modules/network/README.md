# Network Configuration Modules

This directory contains networking configurations for all machines in the Home Lab.

## Structure

- **`common.nix`** - Shared networking settings used by all machines
  - nftables firewall enabled
  - SSH access with secure defaults
  - Tailscale VPN for remote access
  - Basic firewall rules (SSH port 22)

- **`network-<machine-name>.nix`** - Machine-specific networking configurations
  - Import `common.nix` for shared settings
  - Override or extend with machine-specific requirements
  - Define hostname, hostId, and additional firewall ports

## Current Machines

### network-congenital-optimist.nix
- AMD Threadripper workstation
- ZFS configuration (hostId: 8425e349)
- Additional ports: 9091 (Transmission RPC)

### network-sleeper-service.nix  
- Xeon file server
- Headless server configuration
- Ready for additional file sharing service ports

## Usage

Each machine configuration imports its specific network module:

```nix
# In machines/<machine-name>/configuration.nix
imports = [
  ../../modules/network/network-<machine-name>.nix
  # ... other imports
];
```

## Adding New Machines

1. Create `network-<new-machine>.nix` in this directory
2. Import `./common.nix` for shared settings
3. Add machine-specific configuration (hostname, hostId, ports)
4. Import the new file in the machine's `configuration.nix`

## Future Refactoring

The `common.nix` file can be extended to include more shared networking patterns as they emerge across machines. Consider moving repeated patterns here to reduce duplication.
