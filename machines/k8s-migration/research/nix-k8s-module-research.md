# NixOS Kubernetes Module: Deep Dive

**Status:** CHOSEN APPROACH for home-lab cluster  
**Date:** November 2025

## Overview

Use NixOS's built-in `services.kubernetes` module to run Kubernetes directly on the host system alongside other services. This integrates Kubernetes as a first-class NixOS component rather than running it separately.

## Architecture

```
Host (grey-area, NixOS)
├── Kubernetes API server
├── etcd
├── kubelet
├── containerd runtime
├── Ollama (same host)
├── Forgejo (same host)
└── Other services (same host)
```

## Benefits

✅ **Full integration**: Kubernetes coexists with Ollama, Forgejo, etc.  
✅ **No performance overhead**: Runs natively on host  
✅ **Declarative configuration**: Managed via NixOS like everything else  
✅ **Direct resource access**: Full access to GPUs, storage, NICs  
✅ **Reproducible**: Entire config in flake.nix  
✅ **System consistency**: Same OS, same principles for all components  
✅ **Easier troubleshooting**: Unified logging, systemd integration  
✅ **Cost-effective**: Zero resource overhead from containerization  

## Drawbacks

❌ **Complexity**: NixOS Kubernetes module has limited documentation  
❌ **Less isolation**: Kubernetes runs with host OS access; one system break affects all  
❌ **Fewer Talos benefits**: NixOS doesn't have Talos's immutability guarantees  
❌ **Version coupling**: Kubernetes version tied to NixOS release  
❌ **Limited customization**: Module may not support all Kubernetes features  
❌ **Upgrade challenges**: Changes to system can impact cluster  
❌ **State management**: Persistent state on host makes recovery harder  
❌ **Community size**: Fewer examples/support vs. standalone Kubernetes  

## Basic Implementation Pattern

```nix
# In machines/grey-area/configuration.nix or separate module

services.kubernetes = {
  enable = true;
  roles = ["master" "node"];  # Single-node: both roles
  
  apiserver = {
    enable = true;
    # API server configuration
  };
  
  controllerManager = {
    enable = true;
  };
  
  scheduler = {
    enable = true;
  };
  
  kubelet = {
    enable = true;
    # Node configuration
  };
  
  # Storage: etcd or other
  etcd = {
    enable = true;
  };
};
```

## Multi-Node Configuration

For 3-node setup, configure each machine as a master/worker:

```nix
# grey-area
services.kubernetes = {
  enable = true;
  roles = ["master" "node"];
  masterAddress = "grey-area.tailnet";  # Tailscale hostname
  # ... rest of config
};

# congenital-optimist
services.kubernetes = {
  enable = true;
  roles = ["master" "node"];
  masterAddress = "grey-area.tailnet";  # Join to grey-area
  # ... rest of config
};

# limiting-factor
services.kubernetes = {
  enable = true;
  roles = ["node"];
  masterAddress = "grey-area.tailnet";  # Join to grey-area
  # ... rest of config
};
```

## Resource Requirements

- **CPU**: 2+ cores
- **RAM**: 4-8 GB (shared with other services)
- **Storage**: 30+ GB (Kubernetes + Ollama + other services)
- **Network**: Host network (no port mapping needed)

## Configuration Complexity

```
# Estimated module content needed:
# - 150-250 lines for basic 1-node setup
# - Requires knowledge of NixOS + Kubernetes fundamentals
# - Learning curve: 2-4 hours for someone familiar with both
```

## When to Use NixOS Kubernetes Module

- **Full system integration**: Kubernetes as one component of larger system
- **Existing NixOS setup**: Minimal additional infrastructure
- **Stable, predictable workloads**: No need for frequent cluster reset
- **Development on grey-area**: Code, test, deploy all on one machine
- **Long-term production**: System should be stable

## When NOT to Use

❌ Need extreme Kubernetes customization beyond NixOS module support  
❌ Require rapid cluster teardown and recreation (use Talos for that)  
❌ Team unfamiliar with both NixOS AND Kubernetes (steep learning curve)  
❌ Need immutable infrastructure (Talos is better suited)  

## Integration with Services-OS Separation

The NixOS Kubernetes module supports the separation architecture:

1. **Infrastructure Layer (NixOS)**: K8s runtime, networking, storage
2. **Service Layer (Kubernetes)**: All application workloads via containers
3. **GitOps Layer (ArgoCD)**: Declarative deployments from Git

NixOS handles (1), Kubernetes handles (2) and (3).

## Networking Considerations

### Tailscale Integration

Keep Kubernetes control plane communication over Tailscale:

```nix
services.kubernetes = {
  # ... other config
  apiserver = {
    advertiseAddress = "10.x.x.x";  # Tailscale IP
    # ...
  };
};

# Ensure tailscaled is running
services.tailscale.enable = true;
```

### Pod Network

Kubernetes manages pod networking separately:
- Pod CIDR: 10.244.0.0/16 (default)
- Service CIDR: 10.96.0.0/12 (default)
- These are internal to cluster

### External Access

External services via:
- Nginx reverse proxy (already in use)
- Traefik ingress controller (running in Kubernetes)
- Tailscale MagicDNS for internal access

## Comparison with Talos

| Factor | NixOS Module | Talos |
|--------|--------------|-------|
| Setup | Integrated | Standalone |
| Overhead | None | Minimal |
| Customization | Very flexible | Limited by Talos |
| Immutability | No | Yes |
| Documentation | Limited | Excellent |
| Community | Small K8s + large Nix | Growing |
| Learning curve | Steep (both OSes) | Medium (Talos only) |
| Production ready | Yes | Yes |

## Recommended Next Steps

1. **Set up development environment** with NixOS Kubernetes module on a test machine
2. **Document module configuration** as you discover patterns
3. **Create reusable module** for easy deployment across 3 nodes
4. **Test multi-node join** with Tailscale networking
5. **Validate with first service** (e.g., Calibre-Web)
6. **Document learnings** in implementation guides

## Resources

- [NixOS Kubernetes Documentation](https://nixos.org/manual/nixos/stable/index.html#module-services-kubernetes)
- [NixOS Wiki - Kubernetes](https://nixos.wiki/wiki/Kubernetes)
- [Kubernetes Official Docs](https://kubernetes.io/docs/)
- [Kubernetes + NixOS Blog Posts](https://github.com/search?q=NixOS+kubernetes)

## Known Limitations

1. **etcd replication**: May require manual configuration for HA
2. **CNI plugins**: Limited built-in support; may need manual setup
3. **Helm integration**: Not built-in; use separately
4. **Upgrade path**: K8s version tied to NixOS version cycle
5. **Troubleshooting**: Logs scattered between systemd and K8s logs

## Decision Log

**2025-11-23**: Chosen NixOS Kubernetes Module over Talos approach
- Reason: Better integration with existing NixOS infrastructure
- Risk: Limited community documentation; need to experiment more
- Mitigation: Start with simple single-node, iterate to multi-node

---

**Last Updated:** 2025-11-23  
**Status:** Research document; implementation details to follow
