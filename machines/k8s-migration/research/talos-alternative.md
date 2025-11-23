# Why NixOS Kubernetes Modules Instead of Talos

## Quick Decision Summary

**Chosen Approach:** NixOS with `services.kubernetes` modules  
**Rejected Approach:** Talos Linux  
**Rationale:** Familiarity with NixOS ecosystem, simpler operations within existing infrastructure, ability to declaratively manage both OS and Kubernetes from one source of truth.

## What is Talos Linux?

Talos is a minimal, hardened Linux designed specifically for Kubernetes—no shell, no SSH, no package manager. It's managed entirely through the Talos API, making it immutable and purpose-built.

### Talos Advantages

- **Immutable OS**: No in-place edits, reproducible state
- **Purpose-built for K8s**: Minimal attack surface, optimized for containers
- **Excellent security posture**: No package manager = fewer CVEs to patch
- **Canonical support**: Battle-tested by large organizations
- **Sidero metal provisioning**: Excellent for managing bare metal clusters

### Talos Disadvantages

- **Steep learning curve**: Different operational model from traditional Linux
- **Limited flexibility**: Can't easily add custom tools or modify OS behavior
- **API-first management**: Requires understanding Talos machine config YAML
- **Maintenance burden**: Must manage another layer (Talos API + Kubernetes)
- **Integration friction**: Doesn't play well with NixOS tooling or other Linux conventions
- **Overkill for home lab**: Adds complexity without corresponding benefit for single-admin deployment

## Why NixOS Kubernetes Modules Instead?

### 1. Leverage Existing NixOS Knowledge

You're already managing NixOS machines (`grey-area`, `congenital-optimist`, `limiting-factor`). Using `services.kubernetes` means:

- **One configuration language** (Nix) for both OS and Kubernetes
- **Existing deployment tooling** (`nixos-rebuild`, `deploy-rs`)
- **Familiar troubleshooting methods** (`journalctl`, `systemctl`, NixOS options)
- **No context switching** between Talos management API and NixOS administration

### 2. Gradual Migration Path

NixOS Kubernetes modules allow you to run both traditional services AND Kubernetes workloads on the same machines:

```nix
# Same machine can run traditional services...
services.openssh.enable = true;
services.nginx.enable = true;

# ...and Kubernetes
services.kubernetes = {
  roles = [ "master" "node" ];
  apiserver = { ... };
  kubelet = { ... };
};
```

This matters for transitioning existing services incrementally without a hard cutover.

### 3. Single Source of Truth

With NixOS modules, the entire infrastructure is declarative:

```
machines/
├── grey-area/
│   └── configuration.nix          # OS + K8s config combined
├── congenital-optimist/
│   └── configuration.nix
└── limiting-factor/
    └── configuration.nix
```

Compare to Talos approach:

```
machines/
├── talos-config/
│   ├── controlplane.yaml          # OS config
│   └── worker.yaml
└── kubernetes-manifests/           # K8s config (separate)
    ├── ingress.yaml
    └── services.yaml
```

With NixOS, the infrastructure *is* version-controlled as one unit.

### 4. Easier Operations & Debugging

- **SSH access**: Still have traditional shell access for debugging
- **System logs**: Standard `journalctl` instead of learning Talos API
- **Package installation**: Can install debugging tools (`tcpdump`, `nmap`, etc.) without special procedures
- **Custom scripts**: Can add shell scripts, monitoring agents, etc. to the NixOS config

### 5. Better for Single Admin

Home lab with one admin benefits from simplicity over purity:

- **Faster troubleshooting**: Direct shell access beats API-first debugging
- **Fewer moving parts**: One OS layer instead of OS + Talos API layer
- **Lower cognitive load**: Already understand NixOS, extends naturally to K8s

## Talos Still Makes Sense When...

Talos excels in scenarios NixOS Kubernetes modules don't:

✅ **Large, multi-team operations** requiring strict immutability and API control  
✅ **Cloud-native deployments** with external provisioning (Sidero, Equinix Metal)  
✅ **Heterogeneous infrastructure** where teams want K8s without the OS flexibility  
✅ **Hyper-security environments** where package manager access is a risk  
✅ **Maximum uptime requirements** where in-place changes are forbidden  

❌ **Home lab with existing NixOS machines** ← Not a good fit

## Technical Comparison

| Aspect | Talos | NixOS Kubernetes Modules |
|--------|-------|---------------------------|
| **OS Management** | Talos API | NixOS declarative config |
| **Learning Curve** | Steep | Familiar (extends NixOS knowledge) |
| **Shell Access** | No (by design) | Yes (standard SSH) |
| **Mixed Workloads** | K8s only | OS services + K8s |
| **Configuration Language** | YAML machine configs | Nix |
| **Debugging** | Talos API + K8s logs | journalctl, systemctl, SSH |
| **Tooling Integration** | Limited | Full NixOS ecosystem |
| **Community Size** | Growing | Larger NixOS community |
| **Operational Maturity** | Proven at scale | Stable, widely used |
| **Home Lab Fit** | 6/10 | 9/10 |

## Migration Path: NixOS-First Design

### Phase 1: Bootstrap with NixOS + K8s
- All three machines running NixOS
- Kubernetes services.kubernetes module on control planes
- Both OS and K8s managed from Git

### Phase 2: Migrate Services
- Run services in Kubernetes via deployments/statefulsets
- Keep traditional services until tested in K8s
- Use ArgoCD to manage deployment source of truth

### Phase 3: Specialize if Needed
- If operations demands warrant, could later migrate a node to Talos
- But keep core infrastructure on NixOS for simplicity

## Known Limitations of NixOS Kubernetes Modules

1. **Fewer pre-built integrations** than Talos ecosystem
2. **Documentation scattered** across NixOS manual and community wikis
3. **Requires understanding both NixOS and Kubernetes** concepts
4. **Smaller community** than Talos (but growing)
5. **Networking setup more manual** than Talos defaults

These are real trade-offs, but for a 3-node home lab, they're acceptable given the benefits.

## Conclusion

**For this home lab project:**
- **NixOS Kubernetes modules** = right tool for single admin, existing NixOS knowledge, gradual migration
- **Talos** = overkill complexity, context switching, limited benefit

The decision is pragmatic: use what you know, extend it safely, iterate incrementally. You can always reconsider later if operations needs change.

---

**Last Updated:** 2025-11-23  
**Decision Status:** Accepted, informing all downstream planning  
**See Also:** [NixOS K8s Module Research](./nix-k8s-module-research.md)
