# Phase 0: Foundation & Pre-Migration Preparation

**Purpose**: Establish critical infrastructure prerequisites, make key architectural decisions, and ensure a solid foundation before beginning Kubernetes deployment. This phase reduces risk and prevents costly rework during later phases.

**Timeline**: 1-2 weeks (before Phase 1)  
**Status**: Not Started  
**Last Updated**: 2025-11-25

---

## Project Scope & Philosophy

### What This Is

An **advanced home-lab** environment designed to:
- Mirror production-grade practices for **learning and experimentation**
- Provide services to **one primary user** with potential for **additional consumers** (family, friends, self-hosted services)
- Serve as a **sandbox for exploring** Kubernetes, GitOps, infrastructure-as-code, and modern DevOps patterns
- Balance **best practices** with **pragmatic home-lab constraints** (cost, complexity, time)

### What This Is NOT

- A revenue-generating production system requiring 99.99% uptime
- An enterprise environment with compliance requirements
- A system where "move fast and break things" is unacceptable

### Guiding Principles

1. **Learning over perfection** - Prefer understanding why something works over cargo-culting solutions
2. **Production-like, not production** - Implement HA and backup strategies, but accept some risk
3. **Iterate, don't overengineer** - Start simple, add complexity when you understand the tradeoffs
4. **Document decisions** - Future-you will thank present-you
5. **Fail safely** - When experimenting, have rollback paths

---

## Phase 0 Checklist

### 0.1 Architectural Decisions (ADRs)

Before implementation, document key decisions that affect the entire project.

- [ ] **ADR-001: Talos vs NixOS Kubernetes Module**
  - Decision required: Which Kubernetes runtime to use
  - Considerations:
    - Talos: Immutable, purpose-built for K8s, better isolation, but another OS to learn
    - NixOS `services.kubernetes`: Unified Nix ecosystem, existing familiarity, but less K8s-native
  - Recommendation for learning-focused lab: **NixOS module** (builds on existing knowledge, easier debugging)
  - Document in: `research/ADR-001-kubernetes-runtime.md`

- [ ] **ADR-002: Registry Location**
  - Decision required: Where to host container registry
  - Options:
    - Harbor on `limiting-factor` (original plan) - but node is resource-constrained
    - Harbor on `congenital-optimist` (recommended) - more resources
    - Lightweight registry (`distribution/registry`) on `limiting-factor`
    - External (Quay.io, GHCR) - simpler, but external dependency
  - For learning: **Lightweight self-hosted registry** teaches more than external
  - Document in: `research/ADR-002-registry-location.md`

- [ ] **ADR-003: Secrets Management Approach**
  - Decision required: How to handle sensitive data in GitOps workflow
  - Options:
    - SOPS + age (Nix-native, encrypts secrets in Git)
    - Sealed Secrets (Kubernetes-native, encrypts for specific cluster)
    - External Secrets Operator + local Vault (most production-like, highest complexity)
    - Plain secrets with `.gitignore` (not recommended, but simplest)
  - For learning: **SOPS + age** (integrates with Nix, teaches encryption concepts)
  - Document in: `research/ADR-003-secrets-management.md`

- [ ] **ADR-004: Storage Strategy**
  - Decision required: How to handle persistent data
  - Options:
    - NFS only from `limiting-factor` (simple, single point of failure)
    - NFS + Longhorn (distributed, but complex)
    - Local-path provisioner per node (simplest, no HA)
  - For learning: **NFS primary + local-path fallback** (teaches NFS, keeps simplicity)
  - Document in: `research/ADR-004-storage-strategy.md`

### 0.2 Network Preparation

Static IP addresses prevent DNS/connectivity issues when DHCP leases change.

- [ ] **Configure static IPs on all nodes**

  ```nix
  # grey-area: /etc/nixos/configuration.nix or modules/network/default.nix
  networking.interfaces.enp5s0.ipv4.addresses = [{
    address = "10.0.0.12";
    prefixLength = 24;
  }];
  networking.defaultGateway = "10.0.0.1";
  networking.nameservers = [ "10.0.0.1" "1.1.1.1" ];
  ```

  | Node | Interface | Static IP | Gateway |
  |------|-----------|-----------|---------|
  | grey-area | enp5s0 | 10.0.0.12 | 10.0.0.1 |
  | congenital-optimist | enp6s0 | 10.0.0.9 | 10.0.0.1 |
  | limiting-factor | enp1s0 | 10.0.0.59 | 10.0.0.1 |

- [ ] **Reserve IPs in router DHCP** (prevent conflicts with other devices)

- [ ] **Verify Tailscale mesh connectivity**
  ```bash
  # On each node
  tailscale status
  tailscale ping grey-area
  tailscale ping congenital-optimist
  tailscale ping limiting-factor
  ```

- [ ] **Document DNS resolution chain**
  - Local network: Router DNS → pi-hole (10.0.0.x)
  - Tailscale: MagicDNS (*.tailnet)
  - Kubernetes: CoreDNS (cluster.local)
  - Create: `plan-inventory/DNS_RESOLUTION_CHAIN.md`

### 0.3 Backup Infrastructure

Even in a learning environment, losing data is frustrating. Set up minimal backup before migration.

- [ ] **etcd snapshot automation**
  
  For NixOS Kubernetes module:
  ```bash
  # Manual snapshot (automate via systemd timer)
  etcdctl snapshot save /var/backup/etcd/snapshot-$(date +%Y%m%d).db
  ```
  
  Create systemd timer:
  ```nix
  systemd.services.etcd-backup = {
    description = "etcd snapshot backup";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.etcd}/bin/etcdctl snapshot save /var/backup/etcd/snapshot-$(date +%Y%m%d).db";
    };
  };
  
  systemd.timers.etcd-backup = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
  };
  ```

- [ ] **NixOS configuration backup**
  ```bash
  # Ensure home-lab repo is pushed to remote (GitHub/Forgejo)
  cd /home/geir/Projects/home-lab
  git status  # Check for uncommitted changes
  git push origin main
  ```

- [ ] **Critical data inventory**
  
  Document what data exists and where:
  | Data | Location | Backup Priority | Method |
  |------|----------|-----------------|--------|
  | NixOS configs | `/home/geir/Projects/home-lab` | Critical | Git (remote) |
  | Ollama models | `/var/lib/ollama` | Low (re-downloadable) | None |
  | Forgejo repos | TBD | High | Git mirror / restic |
  | Nextcloud files | TBD | High | Restic to NFS |
  | Personal data | `congenital-optimist:/home` | High | Restic |

- [ ] **Install restic for file-level backups**
  ```nix
  environment.systemPackages = [ pkgs.restic ];
  ```

- [ ] **Create backup location on limiting-factor**
  ```bash
  # On limiting-factor (has 3.7 TiB NVMe)
  sudo mkdir -p /mnt/backups/etcd
  sudo mkdir -p /mnt/backups/restic
  ```

### 0.4 Resource Reallocation

Address the `limiting-factor` constraint identified in the review.

- [ ] **Evaluate registry requirements**
  
  Harbor minimum requirements:
  - CPU: 2 cores
  - RAM: 4 GiB (8 GiB recommended)
  - Storage: 40 GiB+ for images
  
  `limiting-factor` has: 4 cores, 12 GiB RAM (but 2 cores reserved for K8s worker)
  
  **Decision**: Use lightweight `distribution/registry` instead of Harbor:
  - CPU: 0.5 cores
  - RAM: 512 MiB
  - Simpler to operate
  - Teaches same concepts

- [ ] **Plan node role assignments**
  
  Revised allocation:
  | Node | K8s Role | Primary Workloads | Reserved Resources |
  |------|----------|-------------------|-------------------|
  | grey-area | Control + Worker | General services, Ollama | 4 CPU, 4 GiB (control plane) |
  | congenital-optimist | Control + Worker | Heavy services, builds | 4 CPU, 4 GiB (control plane) |
  | limiting-factor | Worker | Registry, NFS, lightweight | 2 CPU, 2 GiB (system) |

- [ ] **Remove GNOME from congenital-optimist** (optional, frees ~1 GiB RAM)
  
  If desktop not needed for daily use:
  ```nix
  # Remove these from configuration.nix
  # services.xserver.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;
  
  # Or keep and accept the overhead (it's a learning lab)
  ```

### 0.5 Tooling Installation

Ensure required CLI tools are available on your workstation/admin node.

- [ ] **Kubernetes tooling**
  ```nix
  environment.systemPackages = with pkgs; [
    kubectl
    kubernetes-helm
    k9s           # TUI for K8s
    kubectx       # Context/namespace switching
    stern         # Multi-pod log tailing
  ];
  ```

- [ ] **If using Talos** (skip if NixOS module chosen)
  ```nix
  environment.systemPackages = with pkgs; [
    talosctl
  ];
  ```

- [ ] **GitOps tooling**
  ```nix
  environment.systemPackages = with pkgs; [
    argocd        # ArgoCD CLI
    sops          # Secrets encryption
    age           # Modern encryption tool
  ];
  ```

- [ ] **Container tooling**
  ```nix
  environment.systemPackages = with pkgs; [
    podman        # Container runtime
    skopeo        # Container image operations
    dive          # Analyze container images
  ];
  ```

### 0.6 Monitoring Baseline

Capture current system state for comparison after migration.

- [ ] **Document current resource usage**
  ```bash
  # On each node, save baseline
  inxi -Fxz > ~/baseline-$(hostname)-$(date +%Y%m%d).txt
  free -h >> ~/baseline-$(hostname)-$(date +%Y%m%d).txt
  df -h >> ~/baseline-$(hostname)-$(date +%Y%m%d).txt
  ```

- [ ] **Install node_exporter for Prometheus metrics**
  ```nix
  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = [ "systemd" "processes" ];
    port = 9100;
  };
  ```

- [ ] **Test metrics endpoint**
  ```bash
  curl http://localhost:9100/metrics | head -50
  ```

### 0.7 Documentation Scaffolding

Create placeholder documents for information that will be needed.

- [ ] **Create ADR directory**
  ```bash
  mkdir -p /home/geir/Projects/home-lab/machines/k8s-migration/research/adr
  ```

- [ ] **Create runbook directory**
  ```bash
  mkdir -p /home/geir/Projects/home-lab/machines/k8s-migration/runbooks
  ```

- [ ] **Create rollback procedures template**
  ```bash
  touch /home/geir/Projects/home-lab/machines/k8s-migration/runbooks/ROLLBACK_PROCEDURES.md
  ```

---

## Phase 0 Success Criteria

Before proceeding to Phase 1, verify:

| Criterion | How to Verify | Status |
|-----------|---------------|--------|
| All ADRs documented | `ls research/adr/*.md` shows 4 files | ☐ |
| Static IPs configured | `ip addr` shows expected IPs on all nodes | ☐ |
| Tailscale mesh working | `tailscale ping` succeeds between all nodes | ☐ |
| Backup location ready | `/mnt/backups` exists on limiting-factor | ☐ |
| etcd backup timer configured | `systemctl status etcd-backup.timer` | ☐ |
| NixOS configs committed | `git status` shows clean working tree | ☐ |
| kubectl installed | `kubectl version --client` | ☐ |
| node_exporter running | `curl localhost:9100/metrics` | ☐ |
| Baseline metrics captured | Baseline files exist for all 3 nodes | ☐ |

---

## Estimated Time

| Task Group | Estimated Time | Notes |
|------------|----------------|-------|
| ADR documentation | 2-4 hours | Research + writing |
| Network configuration | 1-2 hours | NixOS rebuild on 3 nodes |
| Backup setup | 2-3 hours | Create dirs, configure timers |
| Resource planning | 1 hour | Already analyzed, just document |
| Tooling installation | 30 min | NixOS rebuild |
| Monitoring baseline | 1 hour | Run commands, save output |
| Documentation scaffolding | 30 min | Create directories/files |
| **Total** | **8-12 hours** | Spread across 1-2 weeks |

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Static IP conflicts with other devices | Reserve IPs in router before changing nodes |
| Forgetting to backup before Phase 1 | Phase 1 checklist includes "verify Phase 0 complete" |
| Analysis paralysis on ADRs | Set time limit (2 hrs per ADR), accept "good enough" |
| Over-engineering backup solution | Start with manual restic, automate later |

---

## Next Steps

After completing Phase 0:

1. Review all ADR documents with fresh eyes (day after writing)
2. Run Phase 0 Success Criteria checklist
3. Create Phase 1 tracking issue/document
4. Begin Phase 1: Kubernetes Cluster Bootstrap

---

## References

- [Architecture Decision Records](https://adr.github.io/)
- [Restic backup documentation](https://restic.readthedocs.io/)
- [SOPS with age](https://github.com/getsops/sops#encrypting-using-age)
- [NixOS Kubernetes module](https://nixos.wiki/wiki/Kubernetes)
- [Docker Registry v2](https://docs.docker.com/registry/)

---

**Document Version**: 1.0  
**Created**: 2025-11-25  
**Author**: Migration planning review
