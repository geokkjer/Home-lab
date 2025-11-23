# Services-OS Separation: Philosophy & Rationale

## Why Services-OS Separation?

### Current Problems with Monolithic NixOS

**1. Tight coupling**: OS and services in one configuration

- Upgrade Kubernetes → must rebuild entire system
- Fix Ollama bug → touches system layer
- One service breaks → risk to whole system

**2. Difficult scaling**: Hard to distribute across machines

- Services defined in NixOS config → not portable
- Each new machine needs custom config
- Duplication of effort

**3. Limited DevOps**: Manual deployments

- No GitOps: changes not version-controlled
- No CI/CD: no automated builds/deployments
- Manual kubectl apply: error-prone

**4. Single-node focus**: Design doesn't support multi-machine

- Kubernetes meant for distributed systems
- NixOS config not designed for fleet management

### Services-OS Separation Benefits

✅ **Decoupled**: OS and services can evolve independently  
✅ **Scalable**: Services portable across machines  
✅ **DevOps-native**: Full CI/CD, GitOps, reproducible deployments  
✅ **Versionable**: Every service version in Git  
✅ **Observable**: Kubernetes provides better monitoring/logging  
✅ **Standard**: Kubernetes is industry-standard (easier for others to contribute)  
✅ **Future-proof**: Ready to scale from 1 node → multi-node → cloud  

## Proposed Architecture

### Problem with Current Setup

```
Current monolithic approach:
┌─ NixOS (one giant system)
│  ├─ Ollama (system service)
│  ├─ Forgejo (system service)
│  ├─ Nextcloud (system service)
│  ├─ Jellyfin (system service)
│  └─ OS components (networking, boot, etc.)
└─ Everything fails together; hard to scale
```

### New Services-First Architecture

```
Proposed separation:
┌─ NixOS (minimal infrastructure)
│  ├─ Kubernetes runtime
│  ├─ Networking
│  ├─ Storage mounts
│  └─ System monitoring
│
└─ Kubernetes cluster (workloads)
   ├─ Pod: Ollama (v0.1.2)
   ├─ Pod: Forgejo (v1.20.5)
   ├─ Pod: Nextcloud (v29.0)
   ├─ Pod: Jellyfin (latest)
   └─ All managed via ArgoCD from Git
```

## What This Means

- **Services are version-tracked** in Git, reproducible across clusters
- **OS and services can scale independently** - upgrade Kubernetes without touching Ollama
- **Services are portable** - move workload between machines without changes
- **GitOps enables CI/CD** - push to Git → automatic deployment
- **Nix builds ensure consistency** - same container everywhere

## Strategic Vision

**Decouple services from operating system layer to achieve:**

- **Reproducibility**: Services defined via Nix containers, version-controlled in Git
- **Scalability**: Services run on Kubernetes, infrastructure independent
- **DevOps Integration**: GitOps (ArgoCD) for declarative deployments
- **Container Registry**: Centralized OCI registry for service distribution
- **OS Minimalism**: NixOS becomes lean infrastructure layer, not service orchestrator

## Technical Architecture Summary

### Layer 1: Infrastructure (NixOS)

```nix
# Minimal, lean, stable
├─ Kernel + basic OS
├─ Kubernetes runtime (NixOS services.kubernetes module)
├─ Networking (Tailscale, host networking)
├─ Storage (ZFS, NFS client)
├─ Monitoring base (Prometheus Node Exporter)
└─ SSH access

# Configuration: ~150 lines of flake.nix
# Deployment: nixos-rebuild switch
# Stability: Medium (infrastructure rarely changes)
```

### Layer 2: Container Registry

```
┌─ Quay.io / Docker Hub / Harbor (self-hosted)
├─ All service containers stored with versions
├─ Pulled by Kubernetes on demand
└─ Can be updated independently of infrastructure
```

### Layer 3: GitOps (ArgoCD)

```
┌─ Declarative deployments from Git
├─ Version-controlled Kubernetes manifests
├─ Automatic syncing (Git → Cluster)
├─ Drift detection and correction
└─ Multi-environment ready (dev/staging/prod)
```

### Layer 4: Services (Containerized)

```
Kubernetes Pods
├─ Ollama (AI inference)
├─ Forgejo (Git hosting)
├─ Nextcloud (File sync)
├─ Jellyfin (Media server)
├─ Calibre-Web (eBook server)
├─ AudioBook (Audiobook service)
└─ Custom services (as needed)

# Each service:
# - Defined as Nix package (reproducible)
# - Built to OCI container
# - Versioned and tagged
# - Deployed via ArgoCD
# - Can be replicated across nodes
# - Data persisted via K8s volumes
```

## Key Principle

Services should be:

- Version controlled
- Reproducibly built
- Declaratively deployed
- Independently scalable
- Infrastructure-agnostic

---

**Last Updated:** 2025-11-23  
**Status:** Foundation document for all planning
