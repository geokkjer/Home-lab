# Kubernetes Deployment Plan for Home-Lab Cluster

**Vision**: Transform the home-lab from distributed monolithic NixOS hosts into a **unified service-OS separation architecture** across three nodes. NixOS becomes minimal infrastructure layer on each node; all services (Ollama, Forgejo, Nextcloud, Calibre-Web, etc.) run as containerized workloads in a 3-node Kubernetes cluster with centralized OCI registry, GitOps (ArgoCD), and reproducible container builds via Nix.

**Current Context**: Three machines (grey-area, congenital-optimist, limiting-factor) running monolithic NixOS with mixed services. congenital-optimist is being repurposed as a server since it's being replaced as the desktop machine. New goal: Unified Kubernetes cluster spanning all three nodes, with OCI registry hosted on limiting-factor, and services distributed via GitOps.

**Date**: November 22, 2025  
**Scope**: 3-node Kubernetes cluster (grey-area, congenital-optimist, limiting-factor) with multi-node architecture from the start  
**Status**: Architectural planning phase for distributed cluster

---

## Executive Summary: Services-OS Separation Architecture

**Strategic Vision**: Decouple services from operating system layer to achieve:

- **Reproducibility**: Services defined via Nix containers, version-controlled in Git
- **Scalability**: Services run on Kubernetes, infrastructure independent
- **DevOps Integration**: GitOps (ArgoCD) for declarative deployments
- **Container Registry**: Centralized OCI registry for service distribution
- **OS Minimalism**: NixOS becomes lean infrastructure layer, not service orchestrator

**Architecture Stack**:

```
┌─────────────────────────────────────────────────┐
│  ArgoCD (GitOps)                                │
│  - Declarative deployments                      │
│  - Sync Git → Kubernetes                        │
│  - Multi-environment ready                      │
└──────────────┬──────────────────────────────────┘
               │
┌──────────────┴──────────────────────────────────┐
│  Kubernetes (Talos or NixOS module)             │
│  - Pod orchestration                            │
│  - Service networking                           │
│  - Resource management                          │
└──────────────┬──────────────────────────────────┘
               │
┌──────────────┴──────────────────────────────────┐
│  OCI Registry (Harbor, Quay, or self-hosted)    │
│  - Nix-built containers                         │
│  - Version tracking                             │
│  - Pull by Kubernetes                           │
│  - Hosted on `limiting-factor` to isolate        │
│    registry services and to keep the container   │
│    build pipeline declarative and reproducible  │
│    per NixOS Containers guidance                 │
└──────────────┬──────────────────────────────────┘
               │
┌──────────────┴──────────────────────────────────┐
│  Nix Container Build Pipeline                   │
│  - Ollama container                             │
│  - Forgejo container                            │
│  - Nextcloud container                          │
│  - Custom service containers                    │
│  - CI/CD integration (GitHub Actions, etc.)     │
└──────────────┬──────────────────────────────────┘
               │
┌──────────────┴──────────────────────────────────┐
│  NixOS (Minimal Infrastructure)                 │
│  - Kubernetes runtime (Talos or services.k8s)   │
│  - Networking (Tailscale, host networking)      │
│  - Storage (ZFS, NFS client)                    │
│  - SSH, Monitoring baseline                     │
└─────────────────────────────────────────────────┘
```

**Three Implementation Approaches**:

1. **Talos Distributed Cluster** - Production-ready, immutable, multi-node native (Recommended)
2. **NixOS Kubernetes Module on Each Node** - Integrated infrastructure, simpler ops but less isolation
3. **Hybrid: Talos on some nodes + NixOS module on others** - Flexibility, but operational complexity

---

## Three-Node Cluster Architecture

### Node Roles and Responsibilities

**grey-area** (Master + Worker)

- **Role**: Control plane node + general workload node
- **Hardware**: [Existing specs]
- **Kubernetes roles**: `master`, `worker`
- **Services**: Can run Ollama, Forgejo, Nextcloud, and other general services
- **Networking**: Tailscale integration for cluster mesh

**congenital-optimist** (Repurposed Server - Master + Worker)

- **Role**: Control plane node + general workload node
- **Hardware**: [Existing specs - being repurposed from desktop]
- **Kubernetes roles**: `master`, `worker`
- **Services**: Runs general services, available now that it's being replaced as desktop
- **Networking**: Joins cluster via Tailscale

**limiting-factor** (Dedicated Registry + Storage Node)

- **Role**: Worker node + dedicated registry node
- **Hardware**: [Existing specs]
- **Kubernetes roles**: `worker`
- **Services**: OCI Registry (Harbor/Quay), NFS server for persistent storage, build pipeline
- **Networking**: Tailscale backbone for registry access

### Three-Node Cluster Topology

```text
┌─────────────────────────────────────────────────────────────┐
│                   Kubernetes Cluster (3-node)               │
├──────────────────┬──────────────────┬──────────────────────┤
│                  │                  │                      │
│  grey-area       │  congenital-     │  limiting-factor     │
│  (Master+Worker) │  optimist        │  (Worker+Registry)   │
│                  │  (Master+Worker) │                      │
│  etcd replica    │  etcd replica    │  etcd replica        │
│  API server      │  API server      │                      │
│  kubelet         │  kubelet         │  kubelet             │
│                  │                  │                      │
│  Workloads:      │  Workloads:      │  Workloads:          │
│  - Ollama        │  - Forgejo       │  - Harbor Registry   │
│  - Nextcloud     │  - Cache/        │  - NFS Server        │
│  - Services      │    Build node    │  - Persistent Vol.   │
│                  │                  │    Management        │
└──────────────────┴──────────────────┴──────────────────────┘
         │                  │                  │
         └──────────────────┼──────────────────┘
                            │
                      Tailscale Mesh
                            │
         ┌──────────────────┴──────────────────┐
         │                                     │
      NFS Mount (storage)          MagicDNS (.tailnet)
         │                                     │
     sleeper-service          Service Discovery
     (external storage)
```

### High Availability Configuration

With three master nodes (one on each machine), the cluster achieves:

- ✅ **HA Etcd**: 3 replicas → survives 1 node failure
- ✅ **API Server**: 3 instances → always accessible
- ✅ **Control Plane**: No single point of failure
- ✅ **Self-healing**: Pod rescheduling if any node goes down
- ✅ **Networking**: Tailscale ensures connectivity even if one node isolated

### Quorum Requirements

```text
Node Count: 3
Quorum for etcd: 2 (⌈3/2⌉ = 2)
Min nodes for HA: 2

→ Cluster can tolerate 1 node failure and remain operational
→ All 3 nodes must sync for new writes, but reads survive node loss
→ If 2+ nodes down: cluster becomes read-only (protection against split-brain)
```

### Network Topology

All nodes connected via **Tailscale mesh**:

```text
grey-area.tailnet         congenital-optimist.tailnet    limiting-factor.tailnet
     │                            │                              │
     └────────────────────────────┼──────────────────────────────┘
                                  │
                        Tailscale Encrypted Mesh
                                  │
                         Full mesh connectivity
                         All nodes ↔ All nodes
```

Service discovery via **MagicDNS**:

- `kubernetes.default.svc.tailnet` → API server
- `registry.limiting-factor.tailnet` → Harbor registry
- `nfs.limiting-factor.tailnet` → NFS server
- `ollama.services.svc.tailnet` → Ollama service

### Data Distribution Strategy

**Control plane data** (etcd):

- Replicated across all 3 nodes
- Automatic replication + consensus (Raft)
- No manual intervention needed

**Application data** (volumes):

- Primary: NFS on limiting-factor
- Backup: Can be replicated to other nodes via Longhorn (optional)
- Database containers: Persistent volumes claim NFS mount

**Container images**:

- Stored in Harbor (limiting-factor)
- Pulled on-demand by kubelet on any node
- Cached locally after first pull

### Resource Allocation Across Cluster

**CPU Distribution**:

- grey-area: [CPU count] cores → Ollama-heavy workloads
- congenital-optimist: [CPU count] cores → General services, secondary workloads
- limiting-factor: [CPU count] cores → Registry, storage, light services

**Memory Allocation** (Kubernetes resource requests/limits):

- grey-area: [Total GB] available
- congenital-optimist: [Total GB] available
- limiting-factor: [Total GB] available (reserved for registry)

**Storage**:

- limiting-factor NFS: [Capacity] total
- Persistent volumes auto-provisioned from NFS pool

---

## Revised Architecture for Three-Node Deployment

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

### Why This Matters

- **Services are version-tracked** in Git, reproducible across clusters
- **OS and services can scale independently** - upgrade Kubernetes without touching Ollama
- **Services are portable** - move workload between machines without changes
- **GitOps enables CI/CD** - push to Git → automatic deployment
- **Nix builds ensure consistency** - same container everywhere

## Overlay Networking Strategy

**Tailscale (Tailnet) keeps traffic contained** inside the overlay while `grey-area` and the registry node (`limiting-factor`) act as peers in the Tailnet. Every Kubernetes node, build runner, and registry process stays reachable via Tailscale IPs, avoiding exposure to the wider LAN. `tailscaled` should run on each host with `--accept-dns=true` so MagicDNS can resolve service hostnames like `ollama.grey-area.tailnet` or `registry.limiting-factor.tailnet` without touching external DNS.

### Service Hostnames via MagicDNS

- Define service hostnames in Tailscale by setting each node's `tailscale up --hostname=<service>` and letting MagicDNS serve `.tailnet` names. Combine this with `tailscale serve` (or kube-proxy sidecars) so `https://olympus.grey-area.tailnet` resolves to the Ollama pod IP, `forgejo.grey-area.tailnet` hits the Forgejo service, etc.
- Use the same `.tailnet` names inside Kubernetes deployments for `ingress.hosts` or `ConfigMap` references so pods talk to each other over the overlay.
- Register `limiting-factor` as `registry.tailnet` (or similar) so GitHub Actions, ArgoCD, and Talos pods pull from the OCI registry through the tailnet.

### Reverse Proxy for Public Access

The existing Nginx reverse proxy remains the only service reachable from the public internet. Point the proxy upstreams at the Tailscale hostnames/IPs (`proxy_pass https://ollama.grey-area.tailnet`). Keep all other traffic inside Tailnet by disabling port forwarding on `grey-area`/`limiting-factor` outside of the proxy ports you need for ArgoCD/ingress. TLS termination happens at Nginx, and it forwards requests over the encrypted Tailnet link so the backend sees only authenticated traffic.

---

## Nix Container Build Strategy

### Building Containers with Nix

Instead of writing Dockerfiles, use `pkgs.dockerTools` or `pkgs.ociTools` to build reproducible OCI containers from Nix expressions:

For the registry and Kubernetes workloads we aim to keep everything declarative, so each container follows the [NixOS Containers](https://nixos.wiki/wiki/NixOS_Containers) pattern: the container definition is a NixOS module, sources stay in Git, and the `containers.<name>` attribute can be deployed by both `nixos-rebuild` on `limiting-factor` and by the Kubernetes module when building pods.

#### Example: Ollama Container

```nix
# packages/ollama-container.nix
{ pkgs, ... }:

pkgs.dockerTools.buildImage {
  name = "ollama";
  tag = "0.1.2";  # Version-controlled
  
  fromImage = pkgs.dockerTools.pullImage {
    imageName = "alpine";
    imageDigest = "sha256:...";  # Pinned base image
  };
  
  copyToRoot = pkgs.buildEnv {
    name = "root";
    paths = with pkgs; [
      ollama
      ca-certificates
    ];
  };
  
  config = {
    Cmd = [ "${pkgs.ollama}/bin/ollama" "serve" ];
    ExposedPorts."11434/tcp" = {};
    Volumes."/root/.ollama" = {};
  };
}
```

#### Example: Forgejo Container

```nix
# packages/forgejo-container.nix
{ pkgs, ... }:

pkgs.dockerTools.buildImage {
  name = "forgejo";
  tag = "1.20.5";
  
  fromImage = pkgs.dockerTools.pullImage {
    imageName = "gitea/gitea";
    imageDigest = "sha256:...";  # Use Gitea base, customize
  };
  
  copyToRoot = pkgs.buildEnv {
    name = "root";
    paths = with pkgs; [
      git
      openssh
    ];
  };
  
  config = {
    Cmd = [ "${pkgs.gitea}/bin/gitea" "web" ];
    ExposedPorts."3000/tcp" = {};
  };
}
```

### Container Registry Strategy

### Registry Node: limiting-factor

`limiting-factor` serves as the dedicated node for the OCI registry, running Docker/Podman on NixOS and exposing registry services while remaining declarative via `containers.registry = { ... }` (see the NixOS Containers guide). This keeps registry duties separate from `grey-area`, allows easier attaching of webhooks, and ensures the registry itself is reproducible and versioned.

**Option 1: Self-Hosted Harbor (Recommended)**

```yaml
# Kubernetes manifest: container-registry.yaml
apiVersion: v1
kind: Namespace
metadata:
  name: registry

---
apiVersion: helm.cattle.io/v1
kind: HelmChart
metadata:
  name: harbor
  namespace: registry
spec:
  chart: harbor
  repo: https://helm.goharbor.io
  values:
    expose:
      type: ingress
      ingress:
        hosts:
          core: registry.grey-area.local
    externalURL: https://registry.grey-area.local
    persistence:
      enabled: true
      size: 100Gi  # Adjust based on needs
```

**Option 2: Quay.io or Docker Hub (Simpler)**

```bash
# Push Nix-built containers to remote registry
nix build ./packages/ollama-container.nix
docker load -i result
docker tag ollama:0.1.2 quay.io/your-org/ollama:0.1.2
docker push quay.io/your-org/ollama:0.1.2
```

**Option 3: MinIO S3 + Container Registry**

```yaml
# Use S3-compatible storage for OCI images
# Lighter weight than Harbor
```

### CI/CD: Automated Container Builds

#### GitHub Actions Workflow

```yaml
# .github/workflows/build-containers.yaml
name: Build and Push Containers

on:
  push:
    branches: [main]
    paths:
      - 'packages/containers/**'
      - '.github/workflows/build-containers.yaml'

jobs:
  build:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Install Nix
        uses: DeterminateSystems/nix-installer-action@main
      
      - name: Build Ollama container
        run: |
          nix build ./packages/ollama-container.nix
          docker load -i result
      
      - name: Login to registry
        uses: docker/login-action@v2
        with:
          registry: quay.io
          username: ${{ secrets.REGISTRY_USER }}
          password: ${{ secrets.REGISTRY_PASSWORD }}
      
      - name: Push to registry
        run: |
          VERSION=$(nix eval --raw ./packages/ollama-container.nix -A tag)
          docker tag ollama:latest quay.io/your-org/ollama:$VERSION
          docker push quay.io/your-org/ollama:$VERSION
```

#### Nix Flake Integration

```nix
# flake.nix additions
{
  outputs = { self, nixpkgs, ... }: {
    packages.x86_64-linux = {
      ollama-container = pkgs.callPackage ./packages/ollama-container.nix {};
      forgejo-container = pkgs.callPackage ./packages/forgejo-container.nix {};
      nextcloud-container = pkgs.callPackage ./packages/nextcloud-container.nix {};
    };
    
    # Build all containers: nix build .#containers
    containers = self.packages.x86_64-linux;
  };
}
```

---

## Approach 1: Talos in Containers (Docker/Podman)

### Overview

Run Talos nodes as containers using `talosctl cluster create --container`. Talos manages its own lightweight container orchestration inside each node container.

### Architecture

```
Host (grey-area, NixOS)
└── Podman/Docker
    └── Talos Control Plane Container
        ├── Kubernetes API server
        ├── etcd
        ├── containerd runtime
        └── All Kubernetes components
```

### Benefits for Services Separation

✅ **Zero system changes**: Runs entirely in container namespace  
✅ **Quick prototyping**: `talosctl cluster create` → ready in 2-5 minutes  
✅ **Complete isolation**: Kubernetes and services run isolated from NixOS  
✅ **Easy teardown**: Delete container cluster; NixOS remains untouched  
✅ **Perfect for testing CI/CD**: Validate ArgoCD, container builds, GitOps workflows  
✅ **Container-native**: Run Nix-built containers directly in cluster  
✅ **Fast iteration**: Service changes don't require NixOS rebuild  

### Drawbacks

❌ **Performance overhead**: Extra virtualization layer (container-in-container)  
❌ **Storage concerns**: Data persistence requires explicit volume management  
❌ **Direct hardware access**: Limited GPU/NIC passthrough  
❌ **Networking complexity**: Requires port mapping and bridge networking  
❌ **Not production-grade**: Better for validation/testing phases  
❌ **Resource limits**: Bounded by container resource allocation  

### Implementation Path

```bash
# 1. Prerequisites (already available on grey-area)
# - Podman/Docker configured
# - kubectl installed

# 2. Create local cluster
talosctl cluster create --name grey-area-test --masters 1 --workers 0

# 3. Bootstrap
talosctl bootstrap --nodes 10.5.0.2

# 4. Get kubeconfig
talosctl kubeconfig

# 5. Verify
kubectl get nodes
```

### When to Use

- **Learning Kubernetes**: Safe sandbox environment
- **Development/CI/CD**: Disposable clusters for testing
- **Quick validation**: Prove workload requirements before committing
- **Multi-cluster testing**: Simulate cluster federation

### Resource Requirements

- **CPU**: 2-4 cores (1 per container node minimum)
- **RAM**: 2-4 GB per container node (control plane needs ≥2GB)
- **Storage**: 20-30 GB for container image + persistent volumes
- **Network**: localhost/private bridge network

### Data Persistence Challenge

```yaml
# Problem: Container data is lost on restart
# Solution: Mount volumes from host

# In talosctl cluster create:
talosctl cluster create --mounts /var/lib/talos-data:/var/lib/talos

# Result: Kubernetes data persists, cluster survives restarts
```

---

## Approach 2: NixOS Kubernetes Module

### Overview

Use NixOS's built-in `services.kubernetes` module to run Kubernetes directly on the host system alongside other services.

### Architecture

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

### Benefits

✅ **Full integration**: Kubernetes coexists with Ollama, Forgejo, etc.  
✅ **No performance overhead**: Runs natively on host  
✅ **Declarative configuration**: Managed via NixOS like everything else  
✅ **Direct resource access**: Full access to GPUs, storage, NICs  
✅ **Reproducible**: Entire config in flake.nix  
✅ **System consistency**: Same OS, same principles for all components  
✅ **Easier troubleshooting**: Unified logging, systemd integration  
✅ **Cost-effective**: Zero resource overhead from containerization  

### Drawbacks

❌ **Complexity**: NixOS Kubernetes module has limited documentation  
❌ **Less isolation**: Kubernetes runs with host OS access; one system break affects all  
❌ **Fewer Talos benefits**: NixOS doesn't have Talos's immutability guarantees  
❌ **Version coupling**: Kubernetes version tied to NixOS release  
❌ **Limited customization**: Module may not support all Kubernetes features  
❌ **Upgrade challenges**: Changes to system can impact cluster  
❌ **State management**: Persistent state on host makes recovery harder  
❌ **Community size**: Fewer examples/support vs. standalone Kubernetes  

### Implementation Path

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

### When to Use

- **Full system integration**: Kubernetes as one component of larger system
- **Existing NixOS setup**: Minimal additional infrastructure
- **Stable, predictable workloads**: No need for frequent cluster reset
- **Development on grey-area**: Code, test, deploy all on one machine
- **Long-term production**: Single-node should be stable

### Resource Requirements

- **CPU**: 2+ cores
- **RAM**: 4-8 GB (shared with other services)
- **Storage**: 30+ GB (Kubernetes + Ollama + other services)
- **Network**: Host network (no port mapping needed)

### Configuration Complexity

```nix
# Estimated module content needed:
# - 150-250 lines for basic 1-node setup
# - Requires knowledge of NixOS + Kubernetes fundamentals
# - Learning curve: 2-4 hours for someone familiar with both
```

---

## ArgoCD & GitOps Integration

### Why ArgoCD for Services Separation?

**ArgoCD** enables **declarative, version-controlled deployments**:

- All services and Kubernetes manifests live in Git
- Changes to services triggered by Git commits, not manual kubectl
- Automatic drift detection: if someone manually changes a pod, ArgoCD reverts it
- Perfect for CI/CD: build containers, push to registry, ArgoCD pulls and deploys

### Deployment Flow

```ascii
Git Repository (Version Control)
├─ apps/
│  ├─ ollama-deployment.yaml
│  ├─ forgejo-deployment.yaml
│  ├─ nextcloud-deployment.yaml
│  └─ kustomization.yaml
├─ nix-packages/
│  ├─ ollama-container.nix
│  ├─ forgejo-container.nix
│  └─ flake.nix
├─ ci/
│  └─ .github/workflows/build-containers.yaml
└─ README.md

        ↓ (Commit to main)

GitHub Actions
├─ Build Nix containers
├─ Push to OCI registry (quay.io, Harbor)
└─ Create pull request with updated image digests

        ↓ (PR merge or auto-merge)

ArgoCD (monitoring Git repository)
├─ Detects changes to apps/
├─ Pulls latest container images from registry
├─ Applies manifests to Kubernetes
└─ Reports deployment status

        ↓

Kubernetes
├─ Pulls container images from registry
├─ Creates/updates pods
└─ Services running with latest versions
```

### Kustomize + Forgejo GitOps Server

Organize deployments with Kustomize overlays kept in a Forgejo-hosted Git repository (`gitops.grey-area.internal` via Tailnet). The repository should expose:

- `bases/` with common manifests (namespaces, services, PVCs)
- `overlays/dev/`, `overlays/prod/` isolating environment-specific configs (replica counts, node selectors, secrets via `secretGenerator`)
- `apps/<service>` directories containing the `kustomization.yaml` that assembles the service and references images by variables managed through Forgejo webhooks or automation.

Forgejo serves as the GitOps source and can also act as a build trigger: use its internal webhook dispatchers or scheduled actions to run repository sync checks, generate new tags, and push updates via `git push` to the overlay branches that ArgoCD watches. ArgoCD pulls from `ssh://forgejo.grey-area.tailnet/grey-area/gitops.git` so the entire deployment graph lives inside the Tailnet, and ArgoCD can use `argocd proj` definitions to limit what Forgejo can change.

Forgejo also doubles as the service catalog for GitOps; each new service is a new repo or overlay, and the same Tailnet hostnames used for containers map to service URLs, keeping the workflow declarative and contained.

### ArgoCD Installation & Configuration

```yaml
# 1. Install ArgoCD namespace and controller
apiVersion: v1
kind: Namespace
metadata:
  name: argocd

---
# 2. Add ArgoCD Helm repository
apiVersion: helm.cattle.io/v1
kind: HelmChart
metadata:
  name: argocd
  namespace: argocd
spec:
  chart: argo-cd
  repo: https://argoproj.github.io/argo-helm
  values:
    server:
      insecure: true  # Or use Tailscale/TLS
    configs:
      secret:
        argocdServerAdminPassword: $2a$10$HASHED_PASSWORD  # Use 'argocd admin initial-password'

---
# 3. Create Application to deploy grey-area services
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: grey-area-services
  namespace: argocd
spec:
  project: default
  
  source:
    repoURL: https://github.com/your-org/Home-lab.git
    targetRevision: main
    path: apps/grey-area
    
  destination:
    server: https://kubernetes.default.svc
    namespace: default
  
  syncPolicy:
    automated:
      prune: true       # Delete resources removed from Git
      selfHeal: true    # Revert manual changes
    syncOptions:
      - CreateNamespace=true
```

### Example: Ollama Deployment with Image Management

```yaml
# apps/grey-area/ollama-deployment.yaml
apiVersion: v1
kind: Namespace
metadata:
  name: services

---
apiVersion: v1
kind: ConfigMap
metadata:
  name: ollama-config
  namespace: services
data:
  OLLAMA_HOST: "0.0.0.0:11434"

---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: ollama
  namespace: services
spec:
  replicas: 1
  selector:
    matchLabels:
      app: ollama
  template:
    metadata:
      labels:
        app: ollama
    spec:
      containers:
      - name: ollama
        image: quay.io/your-org/ollama:0.1.2  # Version pinned via Git
        imagePullPolicy: Always
        ports:
        - containerPort: 11434
        envFrom:
        - configMapRef:
            name: ollama-config
        resources:
          requests:
            memory: "2Gi"
            cpu: "1000m"
          limits:
            memory: "4Gi"
            cpu: "2000m"
        volumeMounts:
        - name: ollama-data
          mountPath: /root/.ollama
        - name: models
          mountPath: /root/.ollama/models
      volumes:
      - name: ollama-data
        persistentVolumeClaim:
          claimName: ollama-data
      - name: models
        nfs:
          server: sleeper-service
          path: /mnt/storage/models
          
---
apiVersion: v1
kind: Service
metadata:
  name: ollama
  namespace: services
spec:
  selector:
    app: ollama
  ports:
  - protocol: TCP
    port: 11434
    targetPort: 11434
  type: ClusterIP

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: ollama-data
  namespace: services
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 10Gi
```

### CI/CD: Automated Container Builds and Deployment

```yaml
# .github/workflows/build-and-deploy.yaml
name: Build Containers and Deploy via ArgoCD

on:
  push:
    branches: [main]
    paths:
      - 'packages/containers/**'
      - 'apps/grey-area/**'

jobs:
  build-and-push:
    runs-on: ubuntu-latest
    outputs:
      ollama-tag: ${{ steps.images.outputs.ollama-tag }}
      forgejo-tag: ${{ steps.images.outputs.forgejo-tag }}
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Install Nix
        uses: DeterminateSystems/nix-installer-action@main
      
      - name: Build all containers
        id: images
        run: |
          # Build Ollama
          nix build ./packages/ollama-container.nix
          OLLAMA_TAG=$(nix eval --raw ./packages/ollama-container.nix -A tag)
          echo "ollama-tag=$OLLAMA_TAG" >> $GITHUB_OUTPUT
          
          # Build Forgejo
          nix build ./packages/forgejo-container.nix
          FORGEJO_TAG=$(nix eval --raw ./packages/forgejo-container.nix -A tag)
          echo "forgejo-tag=$FORGEJO_TAG" >> $GITHUB_OUTPUT
          
          # Load into Docker
          docker load -i ./result-ollama
          docker load -i ./result-forgejo
      
      - name: Login to Quay.io
        uses: docker/login-action@v2
        with:
          registry: quay.io
          username: ${{ secrets.QUAY_USER }}
          password: ${{ secrets.QUAY_TOKEN }}
      
      - name: Push images
        run: |
          docker tag ollama:latest quay.io/your-org/ollama:${{ steps.images.outputs.ollama-tag }}
          docker push quay.io/your-org/ollama:${{ steps.images.outputs.ollama-tag }}
          
          docker tag forgejo:latest quay.io/your-org/forgejo:${{ steps.images.outputs.forgejo-tag }}
          docker push quay.io/your-org/forgejo:${{ steps.images.outputs.forgejo-tag }}
  
  update-manifests:
    needs: build-and-push
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Update deployment manifests
        run: |
          # Update Ollama image in deployment
          sed -i "s|quay.io/your-org/ollama:.*|quay.io/your-org/ollama:${{ needs.build-and-push.outputs.ollama-tag }}|g" \
            apps/grey-area/ollama-deployment.yaml
          
          # Update Forgejo image in deployment
          sed -i "s|quay.io/your-org/forgejo:.*|quay.io/your-org/forgejo:${{ needs.build-and-push.outputs.forgejo-tag }}|g" \
            apps/grey-area/forgejo-deployment.yaml
      
      - name: Commit and push
        run: |
          git config user.name "CI/CD Bot"
          git config user.email "ci@example.com"
          git add apps/grey-area/*.yaml
          git commit -m "chore: update container images [skip ci]"
          git push
  
  sync-argocd:
    needs: update-manifests
    runs-on: ubuntu-latest
    
    steps:
      - name: Sync ArgoCD
        run: |
          curl -X POST \
            https://argocd.grey-area.local/api/v1/applications/grey-area-services/sync \
            -H "Authorization: Bearer ${{ secrets.ARGOCD_TOKEN }}" \
            -H "Content-Type: application/json" \
            -d '{"syncStrategy":{"hook":{}}}'
```

---

## Revised Hybrid Approach: Services-OS Separation

### Phase 1: Container Cluster + Service Containerization (Weeks 1-2)

**Goal**: Validate containers + Kubernetes + ArgoCD workflow across three nodes

```text
grey-area (NixOS - control plane)
├─ Talos or services.kubernetes
│  └─ Kubernetes (1 of 3 master nodes)
│
congenital-optimist (NixOS - control plane)
├─ Talos or services.kubernetes
│  └─ Kubernetes (1 of 3 master nodes)
│
limiting-factor (NixOS - worker + registry)
├─ Talos or services.kubernetes
│  └─ Kubernetes (1 of 3 worker nodes)
└─ Harbor Registry on NFS
```

**Tasks**:

- [ ] Bootstrap Talos cluster on all three nodes (or use NixOS Kubernetes module)
- [ ] Ensure all three nodes join control plane (HA etcd with 3 replicas)
- [ ] Configure Tailscale mesh for inter-node communication
- [ ] Set up Harbor registry on limiting-factor with persistent storage
- [ ] Build first service container with Nix (Ollama)
- [ ] Push Nix-built container to Harbor registry
- [ ] Deploy container to Kubernetes (with pod affinity constraints if needed)
- [ ] Test networking between nodes via service discovery
- [ ] Verify high availability (shut down one node, cluster still running)

**Output**: 3-node cluster operational, HA validated, first service containerized

### Phase 2: ArgoCD + GitOps Integration (Weeks 3-4)

**Goal**: Full declarative, version-controlled deployments

```
Git Repository (main source of truth)
├─ all services in Nix packages/
├─ all deployments in Kubernetes manifests
└─ CI/CD pipeline on GitHub Actions

        ↓

GitHub Actions (triggered on commit)
├─ Build containers from Nix
├─ Push to registry
└─ Update manifests

        ↓

ArgoCD (monitoring Git)
├─ Automatically deploys updates
└─ Maintains desired state from Git
```

**Tasks**:

- [ ] Install ArgoCD in Kubernetes cluster
- [ ] Create GitHub repository structure for manifests
- [ ] Set up GitHub Actions workflow
- [ ] Containerize remaining NixOS services
- [ ] Deploy first service via ArgoCD
- [ ] Migrate remaining services to ArgoCD

**Output**: Full GitOps workflow; services managed via Git commits, not manual deployments

### Phase 3: Migrate Services from NixOS to Kubernetes (Weeks 5-8)

**Goal**: Move all services from NixOS to Kubernetes, minimize NixOS to infrastructure on all three nodes

```text
NixOS (thin infrastructure layer on all 3 nodes)
├─ grey-area: Kubernetes control plane + worker
├─ congenital-optimist: Kubernetes control plane + worker
├─ limiting-factor: Kubernetes worker + registry + storage
└─ All managed by system flake.nix (minimal 80 lines per node)


Kubernetes (all workloads across 3 nodes)
├─ Ollama (grey-area preferred for GPU)
├─ Forgejo (any node)
├─ Nextcloud (any node)
├─ Jellyfin (any node)
├─ Calibre-Web (any node)
└─ All managed via ArgoCD + Git
```

### Multi-Node Considerations

**Load Distribution**:

- grey-area handles Ollama (AI inference) - CPU intensive
- congenital-optimist handles general services - balanced load
- limiting-factor handles registry + storage - I/O intensive

**Pod Placement**:

- Use `nodeSelector` for resource-hungry services (Ollama → grey-area)
- Use `affinity` for load balancing (spread replicas across nodes)
- Use `taints` and `tolerations` for dedicated workloads (registry only on limiting-factor)

**Data Locality**:

- NFS mount on all nodes → pods migrate freely without data loss
- etcd replicates automatically across all masters
- Container images cached locally for faster startup

**Service Migration Order** (reduce risk by testing critical services first):

1. **Calibre-Web** (stateless, safe test)
2. **Ollama** (has data, but independent)
3. **Forgejo** (critical, has persistent data)
4. **Nextcloud** (complex, many integrations)
5. **Jellyfin** (media server, high storage)

**Tasks per service**:

- [ ] Create Nix container for service
- [ ] Test container locally
- [ ] Push to registry
- [ ] Create Kubernetes deployment manifests
- [ ] Deploy to Kubernetes
- [ ] Migrate persistent data
- [ ] Remove from NixOS configuration
- [ ] Test all integrations

**Output**: NixOS is now just infrastructure; all services run in Kubernetes

### Phase 4: Multi-Node Scaling (Month 2+)

**Goal**: Prepare for cluster expansion (future machines)

**Tasks**:

- [ ] Set up persistent storage layer (Ceph/Longhorn)
- [ ] Configure Talos for multi-node
- [ ] Implement service redundancy (2+ replicas per service)
- [ ] High availability for etcd and API server
- [ ] Load balancing across nodes
- [ ] Integrate with Omni/Sidero for automated cluster management

**Output**: Architecture ready to add bare-metal nodes, hybrid cloud deployments

---

## Phase 1: Container-Based Validation (Week 1)

### Preparation

- [ ] Document current grey-area resource usage (CPU, RAM, disk)
- [ ] Identify which current services could move to Kubernetes
- [ ] Test Podman resource allocation (ensure room for cluster)
- [ ] Plan network bridge configuration for Kubernetes pods

### Implementation

- [ ] Install/verify talosctl on grey-area
- [ ] Create Talos container cluster: `talosctl cluster create`
- [ ] Configure volume mounts for data persistence
- [ ] Bootstrap control plane: `talosctl bootstrap`
- [ ] Retrieve kubeconfig: `talosctl kubeconfig`
- [ ] Test basic kubectl commands

### Validation

- [ ] Deploy test workload (nginx, busybox)
- [ ] Test network connectivity to Ollama (on host)
- [ ] Monitor host performance impact
- [ ] Document findings in phase1-results.md
- [ ] Determine if Phase 2 (ArgoCD) is viable

### Documentation

- [ ] Create phase1-setup.md (how-to guide)
- [ ] Record performance benchmarks
- [ ] List lessons learned
- [ ] Update this document with findings

```

---

## Crossplane: Infrastructure as Code for Kubernetes

### Overview

**Crossplane** extends Kubernetes to manage cloud and on-premises infrastructure through declarative, version-controlled manifests. Instead of manually provisioning VMs, databases, or storage, define them as Kubernetes CRDs (Custom Resource Definitions) and let Crossplane reconcile reality to match.

### Why Crossplane for grey-area?

**Home-lab context**: You may eventually want to:
- Provision storage volumes dynamically (e.g., ZFS on sleeper-service)
- Manage secrets centrally (Vault via Kubernetes)
- Define infrastructure as code (same Git-driven approach as services)
- Prepare for multi-node expansion without manual provisioning

**Benefits**:

✅ **Infrastructure in Git**: All resources version-controlled, auditable  
✅ **Kubernetes-native**: Same kubectl, manifests, declarative model as services  
✅ **Automated provisioning**: Define once, Crossplane handles creation/updates  
✅ **Multi-cloud ready**: Can provision on AWS, GCP, Azure, or on-prem via providers  
✅ **GitOps-friendly**: Pairs naturally with ArgoCD for full stack declarative deployments  
✅ **Self-service**: Teams can provision resources without manual ops calls  
✅ **Reproducibility**: Exact infrastructure definition in Git, matches containers  

### Crossplane Architecture

```

Git Repository (Infrastructure as Code)
├─ xrds/ (Custom Resource Definitions)
│  ├─ database.yaml (define "PostgreSQL" as reusable resource)
│  ├─ storage.yaml (define "PersistentVolume" with ZFS backend)
│  └─ network.yaml (define "Service" networking)
│
├─ compositions/ (how to build composite resources)
│  ├─ postgres-composition.yaml (provisions VM + storage + postgres)
│  └─ storage-composition.yaml (creates ZFS volumes on sleeper-service)
│
└─ manifests/ (claim resources)
   ├─ ollama-database-claim.yaml
   ├─ forgejo-storage-claim.yaml
   └─ nextcloud-db-claim.yaml

Git commit →

Crossplane (controller in Kubernetes)
├─ Watches for new Claims (e.g., "I need a PostgreSQL database")
├─ Matches to Composition (how to build PostgreSQL)
├─ Provisions infrastructure via providers (e.g., NFS, cloud API, local)
├─ Monitors health and reconciles drift
└─ Reports status back to Kubernetes

Processing →

Infrastructure Created
├─ Database provisioned
├─ Storage volumes mounted
├─ Credentials injected to pods
└─ Entire stack reproducible from Git

```yml
### Example: Crossplane for Storage + Database

```yaml
# xrds/database.yaml - Define "PostgreSQL" as a reusable resource
apiVersion: apiextensions.crossplane.io/v1
kind: CompositeResourceDefinition
metadata:
  name: xpostgresqls.database.grey-area.internal
spec:
  group: database.grey-area.internal
  names:
    kind: XPostgreSQL
  claimNames:
    kind: PostgreSQL
  versions:
    - name: v1alpha1
      served: true
      referenceable: true
      schema:
        openAPIV3Schema:
          type: object
          properties:
            spec:
              type: object
              properties:
                parameters:
                  type: object
                  properties:
                    dbName:
                      type: string
                    dbUser:
                      type: string
                    dbSize:
                      type: string
                      default: "10Gi"
``
---
# compositions/postgres-composition.yaml - How to build PostgreSQL
apiVersion: apiextensions.crossplane.io/v1
kind: Composition
metadata:
  name: postgresql
spec:
  compositeTypeRef:
    apiVersion: database.grey-area.internal/v1alpha1
    kind: XPostgreSQL
  
  resources:
    # Provision PVC for database storage (from sleeper-service NFS)
    - name: postgresql-pvc
      base:
        apiVersion: v1
        kind: PersistentVolumeClaim
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 20Gi
      patches:
        - fromFieldPath: "spec.parameters.dbSize"
          toFieldPath: "spec.resources.requests.storage"
    
    # Deploy PostgreSQL pod
    - name: postgresql-pod
      base:
        apiVersion: apps/v1
        kind: StatefulSet
        spec:
          serviceName: postgresql
          replicas: 1
          selector:
            matchLabels:
              app: postgresql
          template:
            metadata:
              labels:
                app: postgresql
            spec:
              containers:
              - name: postgres
                image: postgres:15-alpine
                ports:
                - containerPort: 5432
                env:
                - name: POSTGRES_DB
                  valueFrom:
                    secretKeyRef:
                      name: postgres-credentials
                      key: database
                volumeMounts:
                - name: postgres-data
                  mountPath: /var/lib/postgresql/data
              volumes:
              - name: postgres-data
                persistentVolumeClaim:
                  claimName: postgresql-pvc
      patches:
        - fromFieldPath: "spec.parameters.dbName"
          toFieldPath: "spec.template.spec.containers[0].env[0].value"
    
    # Create Secret with credentials (injected to applications)
    - name: postgres-credentials
      base:
        apiVersion: v1
        kind: Secret
        type: Opaque
        stringData:
          database: defaultdb
          username: postgres
          password: changeme  # In practice, use sealed-secrets or external secret management
      patches:
        - fromFieldPath: "spec.parameters.dbName"
          toFieldPath: "stringData.database"
        - fromFieldPath: "spec.parameters.dbUser"
          toFieldPath: "stringData.username"

---
# manifests/nextcloud-database.yaml - Claim a PostgreSQL database
apiVersion: database.grey-area.internal/v1alpha1
kind: PostgreSQL
metadata:
  name: nextcloud-db
  namespace: services
spec:
  parameters:
    dbName: nextcloud
    dbUser: nextcloud-user
    dbSize: "20Gi"
```

### Crossplane Installation

```yaml
# Install via Helm
apiVersion: helm.cattle.io/v1
kind: HelmChart
metadata:
  name: crossplane
  namespace: crossplane-system
spec:
  chart: crossplane
  repo: https://charts.crossplane.io
  values:
    packageManager:
      enabled: true

---
# Install provider for local infrastructure (e.g., Helm provider for local Kubernetes)
apiVersion: pkg.crossplane.io/v1
kind: Provider
metadata:
  name: provider-kubernetes
spec:
  package: xpkg.upbound.io/upbound/provider-kubernetes:v0.10.0
  packagePullPolicy: IfNotPresent

---
# Install NFS/storage provider
apiVersion: pkg.crossplane.io/v1
kind: Provider
metadata:
  name: provider-nfs
spec:
  package: xpkg.upbound.io/upbound/provider-nfs:v0.1.0
```

### When to Use Crossplane

- **Multi-node scaling**: Define storage provisioning rules once, apply to all nodes
- **Hybrid infrastructure**: Mix Kubernetes native resources with external systems (e.g., cloud databases, local storage)
- **Self-service infrastructure**: Teams request resources via Claims, ops approves Compositions
- **GitOps at all layers**: Infrastructure + containers + application manifests all in Git
- **Future cloud migrations**: Define resources cloud-agnostically, switch providers later

### When NOT to Use Crossplane

❌ Single-node, static infrastructure (simpler to manage manually)  
❌ Team unfamiliar with Kubernetes (adds another layer of abstraction)  
❌ Rapidly changing infrastructure requirements (declarative approach less suited)  
❌ Simple use case without reusable resource patterns  

### Crossplane in Your Architecture

**For Phase 1-2**: Optional, focus on containerizing services  
**For Phase 3**: Consider adding Crossplane for storage provisioning as services scale  
**For Phase 4**: Essential for multi-node infrastructure management via GitOps  

---

## Ingress Controllers: Traefik vs Alternatives

### Overview

**Ingress controller** routes external HTTP/HTTPS traffic into Kubernetes services. It acts as a "smart reverse proxy" that:

- Terminates TLS/SSL
- Routes by hostname and path
- Load balances across service replicas
- Provides metrics and logging
- Integrates with certificate managers (Let's Encrypt, etc.)

### Why Ingress in grey-area?

Current setup has:

- Nginx reverse proxy on external interface (public-facing)
- Internal services accessible only via Tailscale

**Ingress adds**:

- Internal service routing within Kubernetes
- Centralized TLS termination
- Automatic certificate management
- Easy service discovery (no manual nginx config)
- Metrics and observability

### Traefik: Dynamic, GitOps-Friendly Ingress

#### Why Traefik?

**Traefik** is a modern, cloud-native ingress controller with excellent Kubernetes integration:

✅ **Dynamic configuration**: Updates routes from Kubernetes resources in real-time  
✅ **GitOps-ready**: Ingress defined in YAML, no separate config files  
✅ **Built-in dashboard**: Visual monitoring of routes and services  
✅ **Middleware support**: Authentication, rate limiting, request rewriting  
✅ **Let's Encrypt native**: Automatic certificate provisioning  
✅ **Lightweight**: Low resource overhead (good for single-node)  
✅ **Multi-protocol**: HTTP, HTTPS, TCP, UDP  
✅ **TLS passthrough**: Support for non-HTTPS services  

#### Traefik Installation

```yaml
# Install Traefik via Helm
apiVersion: helm.cattle.io/v1
kind: HelmChart
metadata:
  name: traefik
  namespace: traefik
spec:
  chart: traefik
  repo: https://helm.traefik.io/traefik
  values:
    # Enable dashboard
    dashboard:
      enabled: true
      ingressRoute:
        enabled: true
        entryPoints: [websecure]
        middlewares: []
    
    # Metrics
    metrics:
      prometheus:
        enabled: true
    
    # Entrypoints (ports)
    ports:
      web:
        port: 80
        expose: true
      websecure:
        port: 443
        expose: true
      traefik:
        port: 9000
    
    # Let's Encrypt configuration
    certResolvers:
      letsencrypt:
        enabled: true
        email: your-email@example.com
        storage: /data/acme.json
        httpChallenge:
          entryPoint: web
```

#### Traefik Example: Route Ollama Service

```yaml
# Deploy Ollama service (unchanged)
apiVersion: v1
kind: Service
metadata:
  name: ollama
  namespace: services
spec:
  selector:
    app: ollama
  ports:
  - protocol: TCP
    port: 11434
    targetPort: 11434

---
# Create Ingress route via Traefik
apiVersion: traefik.containo.us/v1alpha1
kind: IngressRoute
metadata:
  name: ollama-ingress
  namespace: services
spec:
  entryPoints:
    - websecure  # HTTPS only
  routes:
    - match: Host(`ollama.grey-area.local`)
      kind: Rule
      services:
        - name: ollama
          port: 11434
  tls:
    certResolver: letsencrypt
    domains:
      - main: ollama.grey-area.local
```

#### Traefik Example: Route Forgejo with Authentication

```yaml
# Middleware: Basic auth for Forgejo admin panel
apiVersion: traefik.containo.us/v1alpha1
kind: Middleware
metadata:
  name: forgejo-auth
  namespace: services
spec:
  basicAuth:
    secret: forgejo-credentials  # Pre-created secret with htpasswd

---
# IngressRoute with authentication
apiVersion: traefik.containo.us/v1alpha1
kind: IngressRoute
metadata:
  name: forgejo-ingress
  namespace: services
spec:
  entryPoints:
    - websecure
  routes:
    - match: Host(`forgejo.grey-area.local`)
      kind: Rule
      services:
        - name: forgejo
          port: 3000
      middlewares:
        - name: forgejo-auth  # Apply auth middleware
  tls:
    certResolver: letsencrypt
    domains:
      - main: forgejo.grey-area.local
```

#### Traefik Dashboard

```yaml
# Access Traefik dashboard
apiVersion: traefik.containo.us/v1alpha1
kind: IngressRoute
metadata:
  name: traefik-dashboard
  namespace: traefik
spec:
  entryPoints:
    - websecure
  routes:
    - match: Host(`traefik.grey-area.local`) && PathPrefix(`/dashboard`)
      kind: Rule
      services:
        - name: api
          port: 9000
  tls:
    certResolver: letsencrypt
    domains:
      - main: traefik.grey-area.local
```

### Alternative Ingress Controllers

#### 1. **Nginx Ingress Controller**

Standard, widely used, excellent documentation.

```yaml
# Installation
apiVersion: helm.cattle.io/v1
kind: HelmChart
metadata:
  name: nginx-ingress
  namespace: ingress-nginx
spec:
  chart: ingress-nginx
  repo: https://kubernetes.github.io/ingress-nginx

---
# Usage: Standard Kubernetes Ingress resource
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: ollama-ingress
  namespace: services
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - ollama.grey-area.local
    secretName: ollama-tls
  rules:
  - host: ollama.grey-area.local
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: ollama
            port:
              number: 11434
```

**Pros**: Familiar, stable, extensive plugins  
**Cons**: Less dynamic, more configuration overhead, less GitOps-friendly  
**Best for**: Traditional setups, teams experienced with nginx  

#### 2. **HAProxy Ingress**

High-performance, good for extreme load scenarios.

```yaml
apiVersion: helm.cattle.io/v1
kind: HelmChart
metadata:
  name: haproxy-ingress
  namespace: ingress-haproxy
spec:
  chart: haproxy-ingress
  repo: https://haproxy-ingress.github.io
```

**Pros**: High performance, advanced networking features  
**Cons**: Steeper learning curve, less Kubernetes-native  
**Best for**: Performance-critical scenarios, advanced routing rules  

#### 3. **Istio (Service Mesh Ingress)**

Advanced service mesh with traffic management, security policies, observability.

```yaml
apiVersion: helm.cattle.io/v1
kind: HelmChart
metadata:
  name: istio
  namespace: istio-system
spec:
  chart: istio-base
  repo: https://istio-release.storage.googleapis.com/charts
```

**Pros**: Powerful traffic management, security policies, observability, mTLS  
**Cons**: Heavy (30+ CRDs, 50+ resources), complex, high resource overhead  
**Best for**: Complex multi-service architectures, security-first environments  

#### 4. **Caddy**

Lightweight, automatic HTTPS, simple configuration.

```yaml
apiVersion: helm.cattle.io/v1
kind: HelmChart
metadata:
  name: caddy
  namespace: caddy
spec:
  chart: caddy
  repo: https://caddyserver.com/helm
```

**Pros**: Minimal config, automatic HTTPS, lightweight  
**Cons**: Smaller ecosystem, fewer plugins  
**Best for**: Simple setups, teams preferring ease over customization  

### Ingress Controller Comparison

| Factor | Traefik | Nginx | HAProxy | Istio | Caddy |
|--------|---------|-------|---------|-------|-------|
| **Setup Time** | 5 min | 10 min | 15 min | 30+ min | 5 min |
| **Kubernetes Native** | ✅ High | Medium | Medium | ✅ Very High | Medium |
| **Dynamic Config** | ✅ Yes | No | No | ✅ Yes | No |
| **GitOps Ready** | ✅ Yes | Partial | Partial | ✅ Yes | Partial |
| **Dashboard** | ✅ Built-in | Third-party | No | ✅ Kiali | No |
| **TLS/Certificates** | ✅ Automatic | Manual | Manual | Manual | ✅ Automatic |
| **Performance** | Good | Excellent | Excellent | Excellent | Good |
| **Resource Usage** | Low | Low | Medium | High | Very Low |
| **Learning Curve** | Low | Medium | High | Very High | Low |
| **Multi-Protocol** | ✅ HTTP/TCP/UDP | HTTP/HTTPS | All | HTTP/gRPC | HTTP/HTTPS |
| **Middleware/Plugins** | Good | Excellent | Good | Limited | Limited |
| **Production Ready** | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Yes |
| **Best For** | Cloud-native, GitOps | Traditional, stable | High-load | Complex architectures | Simple setups |

### Recommendation for grey-area

**Phase 1-3**: Use **Traefik**

- Lightweight, good for single-node
- GitOps-friendly (Ingress in YAML)
- Automatic certificate management
- Built-in dashboard for monitoring
- Smooth path to multi-node (minimal reconfiguration)

**Phase 4+**: Consider upgrading to **Istio** if:

- Need advanced traffic management (canary deployments, circuit breaking)
- Security policies required (mTLS, authorization)
- Complex multi-service interactions demand observability

### Traefik + Tailscale Integration

Keep Traefik internal (Tailnet only) for services:

```yaml
# IngressRoute: accessible only via Tailscale
apiVersion: traefik.containo.us/v1alpha1
kind: IngressRoute
metadata:
  name: ollama-tailnet
  namespace: services
spec:
  entryPoints:
    - websecure
  routes:
    - match: Host(`ollama.grey-area.tailnet`)  # Tailscale hostname
      kind: Rule
      services:
        - name: ollama
          port: 11434
```

Keep Nginx reverse proxy as the only public-facing entry point:

```nginx
# Existing reverse proxy config
upstream ollama {
    server ollama.grey-area.tailnet:443;  # Route through Traefik in Tailnet
}

server {
    listen 443 ssl;
    server_name api.example.com;
    
    location /ollama {
        proxy_pass https://ollama;
        # TLS termination happens at both Nginx and Traefik (chain of trust)
    }
}
```

---

## Next Steps

1. **Validate Services-OS Separation Vision**: Review architecture with team
2. **Set up container build infrastructure**: Nix + GitHub Actions workflows
3. **Create container registry**: Harbor or Quay.io account
4. **Begin Phase 1**: Container cluster + first service containerization
5. **Plan Phase 2**: ArgoCD installation and GitOps setup
6. **Document learnings**: Create runbooks for each service migration

---

## Comparison Matrix: Services-OS Separation Approaches

| Factor | Talos Container | NixOS Module | Services-OS Separation (Recommended) |
|--------|-----------------|--------------|---------------------------------------|
| **Setup Time** | 5 min | 30 min | Progressive (4-8 hrs Ph1) |
| **Services Separation** | Partial | No | ✅ Full |
| **OCI Registry** | Supported | Partial | ✅ Native |
| **GitOps/ArgoCD** | Supported | Partial | ✅ Native |
| **Container Builds (Nix)** | Supported | Partial | ✅ Native |
| **Performance** | 70-80% | 100% | 100% (Ph3+) |
| **Isolation** | Excellent | Poor | Controlled |
| **Scalability Path** | Limited | Limited | ✅ Unlimited |
| **Production Ready** | No | Possible | Yes (Ph2+) |
| **Learning Curve** | Low | Medium | Medium (progressive) |
| **Operational Burden** | Low | Medium | Medium→High (controllable) |
| **Multi-Node Ready** | No | No | ✅ Yes (Ph3+) |
| **Data Reproducibility** | Low | Medium | ✅ High (Git + Nix) |

---

## Why Services-OS Separation?

### Current Problems with Monolithic NixOS

1. **Tight coupling**: OS and services in one configuration
   - Upgrade Kubernetes → must rebuild entire system
   - Fix Ollama bug → touches system layer
   - One service breaks → risk to whole system

2. **Difficult scaling**: Hard to distribute across machines
   - Services defined in NixOS config → not portable
   - Each new machine needs custom config
   - Duplication of effort

3. **Limited DevOps**: Manual deployments
   - No GitOps: changes not version-controlled
   - No CI/CD: no automated builds/deployments
   - Manual kubectl apply: error-prone

4. **Single-node focus**: Design doesn't support multi-machine
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

---

## Technical Architecture Summary

### Layer 1: Infrastructure (NixOS)

```nix
# Minimal, lean, stable
├─ Kernel + basic OS
├─ Kubernetes runtime (Talos or services.kubernetes)
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

### Data Flow

```
Git Push (main branch)
    ↓
GitHub Actions (triggered on commit)
    ├─ Build containers with Nix
    ├─ Run tests
    ├─ Push to registry
    └─ Update manifests with new image digests
    ↓
Git Auto-Merge (all tests passed)
    ↓
ArgoCD (monitoring Git)
    ├─ Detects changes
    ├─ Pulls latest images from registry
    ├─ Applies new manifests
    └─ Updates Kubernetes cluster
    ↓
Kubernetes (desired state from Git)
    ├─ Creates/updates pods
    ├─ Manages networking
    ├─ Handles volume mounting
    └─ Provides service discovery
    ↓
Services Running (Latest versions from Git)
```

---

## Key Technologies

| Component | Purpose | Why This Choice |
|-----------|---------|-----------------|
| **Talos Linux** | Kubernetes OS | Minimal, immutable, Kubernetes-native |
| **Nix** | Container builds | Reproducible, hermetic builds |
| **Kubernetes** | Container orchestration | Standard, portable, scalable |
| **OCI/Docker** | Container standard | Universal, registry support |
| **ArgoCD** | GitOps deployment | Declarative, Git-driven, popular |
| **Harbor/Quay.io** | Container registry | Version management, ACLs |
| **GitHub Actions** | CI/CD pipeline | Git-native, free tier sufficient |

---

## Migration Path: Detailed Timeline for Three-Node Cluster

### Week 1: Phase 1 Validation (Bootstrap all three nodes)

- Bootstrap Talos/K8s on grey-area (control plane)
- Bootstrap Talos/K8s on congenital-optimist (control plane)
- Join limiting-factor as worker node
- Verify 3-node cluster HA (etcd quorum with 3 replicas)
- Set up Tailscale mesh between all nodes
- Container cluster running and tested

### Weeks 2-3: Phase 2 GitOps Setup (All nodes ready to deploy)

- Harbor registry operational on limiting-factor (with NFS backend)
- ArgoCD installed in cluster (high availability across nodes)
- CI/CD pipeline building Nix containers → Harbor
- Simple service (Calibre-Web) fully deployed via ArgoCD across cluster
- Team trained on multi-node workflow

### Weeks 4-8: Phase 3 Service Migration (Migrate across nodes)

- Migrate services one by one
- Test each on multi-node cluster
- Remove from NixOS on all three nodes
- Final NixOS on all nodes is just infrastructure layer
- All workloads distributed across cluster

### Month 2+: Phase 4 Optimization & Scaling

- Persistent storage layer (Longhorn/Ceph optional for redundancy)
- Multi-replica services for HA (2+ pods per service)
- Monitoring and observability across nodes (Prometheus on each node)
- Load balancing optimization

---

## Three-Node Deployment Specifics

### Node Bootstrap Order

1. **grey-area** (primary control plane)

   ```bash
   # Generate Talos config
   talosctl gen config --output-dir talos-config
   
   # Apply to grey-area
   talosctl apply --nodes grey-area
   ```

2. **congenital-optimist** (secondary control plane)

   ```bash
   # Use same config, join cluster
   talosctl apply --nodes congenital-optimist
   ```

3. **limiting-factor** (worker + registry)

   ```bash
   # Join as worker (non-control-plane node)
   talosctl apply --nodes limiting-factor --worker-nodes
   ```

### Cross-Node Service Discovery

```yaml
# Service accessible from any node via Tailscale MagicDNS
apiVersion: v1
kind: Service
metadata:
  name: ollama
  namespace: services
spec:
  selector:
    app: ollama
  ports:
  - protocol: TCP
    port: 11434
    targetPort: 11434
  type: ClusterIP

# Pod can be on any node:
# grey-area        → pod at 10.244.0.x
# congenital-optimist → pod at 10.244.1.x
# limiting-factor  → pod at 10.244.2.x
#
# All resolve to ollama.services.svc.cluster.local
```

### Shared Storage Across Nodes

```yaml
# NFS PersistentVolume (on limiting-factor)
apiVersion: v1
kind: PersistentVolume
metadata:
  name: nfs-data
spec:
  capacity:
    storage: 500Gi
  accessModes:
    - ReadWriteMany
  nfs:
    server: nfs.limiting-factor.tailnet
    path: /export/kubernetes

---
# Any pod on any node can claim it
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: ollama-data
  namespace: services
spec:
  accessModes:
    - ReadWriteMany
  resources:
    requests:
      storage: 50Gi
  volumeName: nfs-data
```

---

## Risks and Mitigations (Three-Node Cluster)

| Risk | Impact | Mitigation |
|------|--------|-----------|
| **Kubernetes learning curve** | High | Start Phase 1 early, allocate time for learning |
| **Multi-node coordination** | Medium | Use Tailscale for reliable mesh, test node failures |
| **CI/CD complexity** | Medium | Use templates, automate where possible |
| **Data loss during migration** | High | Backup all data before Phase 3, test restore |
| **Service downtime** | Medium | Implement canary deployments, health checks |
| **Container registry failure** | High | Harbor on NFS provides some redundancy |
| **Network partition** | Medium | Tailscale mesh topology, etcd quorum with 3 nodes |
| **Single node failure** | Low | 3-node cluster survives any 1 node loss |
| **Performance regression** | Medium | Benchmark each phase, compare with baseline |

---

## Success Criteria for Three-Node Deployment

### Phase 1 ✅

- [ ] All 3 nodes bootstrapped and joined cluster
- [ ] Kubernetes cluster passes health checks
- [ ] etcd quorum operational with 3 replicas
- [ ] Tailscale mesh connects all 3 nodes
- [ ] At least one service containerized with Nix
- [ ] Container pushed to Harbor registry on limiting-factor
- [ ] Pod running on any node, accessible from others via MagicDNS
- [ ] Test: Shut down one node, cluster remains operational

### Phase 2 ✅

- [ ] ArgoCD installed and HA-configured (replicates across nodes)
- [ ] GitHub Actions pipeline builds and pushes containers to Harbor
- [ ] ArgoCD auto-syncs from Git repository
- [ ] Simple service (Calibre-Web) deployed via GitOps across cluster
- [ ] NFS persistent volumes working across all nodes
- [ ] Pod can migrate between nodes without data loss
- [ ] Team confident with multi-node workflow

### Phase 3 ✅

- [ ] All services running in Kubernetes
- [ ] Data migration from NixOS → Kubernetes complete and verified
- [ ] NixOS on all 3 nodes reduced to ~150 lines (infrastructure only)
- [ ] All services updated via Git + ArgoCD
- [ ] Rollback procedures tested across nodes
- [ ] Service replicas spread across nodes for HA

### Phase 4 ✅

- [ ] Persistent storage layer configured (optional Longhorn/Ceph)
- [ ] Multi-replica services for HA (2+ pods per service)
- [ ] Monitoring and observability across all 3 nodes
- [ ] Load balancing and pod affinity working correctly
- [ ] Documentation complete with multi-node runbooks
- [ ] Team trained on multi-node operations and failure recovery

---

## References

- **Kubernetes Documentation**: <https://kubernetes.io/docs/>
- **ArgoCD Documentation**: <https://argo-cd.readthedocs.io/>
- **Talos Linux**: <https://www.talos.dev/>
- **Nix Containers**: <https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-dockerTools>
- **OCI Image Spec**: <https://github.com/opencontainers/image-spec>
- **Harbor Registry**: <https://goharbor.io/>
- **Hybrid Cluster Plan**: ./hybrid-cluster-plan.md
- **Talos Research**: ./talos-research.md

---

## Document Versioning

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 3.0 | 2025-11-22 | AI | Major update: Expanded from 1-node to 3-node cluster (grey-area, congenital-optimist, limiting-factor). Added three-node architecture, HA topology, cross-node service discovery, shared storage. Updated Phase 1-4 timeline for distributed deployment. |
| 2.0 | 2025-11-21 | AI | Major revision: Added Services-OS separation vision, Nix container builds, ArgoCD/GitOps integration, detailed 4-phase migration plan, Crossplane and Traefik sections |
| 1.0 | 2025-11-21 | AI | Initial plan: 3 approaches + hybrid recommendation |

---

**Last Updated**: 2025-11-22  
**Status**: Ready for Phase 1 implementation across three nodes  
**Architecture**: Services-OS Separation with 3-node Kubernetes cluster + GitOps  
**Next Review**: After Phase 1 completion (1-2 weeks)
