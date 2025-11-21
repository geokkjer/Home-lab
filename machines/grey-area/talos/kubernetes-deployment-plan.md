# Kubernetes Deployment Plan for grey-area (Single Node)

**Vision**: Transform grey-area from a monolithic NixOS host with mixed services into a **service-OS separation architecture**. NixOS becomes minimal infrastructure layer; all services (Ollama, Forgejo, Nextcloud, Calibre-Web, etc.) run as containerized workloads in Kubernetes with OCI registry, GitOps (ArgoCD), and reproducible container builds via Nix.

**Current Context**: grey-area is a NixOS-based AI processing hub with Ollama, Forgejo, Nextcloud, and other containerized services. New goal: Separate services from OS using Kubernetes + OCI registry + Nix containers + GitOps/ArgoCD.

**Date**: November 21, 2025  
**Scope**: 1-node Kubernetes cluster → multi-node ready architecture  
**Status**: Architectural planning phase

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

1. **Talos in Containers** - Sandbox for validation, fast prototyping
2. **NixOS Kubernetes Module** - Integrated infrastructure layer
3. **Hybrid + Services Separation** - Recommended for long-term architecture

---

## Services-OS Separation: Core Concept

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

---

## Nix Container Build Strategy

### Building Containers with Nix

Instead of writing Dockerfiles, use `pkgs.dockerTools` or `pkgs.ociTools` to build reproducible OCI containers from Nix expressions:

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

```
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

**Goal**: Validate containers + Kubernetes + ArgoCD workflow

```
grey-area (NixOS - unchanged)
├─ Podman (runs Talos cluster)
│  └─ Talos Control Plane Container
│     ├─ Kubernetes API
│     └─ Container runtime
│
└─ Services still on NixOS (not yet containerized)
   ├─ Ollama (original service)
   ├─ Forgejo (original service)
   └─ Nextcloud (original service)
```

**Tasks**:
- [ ] Create container cluster: `talosctl cluster create`
- [ ] Build first service container with Nix (Ollama)
- [ ] Set up container registry (Harbor or Quay.io)
- [ ] Push Nix-built container to registry
- [ ] Deploy container to Kubernetes manually
- [ ] Test networking between host services and container workloads

**Output**: Proof that containerization + Kubernetes works, validated CI/CD approach

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

**Goal**: Move all services from NixOS to Kubernetes, minimize NixOS to infrastructure

```
NixOS (thin infrastructure layer)
├─ Kubernetes runtime
├─ Networking
├─ Storage mounts
├─ SSH access
└─ System monitoring

        ↑
        │
        └─ Managed by system flake.nix (minimal 80 lines)


Kubernetes (all workloads)
├─ Ollama
├─ Forgejo
├─ Nextcloud
├─ Jellyfin
├─ Calibre-Web
└─ All managed via ArgoCD + Git
```

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

## Migration Path: Detailed Timeline

### Week 1: Phase 1 Validation
- Container cluster running
- First service (Ollama) containerized
- Basic Kubernetes operations understood
- Registry set up

### Weeks 2-3: Phase 2 GitOps Setup
- ArgoCD installed and configured
- CI/CD pipeline working (Nix → container → registry → ArgoCD)
- Simple service (Calibre-Web) fully deployed via ArgoCD
- Team trained on workflow

### Weeks 4-8: Phase 3 Service Migration
- Migrate services one by one
- Test each in Kubernetes
- Remove from NixOS
- Final NixOS is just infrastructure

### Month 2+: Phase 4 Multi-Node Ready
- Persistent storage layer
- HA configuration
- Omni/Sidero integration
- Ready to add nodes

---

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|-----------|
| **Kubernetes learning curve** | High | Start Phase 1 early, allocate time for learning |
| **CI/CD complexity** | Medium | Use templates, automate where possible |
| **Data loss during migration** | High | Backup all data before Phase 3, test restore |
| **Service downtime** | Medium | Implement canary deployments, health checks |
| **Container registry failure** | High | Multi-registry setup, local fallback |
| **Performance regression** | Medium | Benchmark each phase, compare with baseline |
| **Networking issues** | Medium | Careful planning of pod networking, service discovery |

---

## Success Criteria

### Phase 1 ✅
- [ ] Kubernetes cluster created and running
- [ ] At least one service containerized with Nix
- [ ] Container pushed to registry
- [ ] Performance impact measured and acceptable
- [ ] Team understands basics

### Phase 2 ✅
- [ ] ArgoCD installed and operational
- [ ] GitHub Actions pipeline builds and pushes containers
- [ ] ArgoCD auto-syncs from Git
- [ ] Simple service deployed via GitOps
- [ ] Team confident with workflow

### Phase 3 ✅
- [ ] All services running in Kubernetes
- [ ] Data migration complete and verified
- [ ] NixOS reduced to ~150 lines (infrastructure only)
- [ ] All services updated via Git + ArgoCD
- [ ] Rollback procedures tested

### Phase 4 ✅
- [ ] Persistent storage configured
- [ ] Architecture ready for multi-node
- [ ] Documentation complete
- [ ] Team trained on operations

---

## References

- **Kubernetes Documentation**: https://kubernetes.io/docs/
- **ArgoCD Documentation**: https://argo-cd.readthedocs.io/
- **Talos Linux**: https://www.talos.dev/
- **Nix Containers**: https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-dockerTools
- **OCI Image Spec**: https://github.com/opencontainers/image-spec
- **Harbor Registry**: https://goharbor.io/
- **Hybrid Cluster Plan**: ./hybrid-cluster-plan.md
- **Talos Research**: ./talos-research.md

---

## Document Versioning

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 2.0 | 2025-11-21 | AI | Major revision: Added Services-OS separation vision, Nix container builds, ArgoCD/GitOps integration, detailed 4-phase migration plan |
| 1.0 | 2025-11-21 | AI | Initial plan: 3 approaches + hybrid recommendation |

---

**Last Updated**: 2025-11-21  
**Status**: Ready for review and Phase 1 implementation  
**Architecture**: Services-OS Separation with GitOps  
**Next Review**: After Phase 1 completion (1-2 weeks)
