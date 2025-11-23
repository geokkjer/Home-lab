# Crossplane: Advanced Infrastructure as Code

## What is Crossplane?

Crossplane is a Kubernetes extension that allows you to provision and manage **cloud infrastructure** (VMs, databases, networking) through Kubernetes resources and controllers.

Think of it as "Terraform, but native to Kubernetes."

## Why Crossplane for This Project?

### Current Situation (Manual Infrastructure)

Today, your 3-node cluster exists physically or virtually already:

- `grey-area` (control + worker)
- `congenital-optimist` (control + worker)
- `limiting-factor` (worker + registry + storage)

Infrastructure is hand-configured, not declarative.

### Crossplane Future (Declarative Infrastructure)

With Crossplane, you could declare infrastructure entirely in Kubernetes:

```yaml
# provisions-cluster.yaml
apiVersion: compute.example.com/v1alpha1
kind: VirtualMachine
metadata:
  name: grey-area-vm
spec:
  size: large
  image: nixos-latest
  network: home-lab-net
  
---
apiVersion: storage.example.com/v1alpha1
kind: PersistentVolume
metadata:
  name: shared-storage
spec:
  size: 500Gi
  type: nfs
```

Apply with `kubectl apply -f provisions-cluster.yaml`, and Crossplane provisions the infrastructure.

## When Crossplane Makes Sense

### ✅ Good Fit

- **Multi-environment deployments** (dev, staging, prod)
- **Infrastructure as code** across multiple cloud providers
- **Rapid environment cloning** (spin up new clusters quickly)
- **CI/CD integration** with infrastructure provisioning
- **GitOps-driven infrastructure** (ArgoCD manages both apps and infrastructure)

### ❌ Probably Overkill for This Project

- **Static home lab** with fixed 3 nodes (infrastructure doesn't change often)
- **Single admin** managing one cluster (Terraform or manual config sufficient)
- **Existing physical/virtual infrastructure** already provisioned
- **Learning curve** adds complexity without immediate benefit

## Crossplane Providers

Crossplane integrates with infrastructure platforms via **providers**:

### AWS Provider

```yaml
apiVersion: ec2.aws.crossplane.io/v1beta1
kind: Instance
metadata:
  name: worker-node
spec:
  forProvider:
    imageId: ami-0c55b159cbfafe1f0
    instanceType: t3.large
    region: us-west-2
```

Provisions EC2 instances and manages their lifecycle.

### Azure Provider

```yaml
apiVersion: compute.azure.crossplane.io/v1alpha1
kind: VirtualMachine
metadata:
  name: control-plane
spec:
  forProvider:
    vmSize: Standard_D2s_v3
    osProfile:
      computerName: control-plane
```

### GCP Provider

```yaml
apiVersion: compute.gcp.crossplane.io/v1beta1
kind: Instance
metadata:
  name: worker-node
spec:
  forProvider:
    machineType: e2-standard-4
    zone: us-central1-a
```

### Libvirt Provider (Local VMs)

For home lab testing, can use Libvirt:

```yaml
apiVersion: compute.libvirt.crossplane.io/v1alpha1
kind: Domain
metadata:
  name: test-node
spec:
  forProvider:
    vcpu: 4
    memory: 8192
    diskSize: 100Gi
```

## Example: Gitops-Driven Home Lab Infrastructure

If you decide to use Crossplane later, the workflow would be:

### 1. Define Infrastructure in Git

```yaml
# infrastructure/nodes.yaml
apiVersion: libvirt.crossplane.io/v1alpha1
kind: VirtualMachine
metadata:
  name: grey-area
spec:
  forProvider:
    cpus: 8
    memory: 32Gi
    disk: 500Gi
    network: home-lab-net

---
apiVersion: libvirt.crossplane.io/v1alpha1
kind: VirtualMachine
metadata:
  name: congenital-optimist
spec:
  forProvider:
    cpus: 16
    memory: 64Gi
    disk: 1000Gi
    network: home-lab-net
```

### 2. Define Networking

```yaml
apiVersion: network.libvirt.crossplane.io/v1alpha1
kind: Network
metadata:
  name: home-lab-net
spec:
  forProvider:
    mode: route
    dhcp:
      enabled: true
      rangeStart: 192.168.42.100
      rangeEnd: 192.168.42.254
```

### 3. ArgoCD Manages It All

```yaml
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: home-lab-infrastructure
  namespace: argocd
spec:
  project: default
  source:
    repoURL: https://github.com/yourusername/home-lab
    path: infrastructure/
    targetRevision: main
  destination:
    server: https://kubernetes.default.svc
  syncPolicy:
    automated:
      prune: true
      selfHeal: true
```

Commit infrastructure changes to Git → ArgoCD applies them → Crossplane provisions → Done.

## Crossplane vs. Terraform

| Aspect | Crossplane | Terraform |
|--------|-----------|-----------|
| **Language** | Kubernetes YAML (CRDs) | HashiCorp Configuration Language (HCL) |
| **State Management** | Kubernetes etcd (automatic) | .tfstate files (manual or remote) |
| **Learning Curve** | Know Kubernetes = easy | New language to learn |
| **GitOps Integration** | Native (CRDs in Git) | Via external tools (Atlantis, etc.) |
| **Community Size** | Growing (CNCF) | Huge (industry standard) |
| **Maturity** | Early-stage | Production-proven |
| **Home Lab Fit** | 7/10 | 9/10 |

**For this project:** Terraform is more battle-tested. Crossplane is more elegant if you're deeply invested in Kubernetes.

## Crossplane Architecture

### Core Components

1. **Crossplane Manager Pod** (runs in kube-system namespace)
   - Watches for infrastructure resources (CustomResourceDefinitions)
   - Reconciles desired state with actual state
   - Manages lifecycle and cleanup

2. **Providers** (installable extensions)
   - AWS, Azure, GCP, Libvirt, etc.
   - Each provider adds its own CRDs and controllers

3. **Claims** (user-facing abstractions)
   - Higher-level resources that hide implementation details
   - Example: `XDatabase` claim that can provision RDS, CloudSQL, or Azure Database depending on provider

## Example: Database Claim

Instead of exposing cloud-specific database resources, Crossplane lets you define **claims**:

```yaml
apiVersion: database.example.com/v1alpha1
kind: XPostgreSQLInstance
metadata:
  name: app-db
spec:
  size: large
  region: us-west-2
  version: "14"
  
---
# Crossplane converts this to cloud-specific resource
# If AWS: → RDS PostgreSQL
# If GCP: → Cloud SQL PostgreSQL
# If Azure: → Azure Database for PostgreSQL
```

This provides **portability** across cloud providers.

## Decision for This Project

### Now (Phase 1-2)

**Don't use Crossplane yet.** Reasons:

- Infrastructure is static (3 fixed nodes)
- Manual provisioning / NixOS declarative config is sufficient
- Single admin doesn't need multi-environment complexity
- Added cognitive load without immediate benefit

### Later (Phase 3-4)

**Consider Crossplane if:**

- You add more clusters (staging, disaster recovery)
- You adopt multi-cloud deployment
- You want 100% GitOps (infrastructure + services)
- Team grows and needs automated provisioning

### Terraform as Middle Ground

If you want infrastructure-as-code without Crossplane:

```hcl
# terraform/machines.tf
resource "libvirt_domain" "grey_area" {
  name   = "grey-area"
  vcpu   = 8
  memory = 32768
  disk {
    volume_id = libvirt_volume.grey_area.id
  }
}

resource "libvirt_domain" "congenital_optimist" {
  name   = "congenital-optimist"
  vcpu   = 16
  memory = 65536
  disk {
    volume_id = libvirt_volume.congenital_optimist.id
  }
}
```

Then `terraform apply` manages infrastructure, `kubectl apply` manages services.

## Crossplane Learning Resources

If you decide to explore later:

- [Crossplane Official Docs](https://crossplane.io)
- [Crossplane Architecture Deep Dive](https://crossplane.io/docs/v1.11/concepts/architecture.html)
- [AWS Provider Docs](https://marketplace.upbound.io/providers/upbound/provider-aws/)
- [Composable Infrastructure Patterns](https://crossplane.io/docs/v1.11/concepts/composition.html)

## Conclusion

**Crossplane is powerful but overkill for current home lab scope.** Keep it in mind for future scaling:

- **Phase 1-2:** NixOS + kubectl for infrastructure and services
- **Phase 3-4:** Consider Crossplane if managing multiple clusters or multi-cloud

For now, focus on getting 3-node K8s cluster stable with ArgoCD managing services. Infrastructure remains declaratively managed via NixOS flakes and `nixos-rebuild`.

---

**Last Updated:** 2025-11-23  
**Status:** Research for future consideration, not required for Phase 1  
**See Also:** [services-os-separation-rationale.md](./services-os-separation-rationale.md), [nix-k8s-module-research.md](./nix-k8s-module-research.md)
