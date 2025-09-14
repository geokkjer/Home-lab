# Talos Linux Research

## What is Talos Linux?

Talos Linux is a modern, secure, immutable, and minimal operating system designed specifically for running Kubernetes. It is a container-optimized Linux distribution that reimagines Linux for distributed systems. Key features include:

- **Immutable**: The system is read-only, preventing unauthorized changes.
- **Atomic**: Updates are applied atomically, ensuring consistency.
- **Ephemeral**: No persistent state, making it easy to replace or reset.
- **Minimal**: Stripped down to essentials, reducing attack surface.
- **Secure by default**: Implements security best practices out of the box.
- **Managed via API**: Controlled through a single declarative configuration file and gRPC API.

Talos can be deployed on various platforms: containers, cloud, virtualized, and bare metal.

### Why Talos?

By being minimal, Talos offers more in terms of security, efficiency, resiliency, and consistency. It eliminates common Linux vulnerabilities and simplifies management for Kubernetes environments.

## Quickstart Guide

The quickest way to try Talos is by creating a local cluster using Docker.

### Prerequisites
- `talosctl`: CLI tool for managing Talos (install via `brew install siderolabs/tap/talosctl`)
- `kubectl`: Kubernetes CLI
- Docker installed

### Create a Local Cluster
```bash
talosctl cluster create
```

This creates a virtual cluster with control plane and worker nodes.

### Explore the Cluster
- Access the dashboard: `talosctl dashboard --nodes 10.5.0.2`
- Verify nodes: `kubectl get nodes`

### Destroy the Cluster
```bash
talosctl cluster destroy
```

## Getting Started (Full Setup)

For a production-like setup:

1. **Install talosctl**: `brew install siderolabs/tap/talosctl`
2. **Download Talos ISO**: From the Image Factory (e.g., https://factory.talos.dev/)
3. **Boot Machines**: Boot hardware or VMs with the ISO.
4. **Generate Configuration**:
   ```bash
   export CLUSTER_NAME=mycluster
   export DISK_NAME=sda  # Adjust based on your disk
   talosctl gen config $CLUSTER_NAME https://<control-plane-ip>:6443 --install-disk /dev/$DISK_NAME
   ```
   This creates `controlplane.yaml`, `worker.yaml`, and `talosconfig`.
5. **Apply Configurations**:
   - Control plane: `talosctl apply-config --insecure --nodes <cp-ip> --file controlplane.yaml`
   - Workers: Loop over worker IPs and apply `worker.yaml`
6. **Set Endpoints**: `talosctl config endpoints <cp-ip>`
7. **Bootstrap etcd**: `talosctl bootstrap --nodes <cp-ip>`
8. **Get Kubeconfig**: `talosctl kubeconfig`
9. **Verify**: `kubectl get nodes` and `talosctl health`

## Sidero Ecosystem

Sidero Labs (the creators of Talos) provides a suite of tools for managing Kubernetes clusters across environments.

### Cluster API Bootstrap Provider Talos
- **Purpose**: Bootstrap provider for Cluster API to deploy Talos-based nodes.
- **Usage**: Initialize with `clusterctl init --bootstrap talos --control-plane talos --infrastructure <provider>`
- **Features**:
  - Generates machine configurations for control plane and workers.
  - Supports configuration patches (JSON 6902 or strategic merge).
  - Integrates with infrastructure providers like Sidero (bare metal).

### Cluster API Control Plane Provider Talos
- **Purpose**: Manages the control plane for Talos clusters in Cluster API.
- **Usage**: Defines `TalosControlPlane` resources for replicas, versions, and infrastructure.
- **Features**:
  - Automates control plane scaling and upgrades.
  - Works with bootstrap provider for full cluster lifecycle.

### Terraform Provider Talos
- **Purpose**: Infrastructure as code for Talos clusters.
- **Usage**: Manage Talos configurations and clusters via Terraform.
- **Features**:
  - Declarative management of Talos machine configs.
  - Integration with Terraform workflows.

### Omni
- **Purpose**: A comprehensive Kubernetes management platform for creating and managing clusters on any environment (bare metal, cloud, virtual).
- **Key Features**:
  - Automates cluster creation, management, and upgrades.
  - Integrates with enterprise identity providers (e.g., Google, GitHub, SAML).
  - Supports multi-platform deployments.
  - Provides web UI and CLI (`omnictl`) for operations.
  - Handles machine registration, labeling, and PXE booting.
  - Includes tools like `talosctl` and `kubectl` with OIDC auth.
- **Usage**:
  - Download `omnictl` from Omni dashboard.
  - Authenticate: `omnictl get clusters` (opens browser for login).
  - Create clusters via templates or UI.
  - Manage via API or CLI for CI/CD integration.

### Other Tools
- **TalHelper**: Tool for creating Talos clusters in GitOps repositories.
- **Synology CSI Talos**: CSI driver for Synology NAS with Talos support.

This ecosystem enables seamless, secure, and automated Kubernetes deployments using Talos as the foundation.