<file_path>
home-lab/machines/grey-area/talos/hybrid-cluster-plan.md
</file_path>

<edit_description>
Creating a reference MD file for the hybrid bare metal + cloud cluster plan
</edit_description>

# Hybrid Bare Metal + Cloud Cluster Plan with Talos Linux

## Overview

This plan outlines setting up a hybrid Kubernetes cluster using Talos Linux, combining bare metal machines (e.g., in your home lab at `/home/geir/Projects/home-lab/machines/grey-area/talos/`) with cloud instances from a European provider. The cluster will be connected via VPN for secure, unified management. This setup leverages Talos's platform-agnostic design and Omni for multi-environment orchestration.

**Difficulty**: Moderate (6/10)  
**Time Estimate**: 2-4 hours + VPN setup  
**Why This Setup?**: Combines local hardware resilience with cloud scalability, ideal for home labs expanding to production-like environments.

## European Cloud Providers Supporting Talos VMs

No major European cloud provider offers pre-built "Talos VMs" out of the box, as Talos is a custom OS. However, most support custom images, allowing you to upload or create Talos-based VMs. Here's a breakdown:

### Recommended Providers

1. **Hetzner (Germany)**:
   - **Why?**: Affordable, European-based, good for small/medium clusters. Supports custom images via ISO upload or snapshot.
   - **Talos Support**: Upload Talos ISO to create VMs. Use their API for automation.
   - **Pricing**: Starts at ~€3/month for basic VMs.
   - **Ease**: High – Simple UI and API.

2. **Scaleway (France)**:
   - **Why?**: French provider with good European coverage. Supports custom images and snapshots.
   - **Talos Support**: Import Talos images via their image builder or API.
   - **Pricing**: ~€3-5/month for entry-level.
   - **Ease**: Medium – API-friendly for automation.

3. **OVHcloud (France/Multiple EU locations)**:
   - **Why?**: Broad European presence (France, Germany, UK). Supports custom ISOs and images.
   - **Talos Support**: Upload Talos ISO for bare metal-like VMs.
   - **Pricing**: Competitive, ~€3-10/month.
   - **Ease**: Medium – Robust for larger setups.

4. **AWS (Ireland/Frankfurt)**:
   - **Why?**: Global leader with strong EU presence. Highly scalable.
   - **Talos Support**: Import Talos images using EC2 Import/Export or VM Import. Omni integrates well.
   - **Pricing**: Pay-as-you-go, ~€5-20/month for small instances.
   - **Ease**: Medium – Powerful but requires API knowledge.

5. **Azure (Netherlands/Germany)**:
   - **Why?**: Microsoft's EU regions. Supports custom images.
   - **Talos Support**: Create custom images from Talos ISOs.
   - **Pricing**: ~€5-15/month.
   - **Ease**: Medium – Good for enterprise features.

6. **Google Cloud (Belgium/Netherlands)**:
   - **Why?**: Reliable EU regions. Supports custom images.
   - **Talos Support**: Import Talos images via Compute Engine.
   - **Pricing**: ~€5-20/month.
   - **Ease**: Medium – Advanced networking options.

**Recommendation**: Start with **Hetzner** or **Scaleway** for simplicity and cost in a home lab. Use Omni to automate image creation and deployment across providers.

## Prerequisites

1. **Hardware/Access**:
   - Bare metal: Machines in your home lab (e.g., boot with Talos ISO from <https://factory.talos.dev/>).
   - Cloud: 1-2 VMs from chosen provider (e.g., Hetzner CX11 instances).

2. **Networking/VPN**:
   - VPN: WireGuard, OpenVPN, or Tailscale for secure home-to-cloud connection.
   - Ensure machines have static/VPN IPs.

3. **Software/Tools**:
   - `talosctl`: `brew install siderolabs/tap/talosctl`
   - `omnictl`: Download from Omni dashboard.
   - `kubectl`: For cluster management.
   - Cloud CLI: e.g., `hcloud` for Hetzner.
   - Omni account: Free tier for testing.

4. **Accounts**:
   - Cloud provider account with API access.
   - Omni setup (hosted or self-hosted).

## Step-by-Step Plan

### 1. Set Up VPN

- Install VPN server on a cloud VM (e.g., WireGuard on Hetzner).
- Configure clients on bare metal machines.
- Test: Ping between environments.

### 2. Prepare Machines

- **Bare Metal**: Boot with Talos ISO, note IPs.
- **Cloud**: Launch VMs, upload Talos image (use Omni's image factory for custom builds).

### 3. Register in Omni

- Use Omni UI/CLI to add machines: `omnictl machine set-labels <id> environment=home-lab`
- Label cloud machines similarly.

### 4. Create Cluster Template

Save this as `hybrid-cluster.yaml` in `/home/geir/Projects/home-lab/machines/grey-area/talos/`:

```yaml
kind: Cluster
name: hybrid-cluster
kubernetes:
  version: v1.29.1
talos:
  version: v1.11.0
---
kind: ControlPlane
machines:
  - <bare-metal-uuid>  # Home lab machine
---
kind: Workers
machines:
  - <cloud-uuid>  # Cloud machine
```

### 5. Deploy Cluster

- Sync: `omnictl cluster template sync --file hybrid-cluster.yaml`
- Monitor in Omni UI.

### 6. Verify

- Get kubeconfig: `omnictl cluster kubeconfig hybrid-cluster > kubeconfig.yaml`
- Check: `kubectl --kubeconfig kubeconfig.yaml get nodes`

## Challenges & Solutions

- **VPN Issues**: Use Tailscale for easy setup.
- **Image Upload**: Follow provider docs (e.g., Hetzner's ISO upload).
- **Costs**: Monitor usage; use free tiers.
- **Debugging**: Use `talosctl logs` and Omni logs.

## Next Steps

- Test with 1 bare metal + 1 cloud VM.
- Integrate with GitOps (ArgoCD via Omni).
- Expand to more nodes as needed.

For updates, refer to Omni docs: <https://omni.siderolabs.com/>
