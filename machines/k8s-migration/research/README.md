# Research & Background

This folder contains research, evaluations, and background information to support the Kubernetes migration project. These are exploratory documents, technology comparisons, and detailed technical deep-dives that inform the planning decisions.

## Contents

- **[NixOS Kubernetes Module](./nix-k8s-module-research.md)** - The chosen approach
  - Deep dive into `services.kubernetes` module
  - Benefits and tradeoffs
  - Implementation patterns and examples
  - Learning resources

- **[Talos as Alternative](./talos-alternative.md)** - Reference for comparison
  - Talos in containers (sandboxed testing)
  - Distributed Talos cluster
  - When Talos might be better
  - Talos vs NixOS module tradeoffs

- **[Ingress Controllers Evaluation](./ingress-controllers-evaluation.md)** - Technology comparison
  - Traefik (recommended)
  - Nginx, HAProxy, Istio, Caddy alternatives
  - Comparison matrix
  - Integration with Tailscale

- **[Nix Container Builds](./nix-container-builds.md)** - Building containers reproducibly
  - `pkgs.dockerTools` and `pkgs.ociTools`
  - Container registry strategies
  - CI/CD integration with GitHub Actions
  - Nix flake patterns

- **[Services-OS Separation Rationale](./services-os-separation-rationale.md)** - Philosophy & benefits
  - Problems with monolithic NixOS
  - Why separate services from OS
  - Benefits and tradeoffs
  - Architecture principles

- **[Crossplane for Infrastructure](./crossplane-advanced.md)** - Advanced option for Phase 4+
  - Infrastructure as code
  - Custom resource definitions
  - When to use Crossplane
  - Self-service infrastructure patterns

## How to Use

**Starting out?** Read in this order:

1. Services-OS Separation Rationale (understand the "why")
2. NixOS Kubernetes Module Research (understand the "how")
3. Nix Container Builds (understand the tooling)

**Evaluating alternatives?**

- Ingress Controllers Evaluation (pick routing layer)
- Talos as Alternative (consider other approaches)
- Crossplane for Infrastructure (future optimization)

**Status:** ✅ All 6 research documents complete. These are living documents—update as you learn new information or evaluate different approaches.

---

**Last Updated:** 2025-11-23  
**Purpose:** Inform planning decisions and provide technical reference
