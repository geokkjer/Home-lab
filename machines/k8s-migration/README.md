# Kubernetes Deployment Plan for Home-Lab Cluster

**Vision**: Transform the home-lab from distributed monolithic NixOS hosts into a **unified service-OS separation architecture** across three nodes. NixOS becomes minimal infrastructure layer on each node; all services (Ollama, Forgejo, Nextcloud, Calibre-Web, etc.) run as containerized workloads in a 3-node Kubernetes cluster with centralized OCI registry, GitOps (ArgoCD), and reproducible container builds via Nix.

**Current Context**: Three machines (grey-area, congenital-optimist, limiting-factor) running monolithic NixOS with mixed services. congenital-optimist is being repurposed as a server since it's being replaced as the desktop machine. Sleeper-service is being retired, and limiting-factor is replacing it. New goal: Unified Kubernetes cluster spanning all three nodes, with OCI registry hosted on limiting-factor, and services distributed via GitOps.

- [Planning - inventory and progress](./plan-inventory/README.md)
- [Background and research](./research/README.md)