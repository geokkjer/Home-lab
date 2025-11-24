# Planning & Implementation Documents

This folder contains actionable planning documents, implementation details, and operational guides for the Kubernetes migration project.

## Contents

### Foundation & Reference

1. **[HARDWARE_NETWORKING_INVENTORY.md](./HARDWARE_NETWORKING_INVENTORY.md)** ✅ COMPLETE
   - Hardware specifications for all 3 cluster nodes
   - CPU, memory, storage, networking details (from inxi diagnostics)
   - Capacity planning and constraint analysis
   - Network topology and communication paths
   - Upgrade potential and roadmap

2. **[DNS_STRATEGY.md](./DNS_STRATEGY.md)** ✅ COMPLETE
   - Current pi-hole (Debian) deployment analysis
   - Options: keep as-is, containerize, or use BIND9
   - Hybrid approach (keep for Phase 1-2, plan K8s migration Phase 3+)
   - Blocklist/filtering strategy
   - Phase-by-phase recommendations

3. **[NETWORK_UPGRADE_PLANNING.md](./NETWORK_UPGRADE_PLANNING.md)** ✅ COMPLETE
   - Current 1 Gbps network baseline
   - 2.5G SFP+ mid-tier upgrade path
   - 10G SFP+ enterprise-grade option
   - Multi-speed hybrid approach
   - Phase 4+ networking roadmap
   - NixOS configuration examples
   - Cost and timeline estimates

## How to Use

**Getting started?**
1. Read [HARDWARE_NETWORKING_INVENTORY.md](./HARDWARE_NETWORKING_INVENTORY.md) to understand cluster constraints
2. Review [DNS_STRATEGY.md](./DNS_STRATEGY.md) for DNS options
3. Read [NETWORK_UPGRADE_PLANNING.md](./NETWORK_UPGRADE_PLANNING.md) for future networking roadmap

**Looking for specific info?**
- Cluster specs: See HARDWARE_NETWORKING_INVENTORY.md
- DNS options: See DNS_STRATEGY.md
- Network future: See NETWORK_UPGRADE_PLANNING.md

---

**Status:** Foundation documents complete (hardware, DNS, networking)  
**Last Updated:** 2025-11-23
