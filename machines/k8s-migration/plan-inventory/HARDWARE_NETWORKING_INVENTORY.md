# Hardware & Networking Inventory

A comprehensive repository of hardware specifications, networking configuration, and capacity metrics for the home lab Kubernetes cluster. Updated from live system diagnostics (inxi v7) on each node.

**Last Updated:** 2025-11-23  
**Cluster Status:** 3-node, all healthy

---

## Cluster Overview

| Machine | Role | CPU | RAM | Storage | Network |
|---------|------|-----|-----|---------|---------|
| **grey-area** | Control + Worker | Intel Xeon E5-2670 v3 (12c/24t) | 32 GiB DDR4 | 224 GiB SSD | 1GbE + Tailscale |
| **congenital-optimist** | Control + Worker | AMD Ryzen Threadripper 1920X (12c/24t) | 64 GiB DDR4 | 9.4 TiB mixed | 1GbE + Tailscale |
| **limiting-factor** | Worker + Registry + Storage | Intel N150 (4c) | 12 GiB LPDDR5 | 3.78 TiB mixed | 1GbE + Tailscale |

---

## grey-area: Control Plane + Worker Node

### System Specs

```
Hostname:      grey-area
Kernel:        Linux 6.12.58 (x86_64, 64-bit)
Distro:        NixOS 25.05 (Warbler)
Uptime:        2d 1h 23m
```

### CPU

| Property | Value |
|----------|-------|
| **Model** | Intel Xeon E5-2670 v3 (Haswell generation) |
| **Cores/Threads** | 12 cores / 24 threads |
| **Base/Boost** | 1.2 GHz / 3.1 GHz |
| **Cache** | L1: 768 KiB, L2: 3 MiB, L3: 30 MiB |
| **TDP** | ~130W (estimated) |
| **CPU Flags** | AVX2, TSX, AES-NI, EPT, VT-x enabled |

**Kubernetes Relevance:**
- Production-grade server CPU (not consumer-grade)
- Good for control plane tasks (API server, etcd, controller manager)
- Sufficient for moderate worker workloads
- Low power consumption compared to Threadripper

### Memory

| Property | Value |
|----------|-------|
| **Total** | 32 GiB DDR4 |
| **Available** | 31.24 GiB |
| **Used** | 2.99 GiB (9.6%) |
| **Configuration** | 4 x 8 GiB DDR4, spec 2667 MT/s, running 2133 MT/s |
| **Max Capacity** | 128 GiB (4 DIMM slots) |

**Kubernetes Relevance:**
- Sufficient for control plane (16 GiB minimum recommended)
- Headroom for typical workloads
- Can expand if needed (64 GiB total possible)

### Storage

| Device | Size | Type | Speed | Usage | Mount |
|--------|------|------|-------|-------|-------|
| `/dev/sda` | 224 GiB | SSD | SATA 6.0 Gb/s | System root | `/` |

**Filesystem:**
- Root: ext4 (218 GiB, 36.8% used)
- Boot: vfat (548 MiB, 20.9% used)

**Storage Details:**
- Kingston SHPM2280P2H 240G
- Single SATA SSD (no redundancy)
- Firmware: L5TP
- Speed: SATA 6.0 Gb/s limit (not NVMe)

**Kubernetes Relevance:**
- Limited storage for persistent volumes
- No redundancy (single drive, single failure point)
- Consider remote storage mounts for critical data
- Suitable for control plane (lightweight data)

### Networking

| Interface | Status | Speed | IP (IPv4) | IP (IPv6) | MAC |
|-----------|--------|-------|-----------|-----------|-----|
| `enp5s0` | UP | 1 Gbps | 10.0.0.12/24 | 2001:4643:ac42::... | 22:15:5c:05:30:2e |
| `tailscale0` | UP | VPN | 100.119.86.92/32 | fd7a:115c:a1e0::7401:565c | N/A |

**Network Configuration:**
- Single Gigabit Ethernet (enp5s0)
- DHCP: 10.0.0.12 on home network
- Gateway: 10.0.0.0/24 (standard home net)
- IPv6: Auto-configured with IPv6 ISP allocation
- Tailscale: Mesh VPN overlay, peer-to-peer encrypted

**Kubernetes Relevance:**
- Primary node communication via local 10.0.0.x network
- 1 Gbps link suitable for 3-node cluster
- Tailscale provides secure inter-node tunnel
- Consider QoS if latency-sensitive workloads added

### Power & Thermal

| Metric | Value |
|--------|-------|
| **CPU Temperature** | 26.0°C (idle) |
| **Fan Speed** | N/A (server class, likely managed) |
| **Power State** | Running (can suspend to mem/disk) |

**Kubernetes Relevance:**
- Low thermal load
- Headroom for increased workloads
- Good stability indicator

### OS & Software

- **Init System:** systemd
- **Packages:** 1053 (nix-sys)
- **Compilers:** gcc 14.3.0
- **Shell:** bash 5.2.37

---

## congenital-optimist: Control Plane + Worker Node

### System Specs

```
Hostname:      congenital-optimist
Kernel:        Linux 6.12.58 (x86_64, 64-bit)
Distro:        NixOS 25.05 (Warbler)
Display:       X.org 1.21.1, GNOME Shell (interactive desktop)
Uptime:        5h 3m
```

### CPU

| Property | Value |
|----------|-------|
| **Model** | AMD Ryzen Threadripper 1920X |
| **Cores/Threads** | 12 cores / 24 threads |
| **Base/Boost** | 3.5 GHz / 3.5 GHz (consistent) |
| **Cache** | L1: 1.1 MiB, L2: 6 MiB, L3: 32 MiB |
| **TDP** | ~180W |
| **CPU Flags** | AVX2, SVM (AMD-V), AES-NI, all modern extensions |

**Kubernetes Relevance:**
- High-performance CPU (consumer workstation, not server)
- Excellent for compute-intensive workloads
- Consistent boost clock (better for workloads needing sustained performance)
- Lower overall efficiency than Xeon in terms of power/performance
- Good for container builds and intensive operations

### Memory

| Property | Value |
|----------|-------|
| **Total** | 64 GiB DDR4 |
| **Available** | 62.68 GiB |
| **Used** | 30.59 GiB (48.8%) |
| **Configuration** | 4 x 16 GiB DDR4, speed unspecified (running ~3000 MT/s) |
| **Max Capacity** | 512 GiB (8 DIMM slots, 64 GiB max per slot) |

**Current Usage Breakdown:**
- System: ~1-2 GiB
- Applications/Services: ~28 GiB
- Available headroom: 30+ GiB

**Kubernetes Relevance:**
- Excellent memory capacity for workloads
- Can run substantial stateful services
- Headroom for buffer caches and pod swapping
- Could expand to 256+ GiB if needed

### Storage

| Device | Size | Type | Speed | Purpose | FS |
|--------|------|------|-------|---------|-----|
| `/dev/nvme0n1` | 238 GiB | NVMe SSD | NVMe 31.6 Gb/s (4 lanes) | System root | ZFS (zpool) |
| `/dev/nvme1n1` | 954 GiB | NVMe SSD | NVMe 31.6 Gb/s (4 lanes) | RAID mirror (stuffpool) | ZFS |
| `/dev/nvme2n1` | 954 GiB | NVMe SSD | NVMe 31.6 Gb/s (4 lanes) | RAID mirror (stuffpool) | ZFS |
| `/dev/sda` | 7.28 TiB | HDD | SATA 6.0 Gb/s | Media storage | ZFS (zpool) |

**Filesystem Layout:**
- `zpool/root`: 137 GiB (1.7% used)
- `zpool/home`: 187 GiB (28% used)
- `zpool/nix`: 160 GiB (15.7% used)
- `zpool/var`: 149 GiB (9.3% used)
- `stuffpool`: 922 GiB mirror (872 GiB free)
- HDD: 7.28 TiB XFS (media, backup)

**Storage Specs:**
- NVMe: A-Data SX8200PNP (nvme0), Intel 660p (nvme1, nvme2)
- HDD: Seagate ST8000AS0002 (5980 rpm)
- RAID: ZFS mirror (nvme1+nvme2 → 922 GiB usable)
- Total raw: 9.37 TiB, usable: 8.4 TiB

**Kubernetes Relevance:**
- Excellent local storage for persistent volumes
- RAID mirror provides redundancy for critical data
- NVMe speed allows fast pod startup and I/O
- HDD good for backup/archive storage
- Could serve as local storage node for K8s

### Networking

| Interface | Status | Speed | IP (IPv4) | IP (IPv6) | MAC |
|-----------|--------|-------|-----------|-----------|-----|
| `enp4s0` | DOWN | — | — | — | 70:85:c2:ce:b3:a2 |
| `enp6s0` | UP | 1 Gbps | 10.0.0.9/24 | auto-config | 70:85:c2:ce:b3:a0 |
| `tailscale0` | UP | VPN | 100.109.28.53/32 | fd7a:115c:a1e0::43ad:1c35 | N/A |
| `virbr0` (libvirt) | DOWN | — | 192.168.122.1 | — | 52:54:00:83:60:f3 |
| `virbr1` (libvirt) | DOWN | — | 192.168.100.1 | — | 52:54:00:2e:85:2c |
| `wlp5s0` (WiFi) | DOWN | — | — | — | 72:bd:3f:db:d2:22 |

**Network Configuration:**
- Primary: `enp6s0` on 10.0.0.9/24 (active)
- Secondary: `enp4s0` available but down (potential for bonding/redundancy)
- Libvirt bridges for VM/container networking (currently unused)
- WiFi adapter available (disabled)

**Kubernetes Relevance:**
- Dual NIC capability (enp4s0 available for secondary network if needed)
- 1 Gbps adequate for cluster traffic
- Libvirt infrastructure available for testing
- WiFi available if remote/wireless connectivity needed

### Power & Thermal

| Metric | Value |
|--------|-------|
| **CPU Temperature** | 45.5°C (under load) |
| **GPU (AMD) Temp** | 53.0°C (integrated GPU) |
| **Memory Temp** | 56.0°C |
| **GPU Fan Speed** | 1279 RPM |

**Kubernetes Relevance:**
- Thermal headroom (CPU <60°C, memory <60°C)
- GPU running (integrated graphics support)
- Stable temperature profile for sustained workloads

### OS & Software

- **Init System:** systemd
- **Display Server:** X.org 1.21.1 + GNOME Shell 48.0
- **Packages:** 2271 (2249 nix-sys, 22 flatpak)
- **Compilers:** gcc 14.3.0

**Note:** This machine has an interactive desktop (unusual for a server node). Consider if this should be headless for dedicated K8s work, or if desktop environment is intentional for development.

---

## limiting-factor: Worker Node + Registry + Storage

### System Specs

```
Hostname:      limiting-factor
Kernel:        Linux 6.12.58 (x86_64, 64-bit)
Distro:        NixOS 25.05 (Warbler)
Uptime:        0h 24m (recently booted)
```

### CPU

| Property | Value |
|----------|-------|
| **Model** | Intel N150 (Alder Lake Atom/Pentium line) |
| **Cores/Threads** | 4 cores (SMT unsupported) |
| **Base/Boost** | 700 MHz / 3.6 GHz (turbo) |
| **Current** | 858 MHz average (idle) |
| **Cache** | L1: 384 KiB, L2: 2 MiB, L3: 6 MiB |
| **TDP** | ~6W (ultra-low power) |
| **CPU Flags** | AVX-512, TSX, AES-NI, Intel PT, all modern extensions |

**Kubernetes Relevance:**
- Low-power consumer CPU (not server-grade)
- Adequate for lightweight workloads (registry, metadata services)
- Power-efficient (great for always-on infrastructure)
- Single worker node constraint (only 4 cores)
- Turbo boost helps with spiky workloads
- May throttle under sustained load

### Memory

| Property | Value |
|----------|-------|
| **Total** | 12 GiB LPDDR5 |
| **Available** | 11.45 GiB |
| **Used** | 687 MiB (5.9%) |
| **Configuration** | 4 x 3 GiB LPDDR5 (integrated into mobo) |
| **Max Capacity** | 12 GiB (soldered, not expandable) |

**Kubernetes Relevance:**
- Tight memory budget (12 GiB total)
- 10+ GiB available for workloads
- No upgrade path (soldered memory)
- Suitable for lightweight services (registry, databases)
- Limited buffer for memory spikes

### Storage

| Device | Size | Type | Speed | Purpose |
|--------|------|------|-------|---------|
| `/dev/mmcblk0` | 58 GiB | eMMC SSD | ~200-300 MB/s | System root |
| `/dev/nvme0n1` | 1.86 TiB | NVMe SSD | NVMe 31.6 Gb/s (4 lanes) | Data storage |
| `/dev/nvme1n1` | 1.86 TiB | NVMe SSD | NVMe 31.6 Gb/s (4 lanes) | Data storage |

**Filesystem Layout:**
- Root: ext4 (56.5 GiB, 16% used)
- Boot: vfat (511 MiB, 14.3% used)
- NVMe drives: unformatted (ready for use)

**Storage Specs:**
- eMMC: DV4064, soldered storage
- NVMe: CUSU CV3500Q (enterprise-grade, 2TB each)
- Total: 3.78 TiB raw capacity

**Kubernetes Relevance:**
- Good local storage for container registry
- NVMe SSDs provide fast image pull/push
- eMMC system drive adequate for OS
- Could configure RAID across both NVMe drives
- Suitable for persistent volume storage

### Networking

| Interface | Status | Speed | IP (IPv4) | IP (IPv6) | MAC |
|-----------|--------|-------|-----------|-----------|-----|
| `enp1s0` | UP | 1 Gbps | 10.0.0.59/24 | 2001:4643:ac42::... | e8:ff:1e:df:99:a5 |
| `enp2s0` | DOWN | — | — | — | e8:ff:1e:df:99:a4 |
| `tailscale0` | UP | VPN | 100.127.208.49/32 | fd7a:115c:a1e0::f501:d031 | N/A |
| `wlo1` (WiFi) | DOWN | — | — | — | ea:08:c8:a5:d7:de |

**Network Configuration:**
- Primary: `enp1s0` on 10.0.0.59/24 (active)
- Secondary: `enp2s0` available but down
- WiFi adapter available (disabled)
- Dual NIC capability (same as congenital-optimist)

**Kubernetes Relevance:**
- 1 Gbps sufficient for registry and storage services
- Redundant NIC available if needed
- Good connectivity to cluster

### Power & Thermal

| Metric | Value |
|--------|-------|
| **CPU Temperature** | 35.0°C (idle) |
| **Mobo Temperature** | N/A |
| **Power State** | Very low (ultra-low power CPU) |

**Kubernetes Relevance:**
- Excellent thermal characteristics
- Very low power consumption (ideal for 24/7 operation)
- Suitable for always-on registry/storage services

### OS & Software

- **Init System:** systemd
- **Packages:** 779 (nix-sys)
- **Compilers:** gcc 14.3.0
- **Shell:** bash 5.2.37

---

## Network Topology

### Layer 1: Local Network (10.0.0.0/24)

```
Home Network (ISP Router @ 10.0.0.1)
  │
  ├─ grey-area:        10.0.0.12   (1 Gbps Ethernet)
  ├─ congenital-optimist: 10.0.0.9 (1 Gbps Ethernet)
  ├─ limiting-factor:  10.0.0.59   (1 Gbps Ethernet)
  └─ (other home devices on 10.0.0.0/24)

External IP: 85.164.37.69 (shared WAN)
IPv6: 2001:4643:ac42::/64 (ISP allocation)
```

**Characteristics:**
- Standard home network (192.168 or 10.0.0/24 range)
- DHCP-assigned (non-static currently)
- Dynamic IPv6 with ULA fallback
- 1 Gbps links to each node
- Single ISP connection (SPOF)

### Layer 2: Tailscale VPN Overlay

```
Tailscale Mesh Network (encrypted peer-to-peer)
  │
  ├─ grey-area:           100.119.86.92
  ├─ congenital-optimist: 100.109.28.53
  └─ limiting-factor:     100.127.208.49

Magic DNS: *.taildrop.io (peer node names)
ULA IPv6: fd7a:115c:a1e0::/53 (Tailscale ULA)
```

**Characteristics:**
- WireGuard-based encrypted tunnel
- Automatic peer discovery
- NAT traversal
- End-to-end encryption
- Scalable to multi-site deployments

### Cluster Communication Flows

| Flow | Path | Latency | Use Case |
|------|------|---------|----------|
| Control → Worker | Local 10.0.0.x (1 GbE) | <1ms | Kubernetes API, container pull |
| Inter-pod | Local 10.0.0.x (K8s SDN) | <1ms | Pod-to-pod communication |
| External access | Tailscale 100.x VPN | ~10-50ms | Remote admin, inter-site |
| Registry access | Local 10.0.0.x (1 GbE) | <1ms | Container image pull/push |

---

## Capacity Planning

### CPU Capacity

| Machine | Total vCPU | Reserved* | Available | Oversubscription |
|---------|-----------|-----------|-----------|------------------|
| grey-area | 24 | 4 (control plane) | 20 | Moderate |
| congenital-optimist | 24 | 4 (control plane) | 20 | Moderate |
| limiting-factor | 4 | 2 (registry/storage) | 2 | High |

*Reserved = estimate for system services + control plane daemons

**Planning Impact:**
- grey-area & congenital-optimist: Suitable for general workloads
- limiting-factor: Constrained; only ~2 vCPU for user workloads
- Recommendation: Keep memory-bound services off limiting-factor

### Memory Capacity

| Machine | Total | System | Available | Typical Pod Density |
|---------|-------|--------|-----------|-------------------|
| grey-area | 32 GiB | 2 GiB | 30 GiB | ~10-15 pods (2-3 GiB each) |
| congenital-optimist | 64 GiB | 4 GiB | 60 GiB | ~20-30 pods |
| limiting-factor | 12 GiB | 1 GiB | 11 GiB | ~3-5 pods |

**Planning Impact:**
- congenital-optimist: Most flexible for stateful services
- grey-area: Good for mixed workloads
- limiting-factor: Reserve for lightweight services only

### Storage Capacity

| Machine | Total | Used | Available | Pod Storage |
|---------|-------|------|-----------|-------------|
| grey-area | 224 GiB | 81 GiB | 143 GiB | ~30-50 GiB available |
| congenital-optimist | 9.4 TiB | 94 GiB | 8.3 TiB | ~2-3 TiB available (excellent) |
| limiting-factor | 3.78 TiB | 9 GiB | 3.77 TiB | ~2-3 TiB available (good) |

**Planning Impact:**
- congenital-optimist: Primary persistent volume storage
- limiting-factor: Good for container registry and DB storage
- grey-area: Limited for large persistent volumes

---

## Known Constraints & Considerations

### Cluster-Wide

1. **Single ISP connection** - All nodes share one WAN link (SPOF)
2. **1 Gbps network links** - Adequate for current workloads, may bottleneck at scale
3. **No external load balancer** - Using Tailscale + Traefik ingress
4. **DHCP-assigned IPs** - No static IP configuration (consider configuring)
5. **Mixed hardware generations** - Xeon (2013), Threadripper (2017), Atom (2024)

### grey-area Specific

1. **Single SATA SSD** - Only 224 GiB storage, no redundancy
2. **Server CPU** - Good for control plane, lower performance per watt than Threadripper
3. **Fixed memory** - Would need module replacement to expand beyond 32 GiB

### congenital-optimist Specific

1. **Desktop environment** - GNOME Shell consuming resources; could remove for dedicated server
2. **Power consumption** - Threadripper draws ~180W vs Xeon ~130W
3. **Older architecture** - ZEN v1 (2017); newer CPUs have better efficiency

### limiting-factor Specific

1. **Ultra-low CPU** - Only 4 cores, 6W TDP; will throttle under sustained load
2. **No memory upgrade path** - LPDDR5 soldered; stuck at 12 GiB
3. **eMMC system drive** - Slower than NVMe (already configured, not changeable)
4. **High oversubscription risk** - Should not exceed 2-3 concurrent workloads

---

## Upgrade Potential & Roadmap

### Short-term (Phase 1-2)

- [ ] Configure static IPs on all nodes (instead of DHCP)
- [ ] Add second NIC bonding on congenital-optimist and limiting-factor (redundancy)
- [ ] Consider removing GNOME from congenital-optimist (if not needed)
- [ ] Monitor grey-area storage (may need external NAS mount)

### Medium-term (Phase 3)

- [ ] Add external storage/NAS for persistent volume backups
- [ ] Consider second control plane node (if HA required)
- [ ] Evaluate Gigabit+ switch upgrade (if network becomes bottleneck)

### Long-term (Phase 4+)

- [ ] Replace grey-area SSD with larger NVMe (if persistent storage needed)
- [ ] Consider fourth worker node (if scale requires)
- [ ] Evaluate CPU upgrade for limiting-factor (if too constrained)

---

## Raw System Data

For detailed diagnostic data from each node, see:
- `grey-area.txt` - Full inxi v7 output
- `congenital-optimist.txt` - Full inxi v7 output
- `limiting-factor.txt` - Full inxi v7 output

These raw files contain complete hardware manifests, USB device trees, partition details, and other diagnostic information preserved for reference.

---

**Document Purpose:** Reference material for infrastructure planning, bottleneck analysis, and capacity decisions during Kubernetes migration phases.

**Maintainers:** Update when hardware changes or new diagnostic data available.
