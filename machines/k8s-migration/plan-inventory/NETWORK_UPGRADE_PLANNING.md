# Network Upgrade Planning: 2.5G to 10G Migration

A strategic planning document for upgrading from current 1 Gbps cluster networking to future 2.5G/5G/10G infrastructure.

**Last Updated:** 2025-11-23  
**Status:** Planning document for Phase 4+ deployment

---

## Current Network State

### Existing Infrastructure

| Component | Current | Specification |
|-----------|---------|---------------|
| **Link Speed** | 1 Gbps | Per-node Gigabit Ethernet |
| **NICs per Node** | 2× | All nodes have dual NIC capability (one active) |
| **Protocol** | Ethernet | Standard 802.3, SATA 6.0 Gb/s limit |
| **Topology** | Star | Home router as central switch |
| **Overlay** | Tailscale | WireGuard VPN mesh (10-50ms latency) |

### Current Network Utilization

**Cluster Traffic Patterns:**
- **Control plane → worker:** Kubernetes API (~10-100 Mbps spikes)
- **Pod-to-pod:** Depends on workload (typically <500 Mbps)
- **Container pull:** Registry pulls from limiting-factor (up to 1 Gbps)
- **Backup/sync:** Media transfers on congenital-optimist (up to 1 Gbps)

**Bottleneck Analysis:**
- 1 Gbps *shared* uplink for all cluster traffic
- Media/backup workloads can saturate link
- No QoS currently applied
- Latency acceptable for most workloads

### Current Constraints

| Constraint | Impact | Severity |
|-----------|--------|----------|
| Single ISP uplink | All nodes share 1 Gbps WAN | Medium |
| Gigabit NICs | Not a bottleneck *yet* | Low (will change at scale) |
| No bonding/LAG | Can't aggregate links | Low (future consideration) |
| Home network shared | Cluster shares 10.0.0.0/24 with other devices | Low |
| Tailscale overlay | 10-50ms VPN latency | Low (not performance-critical currently) |

---

## Phase 1-3: No Changes Required

**Current 1 Gbps is sufficient for:**
- ✅ 3-node Kubernetes cluster (initial deployment)
- ✅ 10-20 typical workloads
- ✅ Small container registries
- ✅ Backup/sync at current scale

**When 1 Gbps becomes inadequate:**
- ❌ 50+ concurrent pods with high inter-pod traffic
- ❌ Large database replicas (>1 Gbps sync traffic)
- ❌ High-volume backup jobs
- ❌ Real-time media streaming workloads
- ❌ Scientific computing / HPC workloads

---

## Upgrade Options

### Option 1: 2.5 Gbps SFP+ (Mid-tier Upgrade)

**2.5 Gbps SFP+** is the practical next step: 2.5× throughput at reasonable cost.

#### Hardware Requirements

**NICs:**
- SFP+ 2.5G NICs (Intel X520, Mellanox ConnectX-3)
- Existing multi-NIC infrastructure (limiting-factor, congenital-optimist) already have 2×
- grey-area single NIC, may need upgrade to dual 2.5G

**Cabling:**
- CAT6A ethernet cable (up to 30m @ 10 Gbps, good for 2.5G)
- Cost: $5-10/cable
- Existing runs may work (verify with network tester)

**Switch:**
- Managed switch with 2.5G ports (Netgear, TP-Link, Ubiquiti)
- Entry: TP-Link SG102PCR (2× 2.5G, $30)
- Better: Ubiquiti UniFi Switch 6 (6× 2.5G, $150)
- Enterprise: Mellanox SN2010 (48× 25G, $3000+)

**Cost Estimate (Basic 3-node upgrade):**
```
3× 2.5G NIC cards          $200-400
Switch (Ubiquiti PoE)      $150-300
CAT6A cables (50m)         $50-100
Installation/testing       Time (DIY)
─────────────────────────
Total                       $400-800
```

#### Implementation Path

**Step 1: Procure Hardware**
- Order 3× NIC cards compatible with node motherboards
- Order 2.5G managed switch
- Verify firmware/driver support on NixOS

**Step 2: Configure NICs**
```nix
# NixOS configuration for 2.5G bonds
networking.interfaces.enp4s0 = {
  ipv4.addresses = [{
    address = "10.0.0.12";
    prefixLength = 24;
  }];
};

networking.interfaces.enp6s0 = {
  useDHCP = false;
};

# Optional: Bond both NICs for redundancy
networking.bonds.bond0 = {
  interfaces = [ "enp4s0" "enp6s0" ];
  driverOptions = {
    miimon = "100";
    mode = "active-backup";
  };
};
```

**Step 3: Test**
- Test with iperf3: `iperf3 -c <node> -P 4 -t 30`
- Verify no packet loss
- Monitor switch temperature
- Check for errors in dmesg

**Step 4: Production Rollout**
- Update one node at a time
- Maintain service continuity
- Document any bottlenecks

#### Pros & Cons

| Aspect | Rating | Notes |
|--------|--------|-------|
| **Cost** | ✅ Moderate | $400-800 for full cluster |
| **Complexity** | ✅ Medium | Hardware swap, NixOS config |
| **Vendor Support** | ✅ Good | Mainstream tech (SFP+, CAT6A) |
| **Future-proof** | ⚠️ 3-5 years | Will eventually need 10G |
| **DIY-friendly** | ✅ Yes | Standard Ethernet, no exotic protocols |

---

### Option 2: 10 Gbps SFP+ (High Performance)

**10 Gbps SFP+** is the longer-term target: 10× throughput, industry standard, future-proof.

#### Hardware Requirements

**NICs:**
- 10G SFP+ NICs (Intel X520, Mellanox ConnectX-3, Broadcom NetXtreme)
- Cost per node: $100-200 used, $300+ new
- Power: 25-50W per NIC (more than Gigabit)

**Cabling:**
- DAC (Direct Attach Cable): $30-50 (short runs <3m)
- SR/LR fiber: $100-200 (longer runs, heat dissipation)
- Existing copper won't work (needs low impedance)

**Switch:**
- 10G managed switch (critical component)
- Entry: Mellanox SN2010 (48× 25G), ~$1500 used
- Mid-range: Ubiquiti UniFi Switch Enterprise (8× 10G), $400
- Consumer: MikroTik CRS310 (10× 10G), $250-300
- DIY: Open source switch (ONYX, SONiC) on generic hardware

**Power/Cooling:**
- 10G switches draw 100-300W (needs proper power)
- Heat dissipation (may need room AC upgrade)

**Cost Estimate (Full 10G cluster):**
```
3× 10G NIC cards (used)     $300-600
Switch (Ubiquiti 10G, 8p)   $400-600
10G cabling & optics        $200-300
Installation/testing        Time (DIY)
─────────────────────────
Total                        $900-1500
```

#### Implementation Path

**Phase 4A: Staging (6-12 months)**
- Procure 10G hardware in off-season
- Test in lab with simulator nodes
- Document cabling plan
- Verify driver/firmware compatibility on NixOS

**Phase 4B: Pilot (Limited scope)**
- Deploy 10G between 2 nodes (e.g., grey-area ↔ congenital-optimist)
- Run high-throughput test workload
- Monitor thermal/power/stability
- 1-2 weeks validation

**Phase 4C: Full Rollout (Production)**
- Roll out to 3rd node (limiting-factor)
- Update cluster networking config
- Rebalance pod placement
- Retire old 1G NICs

#### Pros & Cons

| Aspect | Rating | Notes |
|--------|--------|-------|
| **Cost** | ⚠️ Higher | $900-1500, plus power/cooling |
| **Complexity** | ⚠️ High | Fiber optics, switch management |
| **Vendor Support** | ✅ Excellent | Industry standard (SFP+, fiber) |
| **Future-proof** | ✅ 10+ years | Enterprise-grade tech |
| **DIY-friendly** | ⚠️ Moderate | Requires fiber/optics knowledge |

---

### Option 3: Multi-Speed Network (Gradual Upgrade)

**Hybrid approach:** Different link speeds for different purposes.

```
Home Network (1 Gbps ISP link)
  ├─ congenital-optimist: 10G (primary K8s node)
  ├─ grey-area: 2.5G (control plane)
  └─ limiting-factor: 1G (registry, backup can be slower)

OR by use case:
  ├─ K8s cluster traffic: 10G (fast)
  ├─ Container pull: 2.5G (medium)
  └─ Backup/media: 1G (background)
```

#### Benefits
- ✅ Phased investment
- ✅ No forklift upgrade
- ✅ Different speeds for different workloads
- ✅ Flexibility (upgrade nodes on demand)

#### Drawbacks
- ❌ Complex routing
- ❌ Requires managed switch (VLAN support)
- ❌ Potential bottlenecks at slower links
- ❌ Higher maintenance burden

---

## Technology Comparison

| Technology | Speed | Cost | Maturity | DIY | Future |
|-----------|-------|------|----------|-----|--------|
| **Gigabit (current)** | 1 Gbps | ✅ $0 | ✅ Mature | ✅ Easy | 5+ yrs |
| **2.5G SFP+** | 2.5 Gbps | ✅ $400-800 | ✅ Proven | ✅ Medium | 5-7 yrs |
| **5G/nBase-T** | 5 Gbps | ⚠️ $800-1200 | ⚠️ Growing | ⚠️ Medium | 7-10 yrs |
| **10G SFP+** | 10 Gbps | ⚠️ $900-1500 | ✅ Proven | ⚠️ Medium | 10+ yrs |
| **25G SFP28** | 25 Gbps | ❌ $2000+ | ⚠️ Enterprise | ❌ Hard | 15+ yrs |

---

## Networking Strategy by Phase

### Phase 1-2: Bootstrap (Current)
- ✅ Keep 1 Gbps unchanged
- ✅ Dual NICs available for redundancy (not yet bonded)
- ✅ Focus on cluster stability, not throughput

### Phase 3: Stabilization (Month 6-12)
- ✅ Monitor network utilization
- ✅ Collect metrics (throughput, latency, errors)
- ⚠️ If >500 Mbps sustained, plan 2.5G upgrade
- 🔍 Research 2.5G NIC/switch options

### Phase 4a: Mid-tier Upgrade (Month 12-18)
- 🔄 Deploy 2.5G SFP+ to all nodes
- 🔄 Update cluster networking config
- 🔄 Retire 1G NICs (sell/repurpose)
- ✅ Achieved 2.5× throughput headroom

### Phase 4b: Enterprise Grade (Month 18+)
- 🔄 If workload demands, upgrade selected nodes to 10G
- 🔄 Could deploy 10G between high-traffic nodes
- 🔄 Run multi-speed network as interim solution
- 🔄 Eventually converge to full 10G

---

## Monitoring & Capacity Planning

### Metrics to Track (Phase 1-3)

**Network Utilization:**
```bash
# On each node
iftop -i enp4s0  # Real-time bandwidth
nethogs          # Per-process bandwidth

# Kubernetes-level
kubectl top nodes
kubectl top pods --all-namespaces
```

**When to Upgrade:**
- Sustained >600 Mbps (60% of 1 Gbps)
- Regular packet loss or retransmits
- Pod network latency >10ms (on LAN)
- Application timeouts during load

### Performance Baseline (Current)

**Test with iperf3:**
```bash
# On current network (1 Gbps)
iperf3 -c <target> -t 30
# Expected: ~940 Mbps (TCP), 1000 Mbps (UDP)
```

**Record baseline before upgrade:**
- Baseline throughput
- Latency (ping, TCP RTT)
- Packet loss (ethtool statistics)
- CPU utilization

**Post-upgrade:**
- Measure 2.5G throughput (expect ~2350 Mbps TCP)
- Verify latency improvement
- Check NIC efficiency (CPU overhead)

---

## NixOS Configuration Examples

### 1 Gbps Baseline (Current)

```nix
networking.interfaces.enp4s0 = {
  ipv4.addresses = [{
    address = "10.0.0.12";
    prefixLength = 24;
  }];
  useDHCP = false;
};
```

### 2.5G Single NIC

```nix
networking.interfaces.enp4s0 = {
  ipv4.addresses = [{
    address = "10.0.0.12";
    prefixLength = 24;
  }];
  useDHCP = false;
};

# ethtool settings for 2.5G
systemd.network.links."10-enp4s0" = {
  matchConfig.Path = "pci-0000:04:00.0";  # Adjust to your NIC
  linkConfig = {
    Speed = 2500;
    Duplex = "full";
  };
};
```

### Bonded 2.5G (Redundancy)

```nix
networking.bonds.bond0 = {
  interfaces = [ "enp4s0" "enp6s0" ];
  driverOptions = {
    miimon = "100";
    mode = "active-backup";  # or "balance-alb" for load balancing
  };
};

networking.interfaces.bond0 = {
  ipv4.addresses = [{
    address = "10.0.0.12";
    prefixLength = 24;
  }];
  useDHCP = false;
};
```

### 10G with Jumbo Frames (Advanced)

```nix
networking.interfaces.enp4s0 = {
  ipv4.addresses = [{
    address = "10.0.0.12";
    prefixLength = 24;
  }];
  useDHCP = false;
  mtu = 9000;  # Jumbo frames (requires switch support)
};

# Enable jumbo frame support in ethtool
systemd.network.links."10-enp4s0".linkConfig.MTUBytes = 9000;
```

---

## Budget & Timeline Roadmap

### Near-term (Phase 1-3, Months 0-12)
- **Budget:** $0 (no changes)
- **Action:** Monitor, baseline, research

### Mid-term (Phase 4a, Months 12-18)
- **Budget:** $400-800 (2.5G upgrade)
- **Action:** Upgrade all nodes to 2.5G
- **Outcome:** 2.5× throughput headroom

### Long-term (Phase 4b, Months 18+)
- **Budget:** $900-1500 (10G upgrade)
- **Action:** Selective 10G deployment (if needed)
- **Outcome:** 10× baseline throughput, future-proof

---

## Risk Mitigation

### Risks & Mitigations

| Risk | Probability | Mitigation |
|------|-------------|-----------|
| **Hardware incompatibility** | Medium | Test NICs in lab first, verify driver support |
| **Network downtime during upgrade** | Medium | Perform during maintenance window, keep 1G fallback |
| **Switch misconfiguration** | Low | Document VLAN/trunk settings, backup configs |
| **Cable/optics failure** | Low | Redundant connections, diverse cable routing |
| **Power/cooling issues** | Low | Monitor temperatures, plan cooling upgrades |

### Rollback Plan

If 2.5G upgrade fails:
1. Keep old 1G NICs installed in parallel
2. Switch back to 1G interfaces in NixOS config
3. Revert switch configuration
4. Test before attempting 2.5G again
5. Document failure for future reference

---

## Conclusion

**Current 1 Gbps is adequate for Phase 1-3** (next 12 months). No urgent action needed.

**Plan for 2.5G around Month 12-18** as cluster matures and workload increases.

**Reserve 10G as long-term option** if workloads demand it (unlikely in typical home lab scenario).

**Recommendation:** Start Phase 4a with 2.5G SFP+ (good sweet spot of cost/performance). Enables capacity planning without premature investment.

---

**Maintainers:** Update this plan as network utilization data becomes available and hardware options evolve.
