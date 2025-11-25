# DNS Strategy: Current & Future

A repository for DNS infrastructure planning, covering the current pi-hole deployment and options for containerized DNS within the Kubernetes cluster.

**Last Updated:** 2025-11-23

---

## Current State: Pi-hole on Debian

### Hardware Profile

```
Hostname:      pihole
Distro:        Debian GNU/Linux 13 (Trixie)
CPU:           Intel Celeron 2955U (2c, Haswell, 1.4 GHz max)
RAM:           2 GiB
Storage:       16 GiB SSD (14.91 GiB usable, 30% used)
Uptime:        8d 29m (very stable)
```

### Network Configuration

| Interface | Status | Speed | IP (IPv4) | IP (IPv6) |
|-----------|--------|-------|-----------|-----------|
| `enp1s0` (Ethernet) | UP | 1 Gbps | 10.0.0.14/24 | 2001:4643:ac42::... |
| `wlp2s0` (WiFi) | DOWN | — | — | — |
| `tailscale0` | UP | VPN | 100.103.143.108/32 | fd7a:115c:a1e0::2201:8f6c |

**Setup:**

- Dedicated IP: 10.0.0.14 (home network DNS server)
- Tailscale: Available for secure remote DNS access
- 1 Gbps network: Adequate for DNS queries

### Current Services

- **DNS Server:** Dnsmasq (integrated with pi-hole)
- **DHCP:** Dnsmasq
- **Dashboard:** Pi-hole web UI
- **Blocklist Management:** Gravity

### Current Constraints

| Constraint | Impact |
|-----------|--------|
| **Debian system** | Not declarative; manual configuration; drift over time |
| **Old CPU (Celeron 2955U)** | Low TDP, but limited headroom for additional services |
| **2 GiB RAM** | Tight for scale; pi-hole uses ~400-600 MiB normally |
| **16 GiB SSD** | Limited growth room (currently 30% used) |
| **Separate machine** | Additional power consumption; additional point of failure |
| **Not in NixOS ecosystem** | Can't leverage flakes or declarative upgrades |

---

## Option 1: Keep Pi-hole as-is (Status Quo)

### Pros

- ✅ Already working and stable (8+ days uptime)
- ✅ Mature pi-hole ecosystem
- ✅ Low power consumption
- ✅ Dedicated hardware (doesn't steal cluster resources)
- ✅ Web dashboard familiar to users

### Cons

- ❌ Imperative Debian system (manual updates, config drift)
- ❌ Out of NixOS/flakes ecosystem
- ❌ Another machine to maintain and power
- ❌ Not version-controlled in Git
- ❌ Backup/restore is manual

### When This Works

- You're satisfied with current pi-hole operation
- Don't want to disrupt working DNS
- Value simplicity over declarative infrastructure
- Willing to maintain separate Debian box long-term

---

## Option 2: Containerize pi-hole in Kubernetes

Run pi-hole as a container inside your K8s cluster (likely on limiting-factor or congenital-optimist).

### Architecture

```yaml
# Pi-hole in Kubernetes
apiVersion: v1
kind: Namespace
metadata:
  name: dns

---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: pihole
  namespace: dns
spec:
  replicas: 1
  selector:
    matchLabels:
      app: pihole
  template:
    metadata:
      labels:
        app: pihole
    spec:
      containers:
      - name: pihole
        image: pihole/pihole:latest
        ports:
        - containerPort: 53
          protocol: UDP
        - containerPort: 53
          protocol: TCP
        - containerPort: 80
        env:
        - name: TZ
          value: "UTC"
        - name: WEBPASSWORD
          valueFrom:
            secretKeyRef:
              name: pihole-secret
              key: password
        volumeMounts:
        - name: pihole-data
          mountPath: /etc/pihole
        - name: dnsmasq-data
          mountPath: /etc/dnsmasq.d
      volumes:
      - name: pihole-data
        persistentVolumeClaim:
          claimName: pihole-data
      - name: dnsmasq-data
        persistentVolumeClaim:
          claimName: dnsmasq-data

---
apiVersion: v1
kind: Service
metadata:
  name: pihole-dns
  namespace: dns
spec:
  type: NodePort
  selector:
    app: pihole
  ports:
  - name: dns-udp
    port: 53
    protocol: UDP
    targetPort: 53
    nodePort: 30053
  - name: dns-tcp
    port: 53
    protocol: TCP
    targetPort: 53
    nodePort: 30053
  - name: http
    port: 80
    protocol: TCP
    targetPort: 80
    nodePort: 30080
```

### Pros

- ✅ Unified cluster infrastructure
- ✅ Declarative (managed via ArgoCD or kubectl)
- ✅ Version-controlled in Git
- ✅ Easier backup/restore (K8s snapshots)
- ✅ No separate machine power draw
- ✅ Horizontal scalability (multiple pi-hole replicas)
- ✅ Can leverage K8s networking (Tailscale sidecar for remote access)

### Cons

- ❌ DNS becomes dependent on cluster availability
- ❌ K8s restart affects all cluster DNS
- ❌ More complex failure modes
- ❌ Requires persistent volume for gravity database
- ❌ NodePort binding requires specific node(s)

### Recommended Node Placement

- **Preferred:** congenital-optimist (more resources, more reliable)
- **Acceptable:** limiting-factor (low CPU overhead, DNS is lightweight)
- **Not recommended:** grey-area (control plane duties)

---

## Option 3: Declarative DNS with BIND9 on NixOS

Run BIND9 (or Knot DNS) as a NixOS service on a dedicated machine or inside Kubernetes, with full declarative configuration.

### BIND9 on NixOS Machine

```nix
# NixOS configuration for DNS server
services.bind = {
  enable = true;
  zones = {
    "home.lab" = {
      master = true;
      file = "/var/lib/bind/home.lab.zone";
    };
    "100.64.0.0.in-addr.arpa" = {
      master = true;
      file = "/var/lib/bind/100.64.0.0.rev.zone";
    };
  };
};

# Zone file as a config file
environment.etc."bind/home.lab.zone".text = ''
  $TTL 3600
  @   IN  SOA ns1.home.lab. admin.home.lab. (
          2025112301  ; Serial
          3600        ; Refresh
          1800        ; Retry
          604800      ; Expire
          86400 )     ; Minimum TTL
      IN  NS  ns1.home.lab.
      IN  A   10.0.0.12

  ns1 IN  A   10.0.0.12
  
  ; K8s cluster
  grey-area               IN  A   10.0.0.12
  congenital-optimist     IN  A   10.0.0.9
  limiting-factor         IN  A   10.0.0.59
  
  ; Services (CNAMEs to cluster IPs)
  forgejo                 IN  CNAME grey-area
  registry                IN  CNAME limiting-factor
  ollama                  IN  CNAME congenital-optimist
'';
```

### BIND9 in Kubernetes

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: bind-config
  namespace: dns
data:
  named.conf: |
    options {
      directory "/var/cache/bind";
      listen-on { any; };
      listen-on-v6 { any; };
      recursion yes;
      allow-recursion { any; };
      forwarders {
        8.8.8.8;
        8.8.4.4;
      };
    };
    
    zone "home.lab" {
      type master;
      file "/etc/bind/zones/home.lab.zone";
    };
  
  home.lab.zone: |
    $TTL 3600
    @   IN  SOA ns1.home.lab. admin.home.lab. (
            2025112301
            3600
            1800
            604800
            86400 )
        IN  NS  ns1.home.lab.
    
    ns1 IN  A   10.0.0.12
    grey-area IN A 10.0.0.12
    congenital-optimist IN A 10.0.0.9
    limiting-factor IN A 10.0.0.59

---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: bind9
  namespace: dns
spec:
  replicas: 1
  selector:
    matchLabels:
      app: bind9
  template:
    metadata:
      labels:
        app: bind9
    spec:
      containers:
      - name: bind9
        image: internetsystemsconsortium/bind9:9.18
        ports:
        - containerPort: 53
          protocol: UDP
        - containerPort: 53
          protocol: TCP
        volumeMounts:
        - name: bind-config
          mountPath: /etc/bind
      volumes:
      - name: bind-config
        configMap:
          name: bind-config

---
apiVersion: v1
kind: Service
metadata:
  name: bind9-dns
  namespace: dns
spec:
  type: NodePort
  selector:
    app: bind9
  ports:
  - name: dns-udp
    port: 53
    protocol: UDP
    targetPort: 53
    nodePort: 30053
  - name: dns-tcp
    port: 53
    protocol: TCP
    targetPort: 53
    nodePort: 30053
```

### Pros

- ✅ Fully declarative (flakes + NixOS or Git-managed ConfigMaps)
- ✅ Version-controlled
- ✅ No proprietary dashboard needed (you control zones)
- ✅ Extremely stable and proven
- ✅ Can run on lightweight NixOS machine or in K8s
- ✅ Full control over forwarding, recursion, DNSSEC

### Cons

- ❌ More complex to configure than pi-hole UI
- ❌ No built-in blocklists (need external tools)
- ❌ Requires understanding zone file format

### When to Use

- You want fully declarative infrastructure
- Don't need pi-hole's blocklist UI
- Want to version-control all DNS configs in Git
- Comfortable with zone file syntax

---

## Option 4: Hybrid Approach (Recommended)

**Keep pi-hole for now, plan K8s DNS for Phase 3+**

### Phase 1-2: Keep Debian pi-hole

- Maintain current 10.0.0.14 setup
- Document current configuration
- No changes needed (working well)

### Phase 3: Plan Container DNS

- As cluster stabilizes, evaluate migrating pi-hole to K8s
- Test with BIND9 or containerized pi-hole on non-critical branch
- Plan graceful cutover (run both briefly)
- Capture blocklists/rules before migration

### Phase 4: Optimize

- Run DNS as K8s deployment
- Add redundancy (multiple replicas)
- Integrate with ArgoCD (declarative DNS)
- Remove Debian pi-hole box (repurpose hardware)

### Benefits

- ✅ No risk to current infrastructure
- ✅ Time to plan container DNS properly
- ✅ Can test before full migration
- ✅ Separates DNS migration from K8s bootstrap
- ✅ Maintains working DNS during cluster bringup

---

## Blocklist & Filtering Strategy

### Current pi-hole Blocklists

Export from current pi-hole gravity database:

```bash
# Export current configuration
sqlite3 /etc/pihole/pihole-FTL.db "SELECT address FROM adlist;"

# Or via pi-hole API
curl -s "http://10.0.0.14/admin/api.php?blocklists" | jq .
```

### Containerized Blocklists

If moving to K8s, options:

**Option A: Use pi-hole container** (easiest, keeps existing blocklists)

```yaml
image: pihole/pihole:latest
```

**Option B: Use Adguard Home** (similar to pi-hole, containerized)

```yaml
image: adguard/adguardhome:latest
```

**Option C: BIND9 + external blocklist tools**

```bash
# Update zone files with blocklists
curl -s "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts" \
  | awk '{print $1 " " $2}' > /etc/bind/blocklist.hosts
```

---

## DNS Resolution Flow (Current Cluster)

```
User Queries
  ↓
Tailscale MagicDNS (100.x addresses) ←→ Tailscale Nameservers
  ↓
OR
  ↓
Local Queries (10.0.0.x)
  ↓
Pi-hole @ 10.0.0.14 (53/UDP)
  ↓
  ├→ Local zones (grey-area, congenital-optimist, limiting-factor)
  ├→ Gravity blocklists (ads, malware)
  └→ Upstream (8.8.8.8, 1.1.1.1, ISP resolver)
  ↓
Response to Client
```

### Future DNS Flow (K8s-based)

```
User Queries
  ↓
Tailscale MagicDNS (automatic peer resolution)
  ↓
OR
  ↓
K8s Internal DNS (CoreDNS)
  ↓
K8s-hosted BIND9/pi-hole @ 100.x.x.x via Tailscale
  ↓
Response
```

---

## Recommendations

### Immediate (Next 1 week)

1. **Document current setup:**

   ```bash
   # Export pi-hole config
   sudo tar -czf pihole-backup-$(date +%Y%m%d).tar.gz \
     /etc/pihole /etc/dnsmasq.d
   ```

2. **Record current blocklists:**
   - Export gravity database
   - Save as version-controlled list

3. **Plan static IP:**
   - Consider assigning static 10.0.0.14 to pihole (if not already)
   - Or configure DNS resolver to be HA-aware

### Short-term (Phase 1-2: During K8s bootstrap)

1. **Keep pi-hole running** - don't change DNS during cluster bringup
2. **Configure K8s to use pi-hole:**

   ```yaml
   # In kubelet config
   resolvconf:
     nameservers:
     - 10.0.0.14
   ```

3. **Plan container DNS strategy:**
   - Decide: pi-hole or BIND9?
   - Prepare container image
   - Test on development K8s branch

### Medium-term (Phase 3: Post-cluster stabilization)

1. **Test container DNS** on secondary/dev cluster
2. **Prepare migration playbook**
3. **Plan cutover window**
4. **Document zone files** for BIND9 or pi-hole config

### Long-term (Phase 4: Optimization)

1. **Migrate to K8s** (pi-hole or BIND9 container)
2. **Add redundancy** (multiple DNS replicas via K8s)
3. **Retire Debian pi-hole** (repurpose hardware)
4. **Integrate with ArgoCD** (fully declarative DNS)

---

## Option 5: Cloud-Native DNS (ExternalDNS + CoreDNS)

The most Kubernetes-native approach: use **ExternalDNS** to automatically manage DNS records based on Kubernetes resources, backed by **CoreDNS** as the internal resolver.

### Architecture

**CoreDNS** is the default K8s cluster DNS (already running in your cluster). ExternalDNS watches Ingress/Service resources and updates DNS records automatically.

```yaml
# ExternalDNS watches Ingress resources
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: forgejo
  annotations:
    external-dns.alpha.kubernetes.io/hostname: "forgejo.home.lab"
spec:
  ingressClassName: traefik
  rules:
  - host: forgejo.home.lab
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: forgejo
            port:
              number: 3000

# ExternalDNS automatically creates:
# forgejo.home.lab  A  <cluster-ip>
```

### ExternalDNS Deployment

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: external-dns
  namespace: kube-system

---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: external-dns
rules:
- apiGroups: [""]
  resources: ["services", "pods"]
  verbs: ["get", "watch", "list"]
- apiGroups: ["extensions", "networking.k8s.io"]
  resources: ["ingresses"]
  verbs: ["get", "watch", "list"]
- apiGroups: [""]
  resources: ["nodes"]
  verbs: ["list", "watch"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: external-dns
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: external-dns
subjects:
- kind: ServiceAccount
  name: external-dns
  namespace: kube-system

---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: external-dns
  namespace: kube-system
spec:
  replicas: 1
  strategy:
    type: Recreate
  selector:
    matchLabels:
      app: external-dns
  template:
    metadata:
      labels:
        app: external-dns
    spec:
      serviceAccountName: external-dns
      containers:
      - name: external-dns
        image: registry.k8s.io/external-dns/external-dns:v0.13.4
        args:
        - --source=ingress
        - --source=service
        - --provider=rfc2136
        - --rfc2136-host=10.0.0.14  # pi-hole or BIND9
        - --rfc2136-port=53
        - --rfc2136-zone=home.lab
        - --rfc2136-tsig-secret=<base64-encoded-secret>
        - --rfc2136-tsig-secret-alg=hmac-md5
        - --policy=sync  # or "upsert-only"
        - --registry=txt
        - --txt-owner-id=home-lab-k8s
        env:
        - name: EXTERNAL_DNS_LOG_LEVEL
          value: info
```

### How It Works

1. **Annotation-driven:**
   - Add `external-dns.alpha.kubernetes.io/hostname: "service.home.lab"` to Ingress
   - ExternalDNS detects it

2. **Automatic registration:**
   - ExternalDNS connects to pi-hole or BIND9 via RFC2136 (DNS UPDATE)
   - Creates/updates DNS records automatically

3. **Declarative:**
   - All DNS comes from Kubernetes manifests
   - Git-controlled (managed by ArgoCD)
   - Version-tracked changes

4. **Self-healing:**
   - If DNS record deleted manually, ExternalDNS recreates it
   - Keeps DNS in sync with K8s state

### Pros

- ✅ **Fully cloud-native** - DNS driven by K8s resources
- ✅ **Zero-touch DNS management** - no manual zone edits
- ✅ **GitOps-friendly** - declare in YAML, commit to Git
- ✅ **Self-healing** - reconciles DNS state automatically
- ✅ **Scalable** - works with 10 or 1000 services
- ✅ **Provider-agnostic** - works with pi-hole, BIND9, Route53, CloudFlare, etc.
- ✅ **No separate DNS interface needed** - Kubernetes IS the source of truth

### Cons

- ❌ Requires DNS server supporting RFC2136 (pi-hole via dnsmasq, BIND9 support this)
- ❌ Adds another control loop (ExternalDNS reconciliation)
- ❌ TSIG authentication adds complexity (but optional)

### DNS Provider Compatibility

| Provider | RFC2136 | ExternalDNS Support | Notes |
|----------|---------|-------------------|-------|
| **pi-hole + dnsmasq** | ✅ Yes | ✅ Supported | Recommended for home lab |
| **BIND9** | ✅ Yes | ✅ Supported | Enterprise-grade |
| **CoreDNS** | ❌ No | ⚠️ Limited | Use with stub domain only |
| **CloudFlare** | ⚠️ Via API | ✅ Supported | Cloud provider option |
| **Route53** | ⚠️ Via API | ✅ Supported | AWS option |
| **Adguard Home** | ✅ Yes | ✅ Supported | Pi-hole alternative |

### Configuration Examples

#### Example 1: Service Annotation

```yaml
apiVersion: v1
kind: Service
metadata:
  name: forgejo
  annotations:
    external-dns.alpha.kubernetes.io/hostname: "forgejo.home.lab"
spec:
  selector:
    app: forgejo
  ports:
  - port: 80
    targetPort: 3000
  type: LoadBalancer  # or ClusterIP if using Ingress
```

#### Example 2: Ingress Annotation

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: forgejo
  annotations:
    external-dns.alpha.kubernetes.io/hostname: "forgejo.home.lab,git.home.lab"
spec:
  ingressClassName: traefik
  rules:
  - host: forgejo.home.lab
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: forgejo
            port:
              number: 3000
```

#### Example 3: Multiple DNS Entries

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: services
  annotations:
    # Multiple hostnames, ExternalDNS creates all records
    external-dns.alpha.kubernetes.io/hostname: |
      forgejo.home.lab,
      registry.home.lab,
      ollama.home.lab,
      nextcloud.home.lab
spec:
  ingressClassName: traefik
  rules:
  - host: forgejo.home.lab
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: forgejo
            port:
              number: 3000
  - host: registry.home.lab
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: registry
            port:
              number: 5000
```

### ExternalDNS with Tailscale

For remote access via Tailscale without public exposure:

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: forgejo-tailscale
  annotations:
    # Create Tailscale-only DNS (separate zone?)
    external-dns.alpha.kubernetes.io/hostname: "forgejo.ts.home.lab"
spec:
  rules:
  - host: forgejo.ts.home.lab
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: forgejo
            port:
              number: 3000
```

Or use Tailscale's built-in MagicDNS (100.x.x.x addresses auto-resolve peer names).

### Cloud-Native DNS Flow (Recommended for Phase 2+)

```
Developer commits Ingress YAML to Git
  ↓
ArgoCD applies to Kubernetes
  ↓
ExternalDNS watches Ingress
  ↓
ExternalDNS sends RFC2136 UPDATE to pi-hole @ 10.0.0.14
  ↓
pi-hole dnsmasq updates zone file
  ↓
Users query: forgejo.home.lab
  ↓
pi-hole resolves to K8s Ingress IP
  ↓
Traffic reaches service via Traefik
```

### Recommended DNS Setup by Phase

**Phase 1-2 (Bootstrap):**

- Keep pi-hole @ 10.0.0.14 (unchanged)
- No ExternalDNS yet
- Manual DNS entries or use Tailscale MagicDNS

**Phase 2-3 (Stabilization):**

- Deploy ExternalDNS in K8s
- Configure RFC2136 to pi-hole
- Annotate Ingress resources
- New services auto-register DNS

**Phase 3-4 (Optimization):**

- If containerizing DNS: run BIND9 in K8s
- ExternalDNS → K8s-hosted BIND9
- Fully declarative DNS (Ingress → ExternalDNS → BIND9)
- Retire Debian pi-hole

### Advantages Over Manual DNS

| Aspect | Manual | ExternalDNS |
|--------|--------|-------------|
| **DNS entry creation** | Edit zone file manually | Automatic from Ingress annotation |
| **Synchronization** | Manual (easy to forget) | Auto-sync with K8s state |
| **Failover** | Manual CNAME or A record swap | Automatic (ExternalDNS reconciles) |
| **Documentation** | Separate from Ingress | Part of Ingress manifest |
| **Version control** | Zone files (if tracked) | YAML in Git (easy to review/audit) |
| **Scalability** | Tedious with 50+ services | Simple, no extra overhead |

---

## Conclusion

**Current state is fine.** Pi-hole works well and is stable. No urgent need to change during K8s bootstrap.

**Plan for future:** As K8s cluster matures (Phase 3+), containerize DNS to unify infrastructure, improve declarativity, and enable better HA.

**Recommendation:** Use hybrid approach—keep pi-hole until K8s is stable, then plan careful migration to BIND9 or containerized pi-hole.

---

**Maintainers:** Update as DNS infrastructure evolves or container options tested.
