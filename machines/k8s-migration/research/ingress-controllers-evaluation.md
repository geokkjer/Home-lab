# Ingress Controllers: Traefik vs. Alternatives

## Executive Summary

**Chosen for this project:** Traefik  
**Rationale:** Simplicity, GitOps-friendly (CRDs), excellent Tailscale integration, auto-HTTPS via Let's Encrypt

## What is an Ingress Controller?

An Ingress Controller is a Kubernetes component that manages external HTTP/HTTPS access to services running inside the cluster. It watches `Ingress` resources in Kubernetes and configures a reverse proxy (like Traefik, nginx, HAProxy) to route traffic accordingly.

Without an Ingress Controller, your services are only accessible via NodePort or LoadBalancer services.

## Traefik: The Chosen Path

### Why Traefik?

**1. Kubernetes-Native Design**

```yaml
# Traefik is designed around K8s objects
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: forgejo
spec:
  ingressClassName: traefik
  rules:
    - host: forgejo.grey-area.local
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

It just works with standard Kubernetes Ingress resources.

**2. Tailscale Integration**

Traefik has native Tailscale support via annotations:

```yaml
apiVersion: traefik.io/v1alpha1
kind: IngressRoute
metadata:
  name: forgejo-tailscale
spec:
  entryPoints:
    - tailscale
  routes:
    - match: Host(`forgejo`)
      kind: Rule
      services:
        - name: forgejo
          port: 3000
```

Your services are automatically accessible via Tailscale's MagicDNS without public internet exposure.

**3. CRD-Based Configuration**

Traefik extends Kubernetes with Custom Resource Definitions, making advanced features declarative:

```yaml
# Middleware for authentication, rate limiting, etc.
apiVersion: traefik.io/v1alpha1
kind: Middleware
metadata:
  name: auth-basic
spec:
  basicAuth:
    secret: authsecret

---
# Use middleware in routes
apiVersion: traefik.io/v1alpha1
kind: IngressRoute
metadata:
  name: secure-service
spec:
  routes:
    - match: Host(`admin.grey-area.local`)
      kind: Rule
      middlewares:
        - name: auth-basic
      services:
        - name: admin-service
          port: 8080
```

**4. Automatic Certificate Management**

Auto-HTTPS with Let's Encrypt or self-signed certs:

```yaml
apiVersion: traefik.io/v1alpha1
kind: TLSStore
metadata:
  name: default
spec:
  defaultCertificate:
    secretName: my-cert

---
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: letsencrypt
spec:
  acme:
    server: https://acme-v02.api.letsencrypt.org/directory
    email: admin@grey-area.local
    privateKeySecretRef:
      name: letsencrypt
    solvers:
      - http01:
          ingress:
            class: traefik
```

**5. Observability**

Traefik Dashboard built-in, plus Prometheus metrics:

```
http://grey-area.local:9000/dashboard/
```

View all routes, services, middlewares, and real-time metrics.

### Traefik Installation on NixOS

```nix
# machines/grey-area/configuration.nix
services.kubernetes.helm = {
  releases = {
    traefik = {
      namespace = "traefik";
      repo = "https://traefik.github.io/charts";
      chart = "traefik";
      values = {
        service.type = "ClusterIP";  # Tailscale handles external access
        ports.traefik.expose = true;
        dashboard.enabled = true;
        
        # Enable Tailscale entrypoint
        experimental = {
          v3.enabled = true;
        };
      };
    };
  };
};
```

Or declaratively via ArgoCD (recommended for GitOps).

## Alternative Ingress Controllers

### 1. NGINX Ingress Controller

**Pros:**

- Most popular, largest community
- Extremely well-documented
- Highly configurable
- Excellent performance at scale

**Cons:**

- Configuration via annotations can get verbose
- No built-in dashboard
- Requires ConfigMaps for advanced features
- Larger resource footprint

**When to use:**

- Team already familiar with NGINX
- Need maximum configurability
- Running at cloud scale with 1000+ services

**For this project:** Overkill for 3-node cluster, more operational overhead.

### 2. HAProxy Ingress

**Pros:**

- Battle-tested reverse proxy
- Excellent performance
- Lower resource usage than NGINX

**Cons:**

- Smaller community
- Less documentation
- Configuration less intuitive
- No Tailscale integration out-of-box

**When to use:**

- Need maximum performance per CPU
- Teams with HAProxy expertise

**For this project:** Good alternative if resource-constrained, but less elegant than Traefik.

### 3. Istio Service Mesh

**Pros:**

- Complete service mesh (more than just ingress)
- Advanced traffic management
- Excellent observability
- Mutual TLS built-in

**Cons:**

- Very heavy for small clusters
- Steep learning curve
- Overkill for simple ingress needs
- Complex day-2 operations

**When to use:**

- Managing 10+ microservices
- Need service-to-service encryption
- Building multi-cluster mesh

**For this project:** Way too much complexity for current scope.

### 4. Envoy / Contour

**Pros:**

- Modern, CNCF-graduated
- Clean design philosophy
- Good for multi-cluster

**Cons:**

- Smaller community than NGINX
- Less documentation
- Still requires learning Envoy

**When to use:**

- Building modern cloud infrastructure
- Multi-cluster deployments

**For this project:** Solid alternative, but Traefik's simplicity wins.

## Comparison Matrix

| Feature | Traefik | NGINX | HAProxy | Istio |
|---------|---------|-------|---------|-------|
| **Ease of Setup** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐ |
| **Tailscale Support** | ✅ Native | ⚠️ Via annotations | ❌ No | ❌ No |
| **Dashboard/UI** | ✅ Built-in | ❌ No | ❌ No | ✅ Kiali |
| **K8s-Native Design** | ✅ Yes (CRDs) | ⚠️ Partial | ⚠️ Partial | ✅ Yes |
| **Auto-HTTPS** | ✅ Yes | ⚠️ Via cert-manager | ⚠️ Via cert-manager | ✅ Yes |
| **Performance** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Community Size** | Large | Huge | Medium | Large |
| **Documentation** | Excellent | Excellent | Good | Excellent |
| **Home Lab Fit** | ✅ 10/10 | ⭐ 7/10 | ⭐ 8/10 | ❌ 2/10 |

## Traefik Configuration Examples

### Example 1: Simple HTTP Service

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: ollama
spec:
  ingressClassName: traefik
  rules:
    - host: ollama.grey-area.local
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: ollama
                port:
                  number: 11434
```

### Example 2: Tailscale-Only Service

```yaml
apiVersion: traefik.io/v1alpha1
kind: IngressRoute
metadata:
  name: admin-panel
spec:
  entryPoints:
    - tailscale
  routes:
    - match: Host(`admin`)
      kind: Rule
      services:
        - name: admin-service
          port: 8080
```

### Example 3: HTTPS with Let's Encrypt

```yaml
apiVersion: cert-manager.io/v1
kind: Certificate
metadata:
  name: forgejo-cert
spec:
  secretName: forgejo-tls
  issuerRef:
    name: letsencrypt
    kind: ClusterIssuer
  dnsNames:
    - forgejo.grey-area.local

---
apiVersion: traefik.io/v1alpha1
kind: IngressRoute
metadata:
  name: forgejo-https
spec:
  entryPoints:
    - websecure
  routes:
    - match: Host(`forgejo.grey-area.local`)
      kind: Rule
      services:
        - name: forgejo
          port: 3000
  tls:
    secretName: forgejo-tls
```

### Example 4: Rate Limiting

```yaml
apiVersion: traefik.io/v1alpha1
kind: Middleware
metadata:
  name: rate-limit
spec:
  rateLimit:
    average: 100  # requests per second
    burst: 50

---
apiVersion: traefik.io/v1alpha1
kind: IngressRoute
metadata:
  name: api
spec:
  routes:
    - match: Host(`api.grey-area.local`) && PathPrefix(`/`)
      kind: Rule
      middlewares:
        - name: rate-limit
      services:
        - name: api-service
          port: 8080
```

## Entrypoints for This Cluster

### Public Internet (Port 80/443)

Limited in home lab (behind NAT typically), but configured for future:

```yaml
entryPoints:
  web:
    address: :80
  websecure:
    address: :443
```

### Tailscale VPN (Custom Port)

```yaml
entryPoints:
  tailscale:
    address: 127.0.0.1:8443
```

Services exposed to Tailscale peers only, no public internet exposure needed.

### Cluster-Internal (Port 8080)

```yaml
entryPoints:
  internal:
    address: :8080
```

For monitoring, debugging, internal services.

## Deployment via ArgoCD

Instead of `helm install`, use ArgoCD to manage Traefik:

```yaml
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: traefik
  namespace: argocd
spec:
  project: default
  source:
    repoURL: https://traefik.github.io/charts
    chart: traefik
    targetRevision: 24.0.0
    helm:
      values: |
        service.type: ClusterIP
        dashboard.enabled: true
  destination:
    server: https://kubernetes.default.svc
    namespace: traefik
  syncPolicy:
    automated:
      prune: true
      selfHeal: true
```

Then all Ingress definitions follow, also managed by ArgoCD from Git.

## Decision Log

**Decision:** Use Traefik as primary Ingress Controller  
**Date:** 2025-11-23  
**Rationale:**

- Native Kubernetes design fits NixOS K8s philosophy
- Tailscale integration reduces complexity
- Built-in dashboard aids operations
- GitOps-friendly (CRDs managed by ArgoCD)
- Simple enough for single admin

**Alternatives Considered:**

- NGINX: Too heavy for current scope
- HAProxy: Good alternative but less K8s-native
- Istio: Overkill for services-OS separation pattern

**Contingency:** If performance issues emerge, can migrate to NGINX or HAProxy without major architecture changes (both support standard Ingress resources).

---

**Last Updated:** 2025-11-23  
**Status:** Active decision, informing all downstream ingress planning  
**See Also:** [services-os-separation-rationale.md](./services-os-separation-rationale.md)
