# Nix Container Builds: Reproducible Services

## Building Containers with Nix

Instead of writing Dockerfiles, use `pkgs.dockerTools` or `pkgs.ociTools` to build reproducible OCI containers from Nix expressions.

For the registry and Kubernetes workloads, keep everything declarative following the [NixOS Containers](https://nixos.wiki/wiki/NixOS_Containers) pattern: the container definition is a NixOS module, sources stay in Git, and the `containers.<name>` attribute can be deployed by both `nixos-rebuild` on `limiting-factor` and by the Kubernetes module when building pods.

## Example: Ollama Container

```nix
# packages/ollama-container.nix
{ pkgs, ... }:

pkgs.dockerTools.buildImage {
  name = "ollama";
  tag = "0.1.2";  # Version-controlled
  
  fromImage = pkgs.dockerTools.pullImage {
    imageName = "alpine";
    imageDigest = "sha256:...";  # Pinned base image
  };
  
  copyToRoot = pkgs.buildEnv {
    name = "root";
    paths = with pkgs; [
      ollama
      ca-certificates
    ];
  };
  
  config = {
    Cmd = [ "${pkgs.ollama}/bin/ollama" "serve" ];
    ExposedPorts."11434/tcp" = {};
    Volumes."/root/.ollama" = {};
  };
}
```

## Example: Forgejo Container

```nix
# packages/forgejo-container.nix
{ pkgs, ... }:

pkgs.dockerTools.buildImage {
  name = "forgejo";
  tag = "1.20.5";
  
  fromImage = pkgs.dockerTools.pullImage {
    imageName = "gitea/gitea";
    imageDigest = "sha256:...";  # Use Gitea base, customize
  };
  
  copyToRoot = pkgs.buildEnv {
    name = "root";
    paths = with pkgs; [
      git
      openssh
    ];
  };
  
  config = {
    Cmd = [ "${pkgs.gitea}/bin/gitea" "web" ];
    ExposedPorts."3000/tcp" = {};
  };
}
```

## Container Registry Strategy

### Registry Node: limiting-factor

`limiting-factor` serves as the dedicated node for the OCI registry, running Docker/Podman on NixOS and exposing registry services while remaining declarative via `containers.registry = { ... }` (see the NixOS Containers guide). This keeps registry duties separate from `grey-area`, allows easier attaching of webhooks, and ensures the registry itself is reproducible and versioned.

### Option 1: Self-Hosted Harbor (Recommended)

```yaml
# Kubernetes manifest: container-registry.yaml
apiVersion: v1
kind: Namespace
metadata:
  name: registry

---
apiVersion: helm.cattle.io/v1
kind: HelmChart
metadata:
  name: harbor
  namespace: registry
spec:
  chart: harbor
  repo: https://helm.goharbor.io
  values:
    expose:
      type: ingress
      ingress:
        hosts:
          core: registry.grey-area.local
    externalURL: https://registry.grey-area.local
    persistence:
      enabled: true
      size: 100Gi  # Adjust based on needs
```

### Option 2: Quay.io or Docker Hub (Simpler)

```bash
# Push Nix-built containers to remote registry
nix build ./packages/ollama-container.nix
docker load -i result
docker tag ollama:0.1.2 quay.io/your-org/ollama:0.1.2
docker push quay.io/your-org/ollama:0.1.2
```

### Option 3: MinIO S3 + Container Registry

```yaml
# Use S3-compatible storage for OCI images
# Lighter weight than Harbor
```

## CI/CD: Automated Container Builds

### GitHub Actions Workflow

```yaml
# .github/workflows/build-containers.yaml
name: Build and Push Containers

on:
  push:
    branches: [main]
    paths:
      - 'packages/containers/**'
      - '.github/workflows/build-containers.yaml'

jobs:
  build:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Install Nix
        uses: DeterminateSystems/nix-installer-action@main
      
      - name: Build Ollama container
        run: |
          nix build ./packages/ollama-container.nix
          docker load -i result
      
      - name: Login to registry
        uses: docker/login-action@v2
        with:
          registry: quay.io
          username: ${{ secrets.REGISTRY_USER }}
          password: ${{ secrets.REGISTRY_PASSWORD }}
      
      - name: Push to registry
        run: |
          VERSION=$(nix eval --raw ./packages/ollama-container.nix -A tag)
          docker tag ollama:latest quay.io/your-org/ollama:$VERSION
          docker push quay.io/your-org/ollama:$VERSION
```

### Nix Flake Integration

```nix
# flake.nix additions
{
  outputs = { self, nixpkgs, ... }: {
    packages.x86_64-linux = {
      ollama-container = pkgs.callPackage ./packages/ollama-container.nix {};
      forgejo-container = pkgs.callPackage ./packages/forgejo-container.nix {};
      nextcloud-container = pkgs.callPackage ./packages/nextcloud-container.nix {};
    };
    
    # Build all containers: nix build .#containers
    containers = self.packages.x86_64-linux;
  };
}
```

## Benefits of Nix Containers

- **Reproducible**: Same hash = same binary, always
- **Hermetic**: All dependencies declared, no surprises
- **Version-controlled**: Entire container definition in Git
- **Composable**: Reuse common patterns across services
- **Minimal**: Only needed dependencies included
- **Transparent**: Source of every file is traceable

## Best Practices

1. **Pin base images** by digest, not tag
2. **Use minimal base images** (alpine, scratch)
3. **Layer properly** to maximize cache hits
4. **Version containers** in flake.nix or package.nix
5. **Keep container definitions small** (< 100 lines ideally)
6. **Test containers locally** before pushing
7. **Document port mappings** and environment variables

## Common Patterns

### Multi-stage builds

For larger applications, use multi-stage patterns to reduce final image size.

### Environment variables

```nix
config = {
  Env = [
    "OLLAMA_HOST=0.0.0.0:11434"
    "LOG_LEVEL=info"
  ];
};
```

### Volumes and mounts

```nix
config = {
  Volumes = {
    "/data" = {};
    "/cache" = {};
  };
};
```

## Resources

- [Nix pkgs.dockerTools](https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-dockerTools)
- [NixOS Containers Guide](https://nixos.wiki/wiki/NixOS_Containers)
- [Docker Image Spec](https://github.com/moby/moby/blob/master/image/spec/v1.md)
- [OCI Image Format](https://github.com/opencontainers/image-spec)

---

**Last Updated:** 2025-11-23  
**Status:** Research document for container build strategy
