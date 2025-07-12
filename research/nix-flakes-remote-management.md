# Nix Flakes: A Comprehensive Guide to Remote Management

## What Are Nix Flakes?

Nix flakes are an experimental feature (now stable as of Nix 2.4+) that provides a standard way to write reproducible Nix expressions. They address key issues in the Nix ecosystem by offering:

- **Uniform structure** for Nix projects
- **Version pinning** of dependencies via lock files
- **Reproducible builds** across different environments
- **URL-like syntax** for specifying remote resources

## Migration to Multi-Host Flake with Git as Source of Truth

### Current State Analysis

Your current setup already has most pieces in place for a git-centric multi-host flake deployment:

**✅ Already Implemented:**

- Multi-host flake.nix with all machines configured
- Deploy-rs integration for safe remote deployments
- Git repository structure with proper organization
- SSH key-based authentication via Tailscale

**🔄 Needs Migration:**

- Lab tool dependency can be reduced/eliminated
- Direct nixos-rebuild usage needs configuration
- Git remote references need to be established
- Deployment workflows need streamlining

### Migration Steps

#### Step 1: Prepare Git Repository as Source of Truth

1. **Ensure Git Repository is Clean and Accessible**

```bash
# Make sure your repo is committed and pushed
git add .
git commit -m "Pre-migration: Current stable state"
git push origin main

# Verify remote access works
git ls-remote origin
```

2. **Add Git URL to Flake Registry** (Optional but Convenient)

```bash
# Add your repo to the flake registry for easy access
nix registry add home-lab github:yourusername/home-lab
# or for a local git repo:
nix registry add home-lab git+file:///home/geir/Projects/home-lab
```

#### Step 2: Configure Direct nixos-rebuild Usage

Create deployment scripts that use git as the source:

**`scripts/deploy-from-git.sh`:**

```bash
#!/usr/bin/env bash
set -euo pipefail

REPO_URL="github:yourusername/home-lab"  # or your actual repo URL
HOST="${1:-}"
ACTION="${2:-switch}"

if [[ -z "$HOST" ]]; then
    echo "Usage: $0 <hostname> [switch|boot|test]"
    echo "Available hosts: congenital-optimist, sleeper-service, grey-area, reverse-proxy, little-rascal, limiting-factor"
    exit 1
fi

case "$HOST" in
    "congenital-optimist")
        # Local deployment
        nixos-rebuild "$ACTION" --flake "$REPO_URL#$HOST"
        ;;
    *)
        # Remote deployment
        nixos-rebuild "$ACTION" --flake "$REPO_URL#$HOST" \
            --target-host "$HOST.tail807ea.ts.net" \
            --build-host localhost \
            --use-remote-sudo
        ;;
esac
```

#### Step 3: Eliminate Lab Tool Dependencies

Your lab tool currently provides several features. Here's how to replace them with native nixos-rebuild and deploy-rs:

**Lab Tool Feature → Replacement:**

1. **`lab status`** → **SSH + systemctl**

```bash
# Replace with simple SSH checks
ssh sma@sleeper-service.tail807ea.ts.net "systemctl is-system-running"
```

2. **`lab deploy-rs <machine>`** → **Direct deploy-rs**

```bash
# Already in your flake.nix - use directly
nix run github:serokell/deploy-rs -- .#sleeper-service
```

3. **`lab hybrid-update`** → **nixos-rebuild with git**

```bash
# Update flake.lock and deploy
nix flake update github:yourusername/home-lab
nixos-rebuild switch --flake github:yourusername/home-lab#sleeper-service \
    --target-host sleeper-service.tail807ea.ts.net \
    --build-host localhost
```

#### Step 4: Create Streamlined Deployment Workflow

**Option A: Pure nixos-rebuild Approach**

Add this to your flake.nix apps section:

```nix
apps.${system} = {
  # ...existing apps...

  deploy-from-git = {
    type = "app";
    program = "${nixpkgs.legacyPackages.${system}.writeShellScript "deploy-from-git" ''
      set -euo pipefail
      HOST="''${1:-}"
      ACTION="''${2:-switch}"
      REPO_URL="github:yourusername/home-lab"

      if [[ -z "$HOST" ]]; then
        echo "Usage: nix run .#deploy-from-git <hostname> [switch|boot|test]"
        echo "Available hosts: ${builtins.concatStringsSep ", " (builtins.attrNames self.nixosConfigurations)}"
        exit 1
      fi

      case "$HOST" in
        "congenital-optimist")
          nixos-rebuild "$ACTION" --flake "$REPO_URL#$HOST"
          ;;
        *)
          nixos-rebuild "$ACTION" --flake "$REPO_URL#$HOST" \
            --target-host "$HOST.tail807ea.ts.net" \
            --build-host localhost \
            --use-remote-sudo
          ;;
      esac
    ''}";
  };

  update-and-deploy = {
    type = "app";
    program = "${nixpkgs.legacyPackages.${system}.writeShellScript "update-and-deploy" ''
      set -euo pipefail
      HOST="''${1:-all}"

      echo "Updating flake.lock..."
      nix flake update

      if [[ "$HOST" == "all" ]]; then
        for host in ${builtins.concatStringsSep " " (builtins.attrNames self.nixosConfigurations)}; do
          echo "Deploying $host..."
          nix run .#deploy-from-git "$host"
        done
      else
        nix run .#deploy-from-git "$HOST"
      fi
    ''}";
  };
};
```

**Option B: Enhanced deploy-rs Configuration**

Your current deploy-rs config is already excellent. Just ensure you're using it directly:

```bash
# Deploy single machine
nix run github:serokell/deploy-rs -- .#sleeper-service

# Deploy all machines
nix run github:serokell/deploy-rs

# Dry run
nix run github:serokell/deploy-rs -- .#sleeper-service --dry-activate
```

#### Step 5: Implement Git-Based CI/CD (Optional)

Create `.github/workflows/deploy.yml`:

```yaml
name: Deploy NixOS Configurations
on:
  push:
    branches: [main]
  workflow_dispatch:
    inputs:
      target:
        description: 'Target machine (or "all")'
        required: true
        default: "all"
        type: choice
        options:
          - all
          - sleeper-service
          - grey-area
          - reverse-proxy
          - little-rascal

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: DeterminateSystems/nix-installer-action@v4
      - uses: DeterminateSystems/magic-nix-cache-action@v2

      - name: Setup SSH
        env:
          SSH_PRIVATE_KEY: ${{ secrets.SSH_PRIVATE_KEY }}
        run: |
          mkdir -p ~/.ssh
          echo "$SSH_PRIVATE_KEY" > ~/.ssh/id_rsa
          chmod 600 ~/.ssh/id_rsa
          ssh-keyscan -H sleeper-service.tail807ea.ts.net >> ~/.ssh/known_hosts
          # Add other hosts...

      - name: Deploy configurations
        run: |
          if [[ "${{ github.event.inputs.target || 'all' }}" == "all" ]]; then
            nix run github:serokell/deploy-rs
          else
            nix run github:serokell/deploy-rs -- .#${{ github.event.inputs.target }}
          fi
```

### Can You Ditch the Lab Tool?

**Yes, you can largely eliminate the lab tool dependency.** Here's the analysis:

#### Lab Tool Features vs Native Alternatives

| Lab Tool Feature    | Native Alternative                         | Keep or Ditch?                            |
| ------------------- | ------------------------------------------ | ----------------------------------------- |
| `lab status`        | `ssh + systemctl` or simple scripts        | **Ditch** - Simple to replace             |
| `lab deploy-rs`     | Direct `nix run github:serokell/deploy-rs` | **Ditch** - Redundant wrapper             |
| `lab hybrid-update` | `nix flake update + nixos-rebuild`         | **Ditch** - Native tools better           |
| Machine inventory   | Flake configuration                        | **Ditch** - Already in flake.nix          |
| SSH connectivity    | Native SSH + Tailscale                     | **Ditch** - No added value                |
| Health checks       | Systemd + monitoring                       | **Maybe Keep** - If you need custom logic |

#### Recommended Transition Strategy

**Phase 1: Immediate (Keep Lab Tool for Safety)**

- Start using direct nixos-rebuild commands alongside lab tool
- Test git-based deployments in parallel
- Verify all functionality works

**Phase 2: Gradual Migration (2-4 weeks)**

- Replace lab tool usage with native commands in daily workflow
- Keep lab tool available as fallback
- Document new procedures

**Phase 3: Complete Migration (1 month)**

- Remove lab tool from default packages
- Archive lab tool code (don't delete - might need reference)
- Update documentation to reflect new workflow

#### Simplified Daily Workflow (Post-Migration)

```bash
# Check status
ssh sma@sleeper-service.tail807ea.ts.net "systemctl is-system-running"

# Update and deploy single machine
nix flake update
nixos-rebuild switch --flake github:yourusername/home-lab#sleeper-service \
    --target-host sleeper-service.tail807ea.ts.net \
    --build-host localhost

# Or use deploy-rs (recommended)
nix run github:serokell/deploy-rs -- .#sleeper-service

# Deploy all machines
nix run github:serokell/deploy-rs

# Test deployment
nix run github:serokell/deploy-rs -- .#sleeper-service --dry-activate
```

### Key Benefits of This Migration

1. **Simplified Toolchain**: Fewer custom tools to maintain
2. **Better Integration**: Native Nix tooling works better with ecosystem
3. **Improved CI/CD**: GitHub Actions + git hooks work seamlessly
4. **Enhanced Reproducibility**: Git references ensure exact versions
5. **Reduced Complexity**: No custom Guile code to debug
6. **Better Documentation**: Standard Nix workflows are well-documented
7. **Community Support**: deploy-rs has active community and examples

### Migration Checklist

- [ ] Ensure git repository is accessible remotely
- [ ] Test direct nixos-rebuild with git references
- [ ] Verify deploy-rs works with all machines
- [ ] Create simple status checking scripts
- [ ] Update deployment documentation
- [ ] Train team on new workflow
- [ ] Set up CI/CD if desired
- [ ] Archive lab tool code
- [ ] Remove lab tool from flake.nix

## Core Structure and Components

### Flake.nix Structure

Every flake contains a `flake.nix` file in its root directory with this basic structure:

```nix
{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs = { self, nixpkgs }: {
    # Your outputs here
  };
}
```

### Flake.lock Files

- Automatically generated by Nix
- Pins all dependencies to exact revisions
- Ensures deterministic builds
- Contains a graph of all input dependencies with specific commit hashes

## Remote Management Strategies

### 1. GitHub-Based Remote References

Flakes use intuitive URL syntax for remote repositories:

```nix
inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  home-manager.url = "github:nix-community/home-manager";
  my-config.url = "github:username/nixos-config";
}
```

**URL Formats:**

- `github:owner/repo` - Latest commit from default branch
- `github:owner/repo/branch` - Specific branch
- `github:owner/repo/commit-hash` - Specific commit
- `github:owner/repo?dir=subdir` - Subdirectory in repository

### 2. Remote Deployment Tools

#### nixos-rebuild (Built-in)

The official tool with remote capabilities:

```bash
nixos-rebuild switch --flake .#hostname \
  --target-host root@192.168.1.100 \
  --build-host localhost
```

**Advantages:**

- Built into NixOS
- Supports cross-compilation
- Can build locally or remotely

#### deploy-rs

A sophisticated multi-profile deployment tool:

```bash
nix run github:serokell/deploy-rs
```

**Features:**

- Multi-profile deployments
- Lesser-privileged deployments
- Rollback capabilities
- Safety checks for remote connectivity

#### Colmena

Community tool for fleet management:

```bash
nix run nixpkgs#colmena apply
```

**Benefits:**

- Fleet-wide deployments
- Parallel operations
- Custom deployment strategies

### 3. FlakeHub: Modern Distribution Platform

FlakeHub is a commercial platform by Determinate Systems offering:

- **Semantic versioning** for flakes
- **Private flake repositories**
- **Advanced caching** with identity-aware access
- **GitHub Actions integration** for automated publishing

```bash
# Using FlakeHub CLI
fh search <query>
fh apply github:DeterminateSystems/zero-to-nix
```

## Advanced Remote Management Patterns

### 1. Multi-Host Configurations

Structure your flake to manage multiple hosts:

```nix
{
  outputs = { nixpkgs, ... }: {
    nixosConfigurations = {
      server1 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./hosts/server1 ];
      };
      server2 = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [ ./hosts/server2 ];
      };
    };
  };
}
```

### 2. Cross-Architecture Deployments

Nix flakes excel at cross-compilation:

```bash
# Build ARM64 system on x86_64 host
nixos-rebuild switch --flake .#pi4 \
  --target-host pi@192.168.1.50 \
  --build-host localhost
```

### 3. Registry and Shortcuts

Use the flake registry for convenient shortcuts:

```bash
# Add custom registry entries
nix registry add my-config github:username/nixos-config

# Use in other flakes
nixos-rebuild switch --flake my-config#hostname
```

## Best Practices for Remote Management

### 1. Repository Organization

```
nixos-config/
├── flake.nix
├── flake.lock
├── hosts/
│   ├── server1/
│   ├── server2/
│   └── workstation/
├── modules/
│   ├── common/
│   └── services/
└── secrets/
```

### 2. Security Considerations

- Use SSH key authentication
- Implement proper access controls
- Consider using tools like `sops-nix` for secrets management
- Enable automatic rollback on deployment failures

### 3. CI/CD Integration

GitHub Actions example for automated deployments:

```yaml
name: Deploy NixOS
on:
  push:
    branches: [main]
jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: DeterminateSystems/nix-installer-action@v4
      - name: Deploy to servers
        run: |
          nix run github:serokell/deploy-rs
```

### 4. Caching Strategies

- Use binary caches to speed up deployments
- Consider FlakeHub Cache for enterprise environments
- Implement local caching for frequently used dependencies

## Troubleshooting Remote Management

### Common Issues:

1. **SSH connectivity** - Ensure passwordless authentication
2. **Build failures** - Check cross-compilation settings
3. **Lock file conflicts** - Use `nix flake update` carefully
4. **Network timeouts** - Configure appropriate timeouts for remote operations

### Debugging Commands:

```bash
# Test flake evaluation
nix flake show github:username/repo

# Check flake metadata
nix flake metadata github:username/repo

# Update specific input
nix flake update input-name
```

## Conclusion

Nix flakes provide a robust foundation for remote system management with excellent reproducibility guarantees. The combination of GitHub-based distribution, powerful deployment tools like deploy-rs, and modern platforms like FlakeHub creates a comprehensive ecosystem for managing distributed Nix-based infrastructure.

**For your specific home lab setup**, transitioning to a git-centric multi-host flake deployment will:

- **Simplify your toolchain** by eliminating the custom lab tool
- **Improve reproducibility** by using git as the single source of truth
- **Enhance collaboration** by leveraging standard Nix workflows
- **Reduce maintenance burden** by using well-supported community tools

The migration can be done gradually, and your existing flake.nix structure already supports this transition perfectly. The key to successful remote management lies in proper repository organization, security practices, and choosing the right tools for your specific deployment patterns.
