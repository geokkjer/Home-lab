# Migration to Git-Centered Multi-Host Flake Deployment

## Executive Summary

**Yes, you can successfully move to a git-centered multi-host flake with nixos-rebuild and eliminate your custom lab tool.** Your current setup is already 90% ready for this transition.

## Current State Analysis

### ✅ What You Already Have Working

1. **Complete multi-host flake.nix** with all 6 machines configured
2. **Deploy-rs integration** with proper SSH configuration via Tailscale
3. **Well-organized git repository** with proper module structure
4. **SSH key authentication** working across all machines
5. **Proper NixOS configurations** for each machine

### 🔄 What Needs to Change

1. **Replace lab tool commands** with native nixos-rebuild/deploy-rs
2. **Use git URLs** as the source of truth instead of local paths
3. **Streamline deployment workflow** to be git-first

## Migration Steps

### Step 1: Test Git-Based Deployment (30 minutes)

Verify your repo works as a flake input:

```bash
# Test from any machine - this should work immediately
nixos-rebuild build --flake github:yourusername/home-lab#sleeper-service

# Test remote deployment
nixos-rebuild switch --flake github:yourusername/home-lab#sleeper-service \
    --target-host sleeper-service.tail807ea.ts.net \
    --build-host localhost \
    --use-remote-sudo
```

### Step 2: Replace Lab Tool Commands (1 hour)

| Your Current Command            | New Git-Based Command                                                               |
| ------------------------------- | ----------------------------------------------------------------------------------- |
| `lab status`                    | `ssh sma@HOST.tail807ea.ts.net "systemctl is-system-running"`                       |
| `lab deploy-rs sleeper-service` | `nix run github:serokell/deploy-rs -- github:yourusername/home-lab#sleeper-service` |
| `lab hybrid-update all`         | `nix run github:serokell/deploy-rs -- github:yourusername/home-lab`                 |

### Step 3: Create Migration Scripts (30 minutes)

Create `scripts/deploy.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

REPO="github:yourusername/home-lab"
HOST="${1:-}"
ACTION="${2:-switch}"

if [[ -z "$HOST" ]]; then
    echo "Usage: $0 <hostname> [switch|boot|test]"
    echo "Available hosts: congenital-optimist sleeper-service grey-area reverse-proxy little-rascal limiting-factor"
    exit 1
fi

case "$HOST" in
    "congenital-optimist")
        nixos-rebuild "$ACTION" --flake "$REPO#$HOST"
        ;;
    "all")
        nix run github:serokell/deploy-rs -- "$REPO"
        ;;
    *)
        nixos-rebuild "$ACTION" --flake "$REPO#$HOST" \
            --target-host "$HOST.tail807ea.ts.net" \
            --build-host localhost \
            --use-remote-sudo
        ;;
esac
```

### Step 4: Update Flake for Git-First Usage (15 minutes)

Add to your `flake.nix`:

```nix
apps.${system} = {
  # ... existing apps ...

  deploy = {
    type = "app";
    program = "${pkgs.writeShellScript "deploy" ''
      exec ${./scripts/deploy.sh} "$@"
    ''}";
  };

  status = {
    type = "app";
    program = "${pkgs.writeShellScript "status" ''
      hosts="sleeper-service grey-area reverse-proxy little-rascal limiting-factor"
      for host in $hosts; do
        printf "%-20s " "$host:"
        if ssh -o ConnectTimeout=5 sma@$host.tail807ea.ts.net "systemctl is-system-running" 2>/dev/null; then
          echo "✅ Online"
        else
          echo "❌ Unreachable"
        fi
      done
    ''}";
  };
};
```

## Can You Ditch the Lab Tool? YES

### Benefits of Ditching Lab Tool

1. **Simpler toolchain** - No custom Guile code to maintain
2. **Better integration** - Native Nix tooling works with IDEs, CI/CD
3. **Community support** - deploy-rs is actively maintained
4. **Reduced complexity** - Standard workflows everyone understands
5. **Better error handling** - Nix's error messages vs custom tool debugging

### What You'll Lose (and Alternatives)

| Lab Tool Feature  | Alternative               | Difficulty  |
| ----------------- | ------------------------- | ----------- |
| Simple commands   | Shell aliases/scripts     | Easy        |
| Health checks     | systemctl + monitoring    | Easy        |
| Error aggregation | Standard Nix error output | None needed |
| Custom logic      | deploy-rs hooks           | Medium      |

### Transition Strategy

**Week 1: Parallel Testing**

- Use both lab tool and new git-based commands
- Verify everything works identically

**Week 2: Switch Primary Workflow**

- Use git-based commands for daily operations
- Keep lab tool as fallback

**Week 3: Full Migration**

- Remove lab tool from flake.nix
- Archive the code (don't delete)
- Update all documentation

## Daily Workflow (Post-Migration)

### Simple Status Check

```bash
nix run github:yourusername/home-lab#status
```

### Deploy Single Machine

```bash
nix run github:yourusername/home-lab#deploy sleeper-service
```

### Deploy All Machines

```bash
nix run github:serokell/deploy-rs -- github:yourusername/home-lab
```

### Update Dependencies and Deploy

```bash
# Local development
nix flake update
git commit -am "Update dependencies"
git push
nix run github:serokell/deploy-rs

# Or directly from git
nix run github:serokell/deploy-rs -- github:yourusername/home-lab
```

## Why This Migration Makes Sense

### Technical Benefits

1. **Source of truth in git** - No more "is my local repo in sync?"
2. **Atomic deployments** - Git commits = deployment units
3. **Rollback capability** - `git revert` + redeploy
4. **CI/CD ready** - GitHub Actions can deploy on push
5. **Reproducible** - Anyone can deploy from any machine

### Operational Benefits

1. **Simpler onboarding** - Standard Nix workflows
2. **Better documentation** - Tons of deploy-rs examples online
3. **Less maintenance** - No custom tool to update
4. **IDE integration** - Works with direnv, nix-shell, etc.

## Using Git Repo as Flake Input: Advanced Approach

### Overview

Instead of just using git URLs directly in commands, you can structure your setup with your home-lab repo as an input to other flakes or create a "deployment flake" that references your configuration flake. This provides additional benefits:

**Benefits of Git Repo as Input:**

1. **Version pinning** - Lock specific commits of your config
2. **Composite deployments** - Combine multiple config repos
3. **Environment separation** - Different input versions for dev/staging/prod
4. **Dependency management** - Nix manages the git fetching and caching
5. **Reproducible builds** - flake.lock ensures exact versions

### Implementation Options

#### Option 1: Deployment Flake (Recommended)

Create a separate "deployment flake" that uses your home-lab repo as an input:

**`deployment/flake.nix`:**

```nix
{
  description = "Home Lab Deployment Controller";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    deploy-rs.url = "github:serokell/deploy-rs";

    # Your home-lab config as input
    home-lab-config.url = "github:yourusername/home-lab";
    # Or for specific branch: "github:yourusername/home-lab/development"
    # Or for specific commit: "github:yourusername/home-lab/abc123def"
  };

  outputs = { self, nixpkgs, deploy-rs, home-lab-config, ... }: {
    # Re-export configurations from your home-lab
    nixosConfigurations = home-lab-config.nixosConfigurations;

    # Enhanced deployment with additional tooling
    deploy.nodes = {
      sleeper-service = {
        hostname = "sleeper-service.tail807ea.ts.net";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos
            home-lab-config.nixosConfigurations.sleeper-service;
          sshUser = "sma";
          sudo = "sudo -u";
          autoRollback = true;
          magicRollback = true;
        };
      };
      # ... other nodes
    };

    # Deployment apps with version control
    apps.x86_64-linux = {
      deploy-dev = {
        type = "app";
        program = "${nixpkgs.legacyPackages.x86_64-linux.writeShellScript "deploy-dev" ''
          # Deploy using development branch
          nix run github:serokell/deploy-rs -- \
            github:yourusername/deployment-flake/dev
        ''}";
      };

      deploy-prod = {
        type = "app";
        program = "${nixpkgs.legacyPackages.x86_64-linux.writeShellScript "deploy-prod" ''
          # Deploy using main branch (stable)
          nix run github:serokell/deploy-rs -- \
            github:yourusername/deployment-flake/main
        ''}";
      };
    };
  };
}
```

#### Option 2: Multi-Environment in Same Repo

Modify your existing flake to support different input sources:

**Modified `flake.nix`:**

```nix
{
  description = "Home Lab NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    deploy-rs.url = "github:serokell/deploy-rs";

    # Optional: Reference yourself for advanced scenarios
    # home-lab-stable.url = "github:yourusername/home-lab/stable";
  };

  outputs = { self, nixpkgs, deploy-rs, ... } @ inputs:
  let
    system = "x86_64-linux";

    # Function to create deployment for specific git ref
    mkDeployment = gitRef: {
      deploy.nodes = {
        sleeper-service = {
          hostname = "sleeper-service.tail807ea.ts.net";
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos
              (import "${gitRef}/machines/sleeper-service" { inherit inputs; });
            # ... rest of config
          };
        };
      };
    };
  in {
    # ... existing nixosConfigurations ...

    # Multiple deployment targets
    deploy = mkDeployment self;

    apps.${system} = {
      deploy-from-main = {
        type = "app";
        program = "${pkgs.writeShellScript "deploy-main" ''
          nix run github:serokell/deploy-rs -- github:yourusername/home-lab/main
        ''}";
      };

      deploy-from-dev = {
        type = "app";
        program = "${pkgs.writeShellScript "deploy-dev" ''
          nix run github:serokell/deploy-rs -- github:yourusername/home-lab/development
        ''}";
      };
    };
  };
}
```

### Comparison: Direct Git URLs vs Git Inputs

| Aspect                | Direct Git URLs  | Git as Input       |
| --------------------- | ---------------- | ------------------ |
| **Simplicity**        | ✅ Very simple   | ⚠️ More complex    |
| **Version Control**   | ⚠️ Always latest | ✅ Pinned versions |
| **Reproducibility**   | ⚠️ Can change    | ✅ Locked versions |
| **Multi-environment** | ❌ Hard          | ✅ Easy            |
| **Caching**           | ✅ Good          | ✅ Excellent       |
| **Setup Time**        | ✅ Immediate     | ⚠️ More setup      |

### Recommended Hybrid Approach

For your home lab, I recommend a **hybrid approach**:

1. **Start with direct git URLs** (what we outlined earlier) for immediate migration
2. **Evolve to git inputs** once you want more sophisticated deployment patterns

**When to use git inputs:**

- You want staging/production environments
- You need to pin specific versions for stability
- You're composing multiple configuration repositories
- You want sophisticated CI/CD with version management

**When direct git URLs are sufficient:**

- Simple home lab with one configuration repo
- You always want the latest versionwe want to move this repo to a
- You prefer simplicity over advanced features

### Migration Path with Git Inputs

If you choose the git input approach:

**Phase 1: Create deployment flake**

```bash
mkdir -p deployment
cd deployment
# Create the deployment flake shown above
```

**Phase 2: Update workflow**

```bash
# Instead of:
nix run github:serokell/deploy-rs -- github:yourusername/home-lab

# You'd use:
nix run github:serokell/deploy-rs -- github:yourusername/deployment-flake

# Or for specific environments:
nix run github:yourusername/deployment-flake#deploy-dev
nix run github:yourusername/deployment-flake#deploy-prod
```

**Phase 3: Version management**

```bash
# Pin to specific home-lab version
cd deployment
nix flake update home-lab-config --commit-lock-file
git commit -m "Pin home-lab to version abc123"

# Deploy pinned version
nix run github:serokell/deploy-rs
```

### Impact on Lab Tool Elimination

Using git inputs **doesn't change** the fundamental recommendation to eliminate the lab tool. In fact, it makes it even more compelling because:

1. **Nix handles git operations** - No need for custom git logic in lab tool
2. **Better version management** - Native flake.lock handling
3. **Cleaner abstractions** - Separate deployment concerns from config

### Recommendation

For your specific situation, I'd suggest:

1. **Start with direct git URLs** (simpler, immediate migration)
2. **Keep git inputs as future enhancement** when you need:
   - Staging environments
   - Version pinning for stability
   - More complex deployment patterns

The beauty is that both approaches eliminate the lab tool dependency and use git as the source of truth. The git input approach just adds more sophisticated version management on top.

