# deploy-rs Research Summary

## Overview

**deploy-rs** is a Rust-based deployment tool specifically designed for NixOS flakes. It provides declarative, reliable, and efficient deployment of NixOS configurations to remote machines with advanced features like rollback capabilities, health checks, and parallel deployments.

**Repository**: https://github.com/serokell/deploy-rs
**Status**: Actively maintained by Serokell
**Language**: Rust (fast, reliable, memory-safe)

## Key Features

### 🚀 **Core Capabilities**
- **Flake-native**: Built specifically for Nix flakes (no legacy nix-env/channels)
- **Multi-target deployment**: Deploy to multiple machines simultaneously
- **Automatic rollback**: Failed deployments automatically revert to previous generation
- **Health checks**: Configurable post-deployment validation
- **SSH-based**: Uses SSH for secure remote deployment (like our current lab tool)
- **Profile management**: Supports system, user, and custom profiles

### 🔧 **Advanced Features**
- **Parallel deployment**: Deploy to multiple machines concurrently
- **Interactive confirmation**: Can prompt before applying changes
- **Dry-run mode**: Preview changes without applying them
- **Magic rollback**: Automatic rollback on deployment failures or health check failures
- **Custom activation**: Define custom activation scripts and checks
- **Sudo handling**: Intelligent sudo privilege escalation

### 📊 **Reliability Features**
- **Atomic deployments**: Either succeeds completely or rolls back
- **Connection resilience**: Handles SSH connection issues gracefully
- **Generation tracking**: Keeps track of deployment history
- **Activation timeout**: Prevents hanging deployments
- **Health check timeout**: Configurable validation windows

## Configuration Structure

Deploy-rs uses a declarative configuration format in your flake:

```nix
# flake.nix
{
  # ... existing flake configuration

  deploy.nodes = {
    sleeper-service = {
      hostname = "sleeper-service.tail807ea.ts.net";
      profiles.system = {
        user = "root";
        path = deploy-rs.lib.x86_64-linux.activate.nixos 
          self.nixosConfigurations.sleeper-service;
        sshUser = "sma";
        sudo = "sudo -u";
      };
    };
    
    grey-area = {
      hostname = "grey-area.tail807ea.ts.net";
      profiles.system = {
        user = "root";
        path = deploy-rs.lib.x86_64-linux.activate.nixos 
          self.nixosConfigurations.grey-area;
        sshUser = "sma";
        sudo = "sudo -u";
      };
    };
  };

  # Health checks
  deploy.nodes.sleeper-service.profiles.system.activationTimeout = 240;
  deploy.nodes.sleeper-service.profiles.system.confirmTimeout = 30;
}
```

## Command Examples

```bash
# Deploy to single machine
deploy '.#sleeper-service'

# Deploy to all machines
deploy '.#'

# Dry run (check what would be deployed)
deploy '.#sleeper-service' -- --dry-activate

# Skip health checks
deploy '.#sleeper-service' -- --skip-checks

# Interactive confirmation
deploy '.#sleeper-service' -- --confirm-timeout 60

# Deploy specific profile
deploy '.#sleeper-service.system'
```

## Comparison with Current `lab` Tool

| Feature | Current `lab` Tool | deploy-rs |
|---------|-------------------|-----------|
| **Language** | Shell script | Rust (compiled) |
| **Performance** | Good | Excellent |
| **Parallel deployment** | ❌ | ✅ |
| **Automatic rollback** | ❌ | ✅ |
| **Health checks** | ❌ | ✅ |
| **Flake-native** | ✅ | ✅ |
| **SSH-based** | ✅ | ✅ |
| **Status monitoring** | ✅ | Limited |
| **Custom workflows** | ✅ | Limited |
| **Learning curve** | Low | Medium |
| **Configuration** | Shell script | Nix flake |

## Advantages of deploy-rs

### ✅ **Production-Ready Features**
- **Reliability**: Automatic rollback prevents broken deployments
- **Speed**: Rust performance + parallel deployment
- **Safety**: Health checks ensure successful activation
- **Consistency**: Declarative configuration in flake

### ✅ **Operational Benefits**
- **Reduced downtime**: Atomic deployments with quick rollback
- **Error handling**: Sophisticated error recovery mechanisms
- **Audit trail**: Built-in deployment history tracking
- **Validation**: Pre and post-deployment checks

### ✅ **Scale Benefits**
- **Multi-machine**: Deploy entire infrastructure simultaneously
- **Efficiency**: Parallel operations reduce total deployment time
- **Resource management**: Better handling of resource conflicts

## Disadvantages & Limitations

### ❌ **Current Limitations**
- **Status monitoring**: No equivalent to `lab status` for infrastructure overview
- **Custom workflows**: Less flexible than shell scripts for custom operations
- **Learning curve**: Requires understanding deploy-rs configuration syntax
- **Debugging**: Rust binary vs readable shell script
- **Community size**: Smaller ecosystem compared to traditional tools

### ❌ **Home Lab Specific Concerns**
- **Overkill factor**: May be complex for 3-4 machine home lab
- **Customization**: Our `lab` tool has home lab specific features
- **Integration**: Would need to replicate status monitoring capabilities
- **Development workflow**: Less hackable than shell scripts

## Implementation Recommendations

### 🎯 **Hybrid Approach (Recommended)**
Keep the best of both tools:

1. **Use deploy-rs for deployments**:
   ```bash
   lab deploy-rs sleeper-service  # Use deploy-rs backend
   lab deploy grey-area          # Current shell script method
   ```

2. **Keep `lab` tool for status and management**:
   ```bash
   lab status                    # Infrastructure overview
   lab check sleeper-service     # Health monitoring
   lab logs grey-area            # Log access
   ```

### 🔧 **Migration Strategy**

**Phase 1: Evaluation** (Current)
- Add deploy-rs configuration to flake
- Test deployment on non-critical machine
- Compare reliability and performance

**Phase 2: Gradual Adoption**
- Migrate stable machines to deploy-rs
- Keep custom `lab` commands for monitoring
- Maintain shell script fallback

**Phase 3: Integration**
- Enhance `lab` tool to use deploy-rs as backend
- Add deploy-rs specific features to `lab status`
- Maintain unified interface

### 📝 **Flake Integration Example**

```nix
# Add to flake.nix inputs
inputs.deploy-rs.url = "github:serokell/deploy-rs";

# Add to flake outputs
deploy = {
  nodes = {
    sleeper-service = {
      hostname = "10.0.0.8";  # Or Tailscale hostname
      profiles.system = {
        user = "root";
        path = deploy-rs.lib.x86_64-linux.activate.nixos 
          self.nixosConfigurations.sleeper-service;
        sshUser = "sma";
        sshOpts = ["-p" "22"];
        fastConnection = false;  # For home network
        autoRollback = true;
        magicRollback = true;
        activationTimeout = 180;
        confirmTimeout = 30;
      };
    };
  };
};

# Health checks can reference systemd services
deploy.nodes.sleeper-service.profiles.system.activationTimeout = 240;
```

## Conclusion & Recommendation

### 🎯 **For Our Home Lab**

**deploy-rs is valuable for:**
- Production-quality deployments with rollback safety
- Parallel deployment when infrastructure grows
- Reduced risk of broken remote systems
- Professional deployment practices

**Our current `lab` tool excels at:**
- Home lab specific status monitoring
- Custom workflows and debugging
- Simple, hackable shell script approach
- Tailored for our specific infrastructure

### 📋 **Action Plan**

1. **Immediate**: Add deploy-rs configuration to flake (low effort, high learning)
2. **Short-term**: Test deploy-rs on sleeper-service alongside current method
3. **Medium-term**: Consider hybrid approach - deploy-rs for deployment, `lab` for monitoring
4. **Long-term**: Evaluate full migration based on home lab growth and complexity needs

**Verdict**: deploy-rs is a professional-grade tool that would enhance our deployment reliability. The hybrid approach allows us to benefit from deploy-rs's deployment safety while keeping our custom infrastructure monitoring capabilities.