# Lab Tool Development Guide

## Build Commands

### Build the Lab Tool Package
```bash
# Build the lab tool from project root
nix build .#packages.x86_64-linux.lab

# The binary will be available at ./result/bin/lab
```

### Quick Development Build
```bash
# From the lab-tool directory
cd packages/lab-tool
nix build .#lab-tool  # if available, otherwise use full path above
```

## Testing Commands

### Test Lab Tool Functionality
```bash
# Test help command
./result/bin/lab help

# Test machine listing
./result/bin/lab machines

# Test status check
./result/bin/lab status

# Test dry-run deployment
./result/bin/lab deploy little-rascal --dry-run

# Test actual deployment
./result/bin/lab deploy little-rascal
```

### Test System Integration
```bash
# Deploy configuration using nixos-rebuild (requires sudo access)
sudo nixos-rebuild switch --flake .#little-rascal --show-trace

# Or using lab tool (recommended)
lab deploy little-rascal
```

## Development Workflow

### 1. Make Changes
Edit source files in:
- `main.scm` - CLI interface
- `lab/deployment.scm` - Deployment logic
- `lab/machines.scm` - Machine management
- `utils/*.scm` - Utility functions

### 2. Build and Test
```bash
# Rebuild after changes
nix build .#packages.x86_64-linux.lab

# Test basic functionality
./result/bin/lab help
./result/bin/lab machines

# Test deployment (dry-run first)
./result/bin/lab deploy little-rascal --dry-run
```

### 3. Debug Issues
```bash
# Enable Guile debugging
export GUILE_AUTO_COMPILE=0

# Run with verbose output
./result/bin/lab deploy little-rascal --dry-run

# Check deploy-rs command directly
deploy --help
```

## Common Development Tasks

### Update Deploy-rs Command Format
Edit `lab/deployment.scm` in the `build-deploy-command` function:
```scheme
;; Example: Add new flags
(when new-option
  (set! flags (cons "--new-flag=value" flags)))
```

### Add New Machine
Add to the machine list in `lab/machines.scm` or config files.

### Debug Deployment Issues
1. Check the generated command with dry-run
2. Test deploy-rs directly: `deploy '.#little-rascal' --dry-activate`
3. Check flake structure: `nix flake show`

### Module Structure
- `main.scm` - Entry point and CLI parsing
- `lab/core.scm` - Core lab functionality
- `lab/deployment.scm` - Deploy-rs integration
- `lab/machines.scm` - Machine management
- `lab/monitoring.scm` - Health checks and monitoring
- `lab/auto-update.scm` - Automatic update system
- `utils/logging.scm` - Logging system with colors
- `utils/config.scm` - Configuration management
- `utils/ssh.scm` - SSH utilities
- `utils/json.scm` - JSON handling

## Troubleshooting

### Build Failures
```bash
# Check flake structure
nix flake show

# Verify Guile syntax
guile --no-auto-compile -c "(load \"main.scm\")"
```

### Runtime Errors
```bash
# Check module exports
guile -c "(use-modules (lab deployment)) (display 'loaded)"

# Test individual functions
guile -c "(use-modules (lab deployment)) (deploy-machine \"little-rascal\" \"default\" '((dry-run . #t)))"
```

### Deploy-rs Issues
```bash
# Test deploy-rs directly
deploy '.#little-rascal' --dry-activate

# Check machine connectivity
ssh sma@little-rascal 'echo "connected"'
```

## Best Practices

1. **Always test with dry-run first**
2. **Use the lab tool instead of direct nixos-rebuild when possible**
3. **Check flake status before deployment** (`nix flake check`)
4. **Keep commits atomic** - one feature/fix per commit
5. **Update this file when adding new commands or workflows**