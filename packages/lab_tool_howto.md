# Lab Tool Quick Reference

**Home lab infrastructure management and deployment tool**

## 🚀 Quick Commands

```bash
lab status                    # Check all machines
lab deploy-rs sleeper-service # Deploy with safety
lab hybrid-update all         # Update everything
```

## 📋 Status & Monitoring

```bash
lab status              # Basic connectivity check
lab status -v           # Verbose SSH debugging
```

**Output**: ✅ Online | ⚠️ Unreachable | Connection method shown

## 🔄 Deployment Methods

### Modern (Recommended)

```bash
lab deploy-rs <machine>         # Safe deployment with auto-rollback
lab deploy-rs <machine> --dry-run  # Test without applying
```

### Hybrid (Best for Updates)

```bash
lab hybrid-update <machine>     # Update packages + deploy safely
lab hybrid-update all           # Update all machines
lab hybrid-update all --dry-run # Test updates first
```

### Legacy (Fallback)

```bash
lab deploy <machine> boot       # Deploy for next boot
lab deploy <machine> switch     # Deploy and activate now
lab deploy <machine> test       # Temporary deployment
```

## 🔧 Maintenance

```bash
lab update-flake               # Update package versions
```

## 🏠 Machines

- **congenital-optimist** - Local workstation
- **sleeper-service** - File server (NFS, ZFS)
- **grey-area** - Services host (Forgejo, Jellyfin, Ollama)
- **reverse-proxy** - Edge gateway (VPS)

## ⚡ Examples

```bash
# Daily workflow
lab status                          # Check infrastructure
lab hybrid-update sleeper-service   # Update file server
lab deploy-rs grey-area --dry-run   # Test config changes

# Emergency
lab deploy sleeper-service boot     # Fallback deployment
lab status -v                       # Debug connectivity

# Bulk operations
lab hybrid-update all --dry-run     # Test all updates
lab hybrid-update all               # Apply all updates
```

## 🛡️ Safety Features

- **Auto-rollback**: Failed deployments revert automatically
- **Health checks**: Validates services before committing  
- **Dry-run mode**: Test changes without applying
- **Timeouts**: Prevents hanging deployments

## 💡 Tips

- Use `hybrid-update` for regular maintenance
- Always test with `--dry-run` first for bulk operations
- `deploy-rs` provides better safety than legacy method
- Check `lab status` before deployments
