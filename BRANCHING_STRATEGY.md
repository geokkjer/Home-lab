# 🌳 Git Branching Strategy for Home Lab Infrastructure

## Branch Structure

### 🚀 Main Branches

#### `main`
- **Purpose**: Production-ready configurations
- **Protection**: Protected branch with required reviews
- **Deployment**: Automatically deployed to production machines
- **Stability**: Should always be stable and tested

#### `develop`
- **Purpose**: Integration branch for new features
- **Testing**: Continuous integration testing
- **Merging**: Features merge here first
- **Deployment**: Deployed to staging/test environments

### 🔧 Supporting Branches

#### Feature Branches: `feature/<description>`
- **Purpose**: Development of new features or modules
- **Naming**: `feature/add-cosmic-desktop`, `feature/sleeper-service-config`
- **Lifetime**: Temporary, deleted after merge
- **Source**: Branch from `develop`
- **Merge**: Merge back to `develop`

#### Machine Branches: `machine/<machine-name>`
- **Purpose**: Machine-specific configuration changes
- **Naming**: `machine/congenital-optimist`, `machine/sleeper-service`
- **Use Case**: Testing machine-specific changes
- **Merge**: Merge to `develop` after testing

#### Hotfix Branches: `hotfix/<issue>`
- **Purpose**: Critical fixes for production
- **Naming**: `hotfix/security-patch`, `hotfix/boot-failure`
- **Source**: Branch from `main`
- **Merge**: Merge to both `main` and `develop`

#### Module Branches: `module/<module-name>`
- **Purpose**: Development of specific modules
- **Naming**: `module/virtualization`, `module/desktop-gnome`
- **Scope**: Single module focus
- **Testing**: Module-specific testing

### 🏷️ Tagging Strategy

#### Version Tags: `v<major>.<minor>.<patch>`
- **Purpose**: Mark stable releases
- **Format**: `v1.0.0`, `v1.2.1`
- **Trigger**: Major configuration milestones
- **Deployment**: Tag triggers deployment workflows

#### Machine Tags: `<machine>-v<version>`
- **Purpose**: Machine-specific deployments
- **Format**: `congenital-optimist-v1.0.0`
- **Use Case**: Track per-machine configurations
- **Rollback**: Enable machine-specific rollbacks

#### Phase Tags: `phase-<number>-complete`
- **Purpose**: Mark migration phase completion
- **Format**: `phase-1-complete`, `phase-2-complete`
- **Documentation**: Link to plan.md milestones

## 🔄 Workflow Examples

### Standard Feature Development
```bash
# Start new feature
git checkout develop
git pull origin develop
git checkout -b feature/add-incus-clustering

# Develop and test
# ... make changes ...
nix flake check
sudo nixos-rebuild test --flake .#congenital-optimist

# Commit and push
git add .
git commit -m "feat: add Incus clustering support"
git push origin feature/add-incus-clustering

# Create PR to develop
# ... review process ...
# Merge to develop
```

### Machine-Specific Changes
```bash
# Machine-specific branch
git checkout develop
git checkout -b machine/sleeper-service

# Test on specific machine
sudo nixos-rebuild test --flake .#sleeper-service

# Commit and merge
git add .
git commit -m "feat(sleeper-service): add NFS server configuration"
```

### Hotfix Process
```bash
# Critical fix needed
git checkout main
git checkout -b hotfix/zfs-boot-failure

# Fix the issue
# ... emergency fix ...
sudo nixos-rebuild test --flake .#congenital-optimist

# Deploy to main
git add .
git commit -m "fix: resolve ZFS boot failure"
git checkout main
git merge hotfix/zfs-boot-failure
git tag v1.0.1

# Backport to develop
git checkout develop
git merge hotfix/zfs-boot-failure
```

## 📋 Commit Convention

### Format
```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

### Types
- **feat**: New feature or module
- **fix**: Bug fix
- **docs**: Documentation changes
- **style**: Formatting, missing semicolons, etc.
- **refactor**: Code refactoring
- **test**: Adding tests
- **chore**: Maintenance tasks

### Scopes
- **machine**: `(congenital-optimist)`, `(sleeper-service)`
- **module**: `(desktop)`, `(virtualization)`, `(users)`
- **config**: `(flake)`, `(ci)`
- **docs**: `(readme)`, `(plan)`

### Examples
```bash
feat(desktop): add Cosmic desktop environment module
fix(virtualization): resolve Incus networking issues
docs(readme): update installation instructions
refactor(modules): reorganize desktop environment modules
chore(ci): update GitHub Actions workflow
```

## 🛡️ Branch Protection Rules

### Main Branch Protection
- **Required Reviews**: 1 reviewer minimum
- **Status Checks**: All CI checks must pass
- **Up-to-date**: Branch must be up to date before merging
- **Admin Override**: Allow admin override for hotfixes
- **Force Push**: Disabled
- **Deletion**: Disabled

### Develop Branch Protection
- **Required Reviews**: 1 reviewer (can be self-review)
- **Status Checks**: All CI checks must pass
- **Auto-merge**: Allow auto-merge after checks
- **Force Push**: Disabled for others

## 🔄 Merge Strategies

### Feature to Develop
- **Strategy**: Squash and merge
- **Reason**: Clean history, single commit per feature
- **Title**: Use conventional commit format

### Develop to Main
- **Strategy**: Merge commit
- **Reason**: Preserve feature branch history
- **Testing**: Full integration testing required

### Hotfix to Main
- **Strategy**: Fast-forward if possible
- **Reason**: Immediate deployment needed
- **Testing**: Minimal but critical testing

## 🚀 Deployment Strategy

### Automatic Deployment
- **main** → Production machines (congenital-optimist, sleeper-service)
- **develop** → Test environment (if available)

### Manual Deployment
- Feature branches can be manually deployed for testing
- Use `nixos-rebuild test` for non-persistent testing
- Use `nixos-rebuild switch` for persistent changes

### Rollback Strategy
```bash
# Rollback to previous version
git checkout main
git revert <commit-hash>
git tag rollback-v1.0.0-to-v0.9.9

# Or rollback to specific tag
git checkout v1.0.0
sudo nixos-rebuild switch --flake .#congenital-optimist
```

## 📊 Branch Lifecycle

### Weekly Maintenance
- **Monday**: Review open feature branches
- **Wednesday**: Merge develop to main if stable
- **Friday**: Clean up merged feature branches
- **Sunday**: Update dependencies (automated)

### Monthly Tasks
- Review and update branch protection rules
- Clean up old tags and releases
- Update documentation
- Security audit of configurations

## 🎯 Best Practices

### Branch Naming
- Use descriptive names: `feature/improve-zfs-performance`
- Include issue numbers: `feature/123-add-cosmic-desktop`
- Use lowercase with hyphens
- Keep names under 50 characters

### Commit Messages
- Use imperative mood: "add", "fix", "update"
- Keep first line under 50 characters
- Include body for complex changes
- Reference issues: "Fixes #123"

### Testing Requirements
- Always run `nix flake check` before committing
- Test with `nixos-rebuild test` on relevant machines
- Document testing performed in PR description
- Consider impact on other machines

### Code Review
- Focus on configuration correctness
- Check for security implications
- Verify documentation updates
- Ensure rollback plan exists
- Test locally when possible

---

This branching strategy ensures stable, tested configurations while enabling rapid development and emergency fixes when needed.
