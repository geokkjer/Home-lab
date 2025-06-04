## 🏠 Home Lab Configuration Change

### 📋 Description
<!-- Describe what this PR does and why -->

### 🎯 Type of Change
<!-- Mark all that apply -->
- [ ] 🐛 Bug fix (non-breaking change that fixes an issue)
- [ ] ✨ New feature (non-breaking change that adds functionality)
- [ ] 💥 Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] 📚 Documentation update
- [ ] 🔧 Configuration change
- [ ] 🏗️ Infrastructure change
- [ ] 🔒 Security update

### 🖥️ Affected Machines
<!-- Mark all machines affected by this change -->
- [ ] `congenital-optimist` (AMD workstation)
- [ ] `sleeper-service` (Intel file server)
- [ ] Both machines
- [ ] New machine configuration

### 🧪 Testing Performed
<!-- Describe how you tested these changes -->
- [ ] `nix flake check` passes
- [ ] `nixos-rebuild test --flake` successful
- [ ] `nixos-rebuild build --flake` successful
- [ ] Manual testing of affected functionality
- [ ] Rollback tested (if applicable)

### 📝 Testing Checklist
<!-- Check all items that were verified -->
#### System Functionality
- [ ] System boots successfully
- [ ] Network connectivity works
- [ ] Services start correctly
- [ ] No error messages in logs

#### Desktop Environment (if applicable)
- [ ] Desktop environment launches
- [ ] Applications start correctly
- [ ] Hardware acceleration works
- [ ] Audio/video functional

#### Virtualization (if applicable)
- [ ] Incus containers work
- [ ] Libvirt VMs functional
- [ ] Podman containers operational
- [ ] Network isolation correct

#### Development Environment (if applicable)
- [ ] Editors launch correctly
- [ ] Language servers work
- [ ] Build tools functional
- [ ] Git configuration correct

#### File Services (if applicable)
- [ ] NFS mounts accessible
- [ ] Samba shares working
- [ ] Backup services operational
- [ ] Storage pools healthy

### 🔒 Security Considerations
<!-- Any security implications of this change -->
- [ ] No new attack vectors introduced
- [ ] Secrets properly managed
- [ ] Firewall rules reviewed
- [ ] User permissions appropriate

### 📖 Documentation
<!-- Documentation changes -->
- [ ] README.md updated (if needed)
- [ ] Module documentation updated
- [ ] plan.md updated (if needed)
- [ ] Comments added to complex configurations

### 🔄 Rollback Plan
<!-- How to rollback if something goes wrong -->
- [ ] Previous configuration saved
- [ ] ZFS snapshot created
- [ ] Rollback procedure documented
- [ ] Emergency access method available

### 📋 Deployment Notes
<!-- Special considerations for deployment -->
- [ ] No special deployment steps required
- [ ] Requires manual intervention: <!-- describe -->
- [ ] Needs coordination with other changes
- [ ] Breaking change requires communication

### 🔗 Related Issues
<!-- Link any related issues -->
Fixes #<!-- issue number -->
Related to #<!-- issue number -->

### 📸 Screenshots/Logs
<!-- Add any relevant screenshots or log outputs -->

### ✅ Final Checklist
<!-- Verify before submitting -->
- [ ] I have tested this change locally
- [ ] I have updated documentation as needed
- [ ] I have considered the impact on other machines
- [ ] I have verified the rollback plan
- [ ] I have checked for any secrets in the code
- [ ] This change follows the repository's coding standards

### 🧠 Additional Context
<!-- Add any other context about the PR here -->

---

**Reviewer Guidelines:**
1. Verify all testing checkboxes are complete
2. Review configuration changes for security implications
3. Ensure rollback plan is realistic
4. Check that documentation is updated
5. Validate CI pipeline passes
