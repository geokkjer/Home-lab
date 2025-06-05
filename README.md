# 🏠 NixOS Home Lab Adventures

[![NixOS](https://img.shields.io/badge/NixOS-25.05-blue.svg)](https://nixos.org/)
[![Flakes](https://img.shields.io/badge/Nix-Flakes-green.svg)](https://nixos.wiki/wiki/Flakes)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

A personal journey into NixOS flakes and home lab tinkering. This is my playground for learning declarative system configuration and building a multi-machine setup that's both fun and functional.

## 🚀 Getting Started

Want to try this out? Here's how to get rolling:

```bash
# Grab the repo
git clone <repository-url> Home-lab
cd Home-lab

# Make sure everything looks good
nix flake check

# Test it out (won't mess with your current setup)
sudo nixos-rebuild test --flake .#congenital-optimist

# If you're happy with it, make it permanent
sudo nixos-rebuild switch --flake .#congenital-optimist
```

## 🏗️ What We're Working With

### The Machines
- **`congenital-optimist`** - My main AMD Threadripper beast for development and experimentation
- **`sleeper-service`** - Intel Xeon E3-1230 V2 running file server duties (the quiet workhorse)

### The Stack
- **OS**: NixOS 25.05 (Warbler) - because reproducible builds are beautiful
- **Configuration**: Nix Flakes with modular approach - keeping things organized
- **Virtualization**: Incus, Libvirt/QEMU, Podman - gotta test stuff somewhere
- **Desktop**: GNOME, Cosmic, Sway - variety is the spice of life
- **Storage**: ZFS with snapshots and NFS - never lose data again
- **Network**: Tailscale mesh - because VPNs should just work

## 📁 How It's Organized

Everything's broken down into logical chunks to keep things manageable:

```
Home-lab/
├── flake.nix              # Main flake configuration
├── flake.lock             # Locked dependency versions
├── machines/              # Machine-specific configurations
│   ├── congenital-optimist/  # AMD workstation
│   └── sleeper-service/      # Intel file server
├── modules/               # Reusable NixOS modules
│   ├── common/           # Shared system configuration
│   ├── desktop/          # Desktop environment modules
│   ├── development/      # Development tools and editors
│   ├── hardware/         # Hardware-specific configurations
│   ├── services/         # Service configurations
│   ├── system/           # Core system modules
│   ├── users/            # User configurations
│   └── virtualization/   # Container and VM setup
├── users/                # User-specific configurations
│   └── geir/            # Primary user configuration
│       ├── dotfiles/    # Literate configuration with org-mode
│       └── user.nix     # System-level user config
├── overlays/             # Nix package overlays
├── packages/             # Custom package definitions
└── secrets/              # Encrypted secrets (future)
```

## 🔧 How I Manage This Chaos

### Keeping Things Modular
I've split everything into focused modules so I don't go insane:

- **Desktop Environments**: Each DE gets its own module - no more giant config files
- **Virtualization**: Separate configs for Incus, Libvirt, and Podman - mix and match as needed
- **Development**: Modular tool setups for different workflows - because context switching is real
- **Hardware**: Hardware-specific tweaks and drivers - make the silicon sing

### Literate Programming (Because Documentation Matters)
My user configs live in Emacs org-mode files - it's like having your documentation and code hold hands:
- Configuration files that explain themselves
- Automatic tangling from `.org` files to actual configs
- Git tracks both the code and the reasoning behind it

## 🚀 My Workflow

### Tinkering Locally
```bash
# Check if I broke anything
nix flake check

# Test changes without committing to them
sudo nixos-rebuild test --flake .#<machine-name>

# Build and see what happens
sudo nixos-rebuild build --flake .#<machine-name>

# Ship it!
sudo nixos-rebuild switch --flake .#<machine-name>
```

### Git-Driven Chaos (In a Good Way)
1. **Feature Branch**: New idea? New branch.
2. **Local Testing**: Break things safely with `nixos-rebuild test`
3. **Pull Request**: Show off the changes
4. **Review**: Someone sanity-checks my work
5. **Deploy**: Either automated or "click the button"

## 🔐 Secrets and Security

### Current Reality
- No secrets in git (obviously)
- Manual secret juggling during setup (it's fine, really)
- ZFS encryption for the important stuff

### Future Dreams
- **agenix** or **sops-nix** for proper secret management
- **age** keys for encryption magic
- **CI/CD** that doesn't leak passwords everywhere

## 🎯 The Hardware

### CongenitalOptimist (The Workstation)
- **CPU**: AMD Threadripper (check hardware-configuration.nix for the gory details)
- **GPU**: AMD (with proper drivers and GPU passthrough for VMs)
- **Storage**: ZFS pools (zpool for system, stuffpool for data hoarding)
- **Role**: Main development machine, VM playground, desktop environment testing ground
- **Services**: Whatever I'm experimenting with this week

### SleeperService (The Quiet One)
- **CPU**: Intel Xeon E3-1230 V2 @ 3.70GHz (4 cores, 8 threads - still plenty peppy)
- **Memory**: 16GB RAM (enough for file serving duties)
- **Storage**: ZFS with redundancy (because data loss is sadness)
- **Role**: Network storage, file sharing, backup duties, monitoring the other machines
- **Services**: NFS, Samba, automated backups, keeping an eye on things

## 🧪 Testing (The "Does It Work?" Phase)

### Automated Testing (Someday Soon)
- **Configuration Validation**: `nix flake check` in CI - catch dumb mistakes early
- **Build Testing**: Test builds for all machines - make sure nothing's broken
- **Module Testing**: Individual module validation - each piece should work alone
- **Integration Testing**: Full system builds - the moment of truth

### My Manual Testing Ritual
- [ ] System actually boots (surprisingly important)
- [ ] Desktop environments don't crash immediately
- [ ] VMs and containers start up
- [ ] Network services respond
- [ ] Development environment loads
- [ ] Can actually get work done

## 📈 Keeping Things Running

### Health Checks (The Boring But Important Stuff)
- Generation switching (did the update work?)
- Service status monitoring (what's broken now?)
- ZFS pool health (happy disks = happy life)
- Network connectivity (can I reach the internet?)
- Resource usage (is something eating all my RAM?)

### Backup Strategy (Paranoia Pays Off)
- **ZFS Snapshots**: Automatic filesystem snapshots - time travel for your data
- **Configuration Backups**: Git repo with full history - every mistake preserved for posterity
- **Data Backups**: Automated services on SleeperService - redundancy is key
- **Recovery Procedures**: Documented rollback processes - for when everything goes sideways

## 🔄 CI/CD Dreams (Work in Progress)

### Validation Pipeline (The Plan)
```yaml
# What I want GitHub Actions to do
- Syntax Check: nix flake check  # Catch the obvious stuff
- Build Test: nix build .#nixosConfigurations.<machine>  # Does it actually build?
- Security Scan: Nix security auditing  # Keep the bad guys out
- Documentation: Update system docs  # Because future me will forget
```

### Deployment Pipeline (The Dream)
```yaml
# Automated deployment magic
- Staging: Deploy to test environment  # Break things safely
- Integration Tests: Automated system testing  # Does everything still work?
- Production: Deploy to production machines  # The moment of truth
- Rollback: Automatic rollback on failure  # When things go wrong (they will)
```

## 🤝 Want to Contribute?

### How to Jump In
1. Fork or clone the repo
2. Create a feature branch for your idea
3. Make your changes
4. Test locally with `nixos-rebuild test` (don't break my machine)
5. Submit a pull request
6. Chat about it in the review
7. Merge when we're both happy

### Module Development Tips
- Keep modules focused - one job, do it well
- Document what your module does and how to use it
- Test modules independently when you can
- Use consistent naming (future you will thank you)
- Include example configurations for others

## 📖 Documentation

- **[Plan](plan.md)**: The grand vision and migration roadmap
- **[Instructions](instruction.md)**: Step-by-step setup and AI agent guidance
- **[Machine Documentation](machines/)**: Individual machine configs and notes
- **[Module Documentation](modules/)**: How each module works
- **[User Documentation](users/)**: User-specific configuration details

## 🎯 The Journey So Far

### Phase 1: Flakes Migration ✅
- [x] Converted to flake-based configuration (no more channels!)
- [x] Modularized desktop environments (sanity preserved)
- [x] Added comprehensive virtualization (all the containers)
- [x] Set up GitOps foundation (git-driven everything)

### Phase 2: Configuration Cleanup (In Progress)
- [ ] Optimize modular structure (make it even better)
- [ ] Enhance documentation (explain the magic)
- [ ] Standardize module interfaces (consistency is king)

### Phase 3: Multi-Machine Expansion (Coming Soon)
- [ ] Add SleeperService configuration (wake up the sleeper)
- [ ] Implement service modules (automate all the things)
- [ ] Set up network storage (centralized data paradise)

### Phase 4: Automation & CI/CD (The Dream)
- [ ] Implement automated testing (catch problems early)
- [ ] Set up deployment pipelines (one-click deploys)
- [ ] Add monitoring and alerting (know when things break)

### Phase 5: Advanced Features (Future Fun)
- [ ] Secrets management (proper secret handling)
- [ ] Advanced monitoring (graphs and dashboards)
- [ ] Backup automation (paranoia made easy)

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details. Feel free to steal ideas, improve things, or just poke around.

## 🙏 Thanks

- **NixOS Community** for excellent docs and endless patience with newbie questions
- **Culture Ship Names** for inspiring machine nomenclature (because why not?)
- **Emacs Community** for literate programming inspiration and org-mode magic
- **Home Lab Community** for sharing knowledge, war stories, and "it works on my machine" solutions

---

*"The ship had decided to call itself the Arbitrary, presumably for much the same reason."*
