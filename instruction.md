# AI Agent General Instructions

## Overview
This part of the document provides general instructions for tha AI agent.

## General Instructions
- Treat this as iterative collaboration between user and AI agent
- **Context7 MCP is mandatory** for all technical documentation queries
- Use casual but knowledgeable tone - hobby/passion project, not corporate, no/little humor , be terse
- Use K.I.S.S priciples in both code and written languageS
- Update documentation frequently as project evolves

## Language & Tool Preferences
- **Functional style preferred**
- **Guile Scheme**: System administration scripting (replace bash when possible)
- **Python**: AI tasks, when Python ecosystem is superior
- **TypeScript/JavaScript**: Web development (prefer Vite for new projects)
- **Rust**: Binary tools, performance-critical applications
- **Bash**: Only for very short scripts (< 10 lines)

## Context7 MCP Usage Protocol
1. **Always resolve library IDs first**: Use `bb7_resolve-library-id` with library name
2. **Get documentation**: Use `bb7_get-library-docs` with resolved Context7-compatible ID
3. **For NixOS**: Search for "nixos", "nixpkgs", or specific module names
4. **Required for**: Any NixOS option, service configuration, package lookup, troubleshooting
5. **Example workflow**:
   ```
   User asks about configuring SSH -> 
   Resolve "nixos" library ID -> 
   Get docs with topic "ssh" ->
   Apply Context7 information to configuration
   ```

## Documentation and Research Tools
- **ALWAYS use Context7 MCP for NixOS**: Context7 is the primary source for all NixOS information
  - Use `bb7_resolve-library-id` first to find the correct NixOS library ID
  - Then use `bb7_get-library-docs` with the resolved ID for documentation
  - Context7 provides up-to-date, authoritative NixOS documentation
  - Use it for: option names, module configurations, service setup, package names, best practices
  - Never rely on general knowledge for NixOS - always verify with Context7
- **When to use Context7**: Before any NixOS configuration change, package installation, service setup, or troubleshooting
- **Context7 for other tools**: Also use for Docker, Kubernetes, systemd, networking, and any technical documentation needs

## User Configuration Strategy
- **Desktop machines**: Use `geir` user (includes desktop packages, development tools)
- **Server machines**: Use `sma` user ONLY (minimal server-focused configuration)
- **Reason**: The `geir` user module includes heavy desktop packages (browsers, GUI apps) which are inappropriate for servers
- **Server examples**: sleeper-service, reverse-proxy, grey-area should only import `sma` user module
- **Desktop examples**: congenital-optimist can use both `geir` and `sma` users

# AI Agent Instructions: NixOS Home Lab Management

## System Information
- **Current**: NixOS 25.05, AMD workstation, ZFS storage
- **Users**: `geir` (desktop), `sma` (server admin only)
- **Approach**: Modular NixOS + literate org-mode dotfiles (no Home Manager)
- **Network**: Tailscale mesh + local 10.0.0.0/24

## Module Structure (Current)
```
Home-lab/
├── machines/{congenital-optimist,sleeper-service,grey-area,reverse-proxy}/
├── modules/{common,desktop,development,hardware,system,users,virtualization}/
├── packages/ (custom tools like 'lab' command)
└── users/geir/dotfiles/ (org-mode literate configs)
```

## Essential Workflows

### Before Any NixOS Changes
1. **Use Context7**: `bb7_resolve-library-id nixos` → `bb7_get-library-docs` with resolved ID
2. **Stage changes in git**: `git add .` (ensures Nix can access all files)
3. **Check current state**: `nix flake check` and verify system health
4. **Test build**: `nix build .#nixosConfigurations.<machine>.config.system.build.toplevel`

### Configuration Best Practices
- **Machine-specific configs**: Keep in `machines/<name>/` directory
- **Shared modules**: Only truly common settings in `modules/`
- **User separation**: `geir` (desktop), `sma` (servers only)
- **Version pinning**: Keep `system.stateVersion = "23.11"` (DO NOT CHANGE)

### Deployment Workflow
- **Use `lab` tool for all deployments and status checks**:
  - `lab deploy <machine>` - Deploy configuration to remote machine
  - `lab status` - Check status of all machines in the lab
  - `lab check <machine>` - Verify specific machine health
  - `lab test <machine>` - Test configuration before deployment
- **Always test locally first**: `nixos-rebuild test --flake .#<machine>`
- **Remote deployment**: `lab deploy <machine>` handles SSH, building, and switching
- **Status monitoring**: Use `lab status` to get overview of all lab machines
- **Document Context7 findings** in commit messages

### Git/Forgejo Configuration
- **Primary repository**: Hosted on self-hosted Forgejo instance
- **Forgejo URL**: `ssh://forgejo@git.geokkjer.eu:1337/geir/Home-lab.git`
- **SSH port**: 1337 (proxied through reverse-proxy to grey-area:22)
- **User**: Must use `forgejo` user, not `git` user
- **GitHub mirror**: `git@github.com:geokkjer/Home-lab.git` (secondary/backup)
- **Remote configuration**:
  ```bash
  git remote add origin ssh://forgejo@git.geokkjer.eu:1337/geir/Home-lab.git
  git remote add github git@github.com:geokkjer/Home-lab.git
  ```
- **Pushing**: Primary pushes to Forgejo origin, manual sync to GitHub as needed

## Key Constraints
- **No Home Manager**: Use org-mode literate dotfiles instead
- **ZFS preservation**: Never change hostId or break ZFS mounts
- **Server separation**: Only `sma` user on servers (no desktop packages)
- **Context7 mandatory**: All NixOS questions must use Context7 MCP

## Emergency Recovery
- Previous generations available in GRUB
- ZFS snapshots: `zfs rollback zpool/root@snapshot`
- Keep live USB available