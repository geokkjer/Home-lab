<div align="center">
  <img src="assets/nixos_logo.svg" alt="NixOS Logo" width="120" height="150">
  
# NixOS Home Lab Infrastructure
  
  [![NixOS](https://img.shields.io/badge/NixOS-25.05-blue.svg)](https://nixos.org/)
  [![Flakes](https://img.shields.io/badge/Nix-Flakes-green.svg)](https://nixos.wiki/wiki/Flakes)
  [![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
</div>

AI-integrated NixOS home lab infrastructure with Guile Scheme automation tools. Features declarative system configuration, Task Master AI integration, local Ollama processing, and MCP protocol implementation for intelligent infrastructure management across 4 fully operational machines.

## AI-Enhanced DevSecOps with Local Intelligence

Advanced home lab featuring AI-powered task management, Guile Scheme automation, and local Ollama inference for complete data privacy and intelligent infrastructure operations.

## Quick Start

```bash
# Clone repository
git clone <repository-url> Home-lab
cd Home-lab

# Validate configuration
nix flake check

# Test configuration (temporary, reverts on reboot)
sudo nixos-rebuild test --flake .#<machine-name>

# Apply configuration permanently  
sudo nixos-rebuild switch --flake .#<machine-name>
```

## Architecture Overview

### AI-Enhanced Infrastructure

#### Core AI Components
- **Task Master AI** - Intelligent task management with local Ollama inference
- **MCP Protocol Server** - Guile Scheme-based Model Context Protocol implementation
- **Local AI Models** - Qwen3:4b, DeepSeek-R1:1.5b, Gemma3:4b running on grey-area
- **RAG System** - Document retrieval and processing for enhanced AI context
- **VS Code Integration** - AI-powered development environment with GitHub Copilot

### Machine Infrastructure (All Operational)

- **congenital-optimist** - AI-enhanced development workstation with multiple desktop environments
- **sleeper-service** - ZFS file server with 903GB media library and NFS services
- **grey-area** - AI processing hub with Ollama, containerized services, and Forgejo Git hosting
- **reverse-proxy** - External gateway with SSL termination, service routing (git.geokkjer.eu)

### Technology Stack

- **Base OS**: <img src="assets/nixos_logo.svg" alt="NixOS" width="20" height="25" style="vertical-align: middle; margin-right: 5px;">NixOS 25.05 with Nix Flakes
- **Configuration**: Modular, declarative system configuration
- **AI Integration**: Task Master AI with local Ollama inference (Qwen, DeepSeek, Gemma models)
- **Automation**: Guile Scheme-based infrastructure tools with MCP protocol
- **Development**: VS Code extension with GitHub Copilot integration
- **Virtualization**: Incus containers, Libvirt/QEMU VMs, Podman containers
- **Desktop**: GNOME, Cosmic, Sway window managers
- **Storage**: ZFS with snapshots, automated mounting, NFS network storage (903GB media library)
- **Network**: Tailscale mesh VPN with centralized hostname resolution
- **Services**: Forgejo Git hosting, Jellyfin media server, RAG document processing

## Project Structure

Modular configuration organized for scalability and maintainability:

```
Home-lab/
├── flake.nix              # Main flake configuration
├── flake.lock             # Dependency lock file
├── .taskmaster/           # Task Master AI configuration and tasks
│   ├── tasks/            # Task database and generated files
│   ├── config.json       # AI model configuration
│   └── docs/             # PRD and documentation
├── machines/              # Machine-specific configurations
│   ├── congenital-optimist/  # AI-enhanced workstation
│   ├── sleeper-service/   # File server with 903GB media
│   ├── grey-area/        # AI processing and application server
│   └── reverse-proxy/    # External gateway (git.geokkjer.eu)
├── modules/               # Reusable NixOS modules
│   ├── common/           # Base system configuration
│   ├── ai/               # Claude Code and AI tools
│   ├── desktop/          # Desktop environment modules
│   ├── development/      # Development tools
│   ├── services/         # Service configurations (Ollama, RAG, etc.)
│   ├── users/            # User management
│   └── virtualization/   # Container and VM setup
├── packages/             # Custom packages and tools
├── research/             # AI integration research and analysis
├── documentation/        # Comprehensive project documentation
└── scripts/              # Automation and utility scripts
```

## Configuration Philosophy

<img src="assets/nixos_logo.svg" alt="NixOS" width="30" height="37" style="float: left; margin-right: 10px; margin-top: 5px;">

### AI-Enhanced Modular Design

- **Intelligent Orchestration**: Task Master AI manages complex deployment workflows
- **Local AI Processing**: Complete data privacy with Ollama inference on local hardware
- **Composable Modules**: Mix and match modules per machine requirements with AI assistance
- **Testable Infrastructure**: Individual modules validated independently with automated testing
- **Living Documentation**: AI-generated documentation that evolves with the infrastructure

### User Management Strategy

- **Role-based Users**: Separate users for desktop vs server administration
- **Centralized Configuration**: Consistent user setup across all machines
- **Security Focus**: SSH key management and privilege separation
- **Literate Dotfiles**: Org-mode documentation for complex configurations

### Network Architecture

- **Mesh VPN**: Tailscale for secure inter-machine communication
- **Service Discovery**: Centralized hostname resolution
- **Firewall Management**: Service-specific port configuration
- **External Access**: Reverse proxy with SSL termination

## Development Workflow

### Local Testing

```bash
# Validate configuration syntax
nix flake check

# Build without applying changes
nix build .#nixosConfigurations.<machine>.config.system.build.toplevel

# Test configuration (temporary)
sudo nixos-rebuild test --flake .#<machine>

# Apply configuration permanently
sudo nixos-rebuild switch --flake .#<machine>
```

### Git Workflow

1. **Feature Branch**: Create branch for configuration changes
2. **Local Testing**: Validate changes with `nixos-rebuild test`
3. **Pull Request**: Submit changes for review
4. **Deploy**: Apply configuration to target machines

### Remote Deployment

- **SSH-based**: Remote deployment via secure shell
- **Atomic Updates**: Complete success or automatic rollback
- **Health Checks**: Service validation after deployment
- **Centralized Management**: Single repository for all infrastructure

## Service Architecture

### Core Services - Fully Operational Stack

#### AI & Automation Services
- **Task Master AI**: Intelligent task management with 31 completed infrastructure tasks
- **Ollama AI Processing**: Local inference on grey-area (Qwen3, DeepSeek, Gemma models)
- **MCP Server**: Guile Scheme-based Model Context Protocol implementation
- **RAG System**: Document retrieval and processing for enhanced AI context

#### Infrastructure Services
- **Forgejo Git Hosting**: Self-hosted at git.geokkjer.eu with external SSH access (port 1337)
- **Jellyfin Media Server**: Streaming with transcoding, accessing 903GB library via NFS
- **File Storage**: NFS network storage with ZFS snapshots (sleeper-service)
- **Reverse Proxy**: Nginx with Let's Encrypt SSL and external access
- **Container Platform**: Podman-based services across grey-area
- **Additional Services**: Calibre-web (e-books), audiobook server, web applications

### Service Discovery

- **Internal DNS**: Tailscale for mesh network resolution
- **External DNS**: Public domain with SSL certificates
- **Service Mesh**: Inter-service communication via secure network
- **Load Balancing**: Traffic distribution and failover

### Data Management

- **ZFS Storage**: Copy-on-write filesystem with snapshots
- **Network Shares**: NFS for cross-machine file access
- **Backup Strategy**: Automated snapshots and external backup
- **Data Integrity**: Checksums and redundancy

## Security Model

### Network Security

- **VPN Mesh**: All inter-machine traffic via Tailscale
- **Firewall Rules**: Service-specific port restrictions
- **SSH Hardening**: Key-based authentication only
- **Fail2ban**: Automated intrusion prevention

### User Security

- **Role Separation**: Administrative vs daily-use accounts
- **Key Management**: Centralized SSH key distribution
- **Privilege Escalation**: Sudo access only where needed
- **Service Accounts**: Dedicated accounts for automated services

### Infrastructure Security

- **Configuration as Code**: All changes tracked in version control
- **Atomic Deployments**: Rollback capability for failed changes
- **Secret Management**: Encrypted secrets with controlled access
- **Security Updates**: Regular dependency updates

## Testing Strategy

### Automated Testing

- **Syntax Validation**: Nix flake syntax checking
- **Build Testing**: Configuration build verification
- **Module Testing**: Individual component validation
- **Integration Testing**: Full system deployment tests

### Manual Testing

- **Boot Validation**: System startup verification
- **Service Health**: Application functionality checks
- **Network Connectivity**: Inter-service communication tests
- **User Environment**: Desktop and development tool validation

## Deployment Status

### Infrastructure Maturity - FULLY OPERATIONAL 🚀

- ✅ **AI-Integrated Management**: Task Master AI with 31 completed tasks, Guile Scheme MCP server
- ✅ **Multi-machine Configuration**: 4 machines fully deployed and operational
- ✅ **Local AI Processing**: Ollama inference on grey-area with multiple models
- ✅ **Service Integration**: Forgejo Git hosting (git.geokkjer.eu), Jellyfin media server, file storage
- ✅ **Network Mesh**: Secure Tailscale VPN with centralized hostname resolution
- ✅ **External Access**: Public services with SSL termination and SSH forwarding
- ✅ **Development Tools**: VS Code extension with MCP integration for AI-assisted development
- ✅ **Centralized Management**: Single repository with intelligent task orchestration

### Current Capabilities

- **AI-Powered Development**: Task Master AI with local Ollama inference for intelligent task management
- **Advanced Automation**: Guile Scheme-based tools replacing traditional Bash scripts
- **MCP Integration**: Model Context Protocol server for AI tool communication
- **Development Environment**: Full IDE setup with AI-enhanced workflows and multiple desktop options
- **File Services**: Network storage with 903GB media library (38 movies, 29 TV series, 79 audiobooks)
- **Git Hosting**: Self-hosted Forgejo with external access via git.geokkjer.eu
- **Media Streaming**: Jellyfin with transcoding, Calibre-web, audiobook server
- **Container Platform**: Podman-based containerized services across grey-area
- **RAG System**: Document retrieval and processing for enhanced AI context

## Documentation

- **[Migration Plan](plan.md)**: Detailed implementation roadmap with current operational status
- **[Task Master Integration](research/claude-task-master-ai-integration-status.md)**: AI-powered task management setup
- **[Ollama Research](research/taskmaster-ai.md)**: Local AI processing with privacy-first approach
- **[Guile Development](research/guile-configuration-strategy.md)**: Scheme-based automation tools
- **[RAG System](research/RAG-MCP-TaskMaster-Domain-Model.md)**: Document processing and retrieval
- **[Development Workflow](documentation/DEVELOPMENT_WORKFLOW.md)**: AI-enhanced contribution guidelines
- **[AI Instructions](instruction.md)**: Agent guidance for intelligent system management

## License

MIT License - see [LICENSE](LICENSE) for details.

## Current Status Summary

### ✅ Fully Operational AI-Enhanced Home Lab (December 2024 - June 2025)

#### Infrastructure Achievement
- **4/4 machines deployed** and fully operational with complete service integration
- **Task Master AI** managing infrastructure with 31 completed automation tasks
- **Local AI processing** with Ollama models providing complete data privacy
- **External services** accessible via git.geokkjer.eu with SSL termination

#### AI Integration Milestones
- **MCP Protocol Server** implemented in Guile Scheme for AI tool communication
- **VS Code Integration** with GitHub Copilot and local AI models
- **RAG System** for intelligent document processing and retrieval
- **Intelligent Task Management** with automated project breakdown and tracking

#### Service Stack Status
- **Git Hosting**: Forgejo operational at git.geokkjer.eu
- **Media Services**: Jellyfin, Calibre-web, audiobook server with 903GB library
- **AI Processing**: Ollama inference with Qwen3, DeepSeek, Gemma models
- **Network Mesh**: Tailscale VPN connecting all infrastructure securely
- **Storage**: ZFS with NFS exports serving content across the network

This represents a **comprehensive evolution** from traditional infrastructure management to **AI-enhanced, privacy-focused automation** while maintaining full control over data and processing.

---

<div align="center">
  <img src="assets/nixos_logo.svg" alt="NixOS Logo" width="40" height="50">
  <br>
  <em>AI-enhanced infrastructure designed for intelligence, privacy, and continuous evolution.</em>
</div>
