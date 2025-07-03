# AI Coding Assistant Customization Guide

This comprehensive guide explains how to personalize and focus your AI coding experience using GitHub Copilot and Claude Code. It covers custom prompts, memory files, instructions, and advanced personalization techniques for both tools.

---

## Table of Contents

- [Overview](#overview)
- [GitHub Copilot Customization](#github-copilot-customization)
  - [Personal Custom Instructions](#personal-custom-instructions)
  - [Repository Custom Instructions](#repository-custom-instructions)
  - [Organization Custom Instructions](#organization-custom-instructions)
  - [Prompt Files Memory and Reusable Prompts](#prompt-files-memory-and-reusable-prompts)
- [Claude Code Customization](#claude-code-customization)
  - [Memory Files CLAUDE.md](#memory-files-claudemd)
  - [Custom Commands](#custom-commands)
  - [Settings and Configuration](#settings-and-configuration)
  - [System Prompts](#system-prompts)
- [Configuration Instructions](#configuration-instructions)
- [Advanced Prompt Engineering](#advanced-prompt-engineering)
- [Best Practices](#best-practices)
- [Examples and Templates](#examples-and-templates)
- [References](#references)

---

## Overview

Modern AI coding assistants like GitHub Copilot and Claude Code can be significantly enhanced through customization. By providing contextual information, custom instructions, and reusable prompts, you can:

- Improve the relevance and quality of suggestions
- Reduce repetitive prompt writing
- Establish team/project/organization-wide standards
- Create personalized workflows that match your coding style
- Build a knowledge base that grows with your projects

---

## GitHub Copilot Customization

GitHub Copilot supports several customization approaches that work across different environments.

### Personal Custom Instructions

Personal instructions apply globally to all your Copilot Chat conversations.

**Use cases:**
- Language preferences: `Always respond in Norwegian`
- Code style preferences: `Prefer TypeScript over JavaScript`
- Explanation style: `Be concise and use bullet points`

**Configuration:**
1. Open VS Code settings (Ctrl/Cmd + ,)
2. Search for "copilot chat"
3. Find "Personal Custom Instructions"
4. Add your preferences

### Repository Custom Instructions

Repository instructions apply to all conversations within a specific repository context.

**Use cases:**
- Project coding standards: `Use early returns in all functions`
- Framework specifications: `Use Vue with PrimeVue library`
- Testing requirements: `Write tests using Jest and React Testing Library`

**Setup:**
Create `.github/copilot-instructions.md` in your repository root:

```md
We use Bazel for Java dependencies, not Maven.
Always use double quotes and tabs for JavaScript.
Our team uses Jira for tracking work items.
All API endpoints must include error handling.
```

### Organization Custom Instructions

*(Enterprise only)* Apply to all conversations within an organization.

**Use cases:**
- Security guidelines: `For security questions, consult the Security Docs Knowledge Base`
- Company language: `Always respond in Portuguese`
- Code standards: `Do not generate code blocks in responses`

### Prompt Files Memory and Reusable Prompts

Prompt files are Markdown files (`*.prompt.md`) containing reusable instructions.

**Setup:**
1. Create a prompts directory (e.g., `.github/prompts/`)
2. Add VS Code settings:

```json
{
  "chat.promptFiles": true,
  "chat.promptFilesLocations": {
    "default": ["./.github/prompts"]
  }
}
```

**Example prompt file** (`security-review.prompt.md`):

```md
Security Code Review Checklist:
- Validate all user inputs
- Check for SQL injection vulnerabilities
- Verify authentication and authorization
- Review error handling (no sensitive data exposure)
- Confirm HTTPS usage for data transmission
- Check for proper session management
```

---

## Claude Code Customization

Claude Code is Anthropic's terminal-based AI coding assistant that offers powerful customization through memory files, custom commands, and configuration options.

### Memory Files CLAUDE.md

The `CLAUDE.md` file serves as the project's memory, storing context, conventions, and instructions.

**Initialize a project memory file:**

```bash
> /init
```

**Add quick memory entries:**

```bash
# Always use descriptive variable names
```

**Example CLAUDE.md structure:**

```md
# Project Context
This is a NixOS home lab configuration managing multiple servers.

# Coding Standards
- Use descriptive variable names
- Write comprehensive documentation
- Follow NixOS conventions for module structure

# Architecture
- Flake-based configuration
- Modular design with separate machine configs
- Common modules for shared functionality

# Summary Instructions
When using compact mode, focus on configuration changes and error outputs.

# Individual Preferences
- @~/.claude/my-homelab-instructions.md
```

### Custom Commands

Claude Code supports both project-specific and personal slash commands.

**Project commands** (available in current project):

```bash
mkdir -p .claude/commands
echo "Review this NixOS configuration for security issues:" > .claude/commands/nixos-security.md
```

Usage: `/project:nixos-security`

**Personal commands** (available across all projects):

```bash
mkdir -p ~/.claude/commands
echo "Review this code for performance optimizations:" > ~/.claude/commands/optimize.md
```

Usage: `/user:optimize`

### Settings and Configuration

Claude Code uses `settings.json` for configuration:

```json
{
  "permissions": {
    "allow": [
      "Bash(nixos-rebuild --dry-run)",
      "Bash(git status)",
      "Read(~/.config/*)"
    ],
    "deny": [
      "Bash(rm -rf *)",
      "Bash(curl *)"
    ]
  },
  "env": {
    "EDITOR": "emacs",
    "CLAUDE_CODE_ENABLE_TELEMETRY": "1"
  },
  "includeCoAuthoredBy": true,
  "cleanupPeriodDays": 30
}
```

**Configuration commands:**
- `claude config list` - List all settings
- `claude config set <key> <value>` - Set a value
- `claude config add <key> <value>` - Add to list
- `claude config remove <key> <value>` - Remove from list

### System Prompts

Claude Code allows custom system prompts for specialized behavior:

```bash
# Override system prompt (print mode only)
claude -p "Build a REST API" --system-prompt "You are a senior backend engineer focused on NixOS deployments"

# Append to existing system prompt
claude -p "Review this config" --append-system-prompt "Focus on NixOS best practices and security"
```

---

## Configuration Instructions

### GitHub Copilot in VS Code

1. **Enable prompt files:**
   ```json
   {
     "chat.promptFiles": true,
     "chat.promptFilesLocations": {
       "default": ["./.github/prompts", "./prompts"]
     }
   }
   ```

2. **Create repository instructions:**
   - Create `.github/copilot-instructions.md`
   - Add project-specific context and standards

3. **Set personal preferences:**
   - Open VS Code settings
   - Navigate to Copilot Chat settings
   - Add personal custom instructions

### Claude Code Setup

1. **Install globally:**
   ```bash
   npm install -g @anthropic-ai/claude-code
   ```

2. **Initialize project:**
   ```bash
   cd your-project
   claude
   > /init
   ```

3. **Configure settings:**
   ```bash
   claude config set permissions.allow "Bash(git status)"
   ```

4. **Create custom commands:**
   ```bash
   mkdir -p .claude/commands
   echo "Your command prompt here" > .claude/commands/command-name.md
   ```

---

## Advanced Prompt Engineering

### Effective Prompt Structure

1. **Start general, then get specific**
2. **Provide examples and context**
3. **Break complex tasks into steps**
4. **Use clear, unambiguous language**
5. **Include relevant code context**

### Template Structure

```md
# Goal
Brief description of what you want to achieve

# Context
- Current tech stack
- Project constraints
- Relevant background

# Requirements
- Specific technical requirements
- Code style preferences
- Testing expectations

# Examples
Show desired input/output format
```

### Long Context Tips

For large codebases or documents, use structured formats:

```md
<documents>
  <document index="1">
    <source>config.nix</source>
    <document_content>
      {{CONFIGURATION_CONTENT}}
    </document_content>
  </document>
</documents>

Analyze the NixOS configuration and suggest improvements.
```

---

## Best Practices

### For Both Tools

1. **Be specific about your environment**
2. **Include relevant file extensions and frameworks**
3. **Use consistent terminology across your team**
4. **Keep instructions concise but comprehensive**
5. **Update instructions as your project evolves**

### GitHub Copilot Specific

1. **Layer instructions logically** (personal → repository → organization)
2. **Use prompt files for reusable tasks**
3. **Test instructions with different conversation contexts**
4. **Avoid conflicting instructions across levels**

### Claude Code Specific

1. **Maintain CLAUDE.md as living documentation**
2. **Use import statements for modular organization**
3. **Set appropriate permissions for security**
4. **Create commands for frequent workflows**
5. **Use memory shortcuts (#) for quick additions**

---

## Examples and Templates

### GitHub Copilot Repository Instructions

```md
# Development Standards
- Use early returns to reduce nesting
- Prefer composition over inheritance
- Write self-documenting code with descriptive names

# Technology Stack
- Frontend: React with TypeScript
- Backend: Node.js with Express
- Database: PostgreSQL with Prisma ORM
- Testing: Jest and React Testing Library

# Code Review Focus
- Security: Validate all inputs, check for XSS/CSRF
- Performance: Avoid N+1 queries, optimize bundle size
- Accessibility: Include ARIA labels, test with screen readers

# Commit Message Format
- Use conventional commits: feat, fix, docs, style, refactor, test, chore
- Include issue number when applicable
```

### Claude Code Memory Template

```md
# Project Overview
Brief description of the project and its goals

# Architecture
- System design overview
- Key components and their relationships
- Data flow and integration points

# Development Workflow
- Branch strategy
- Testing approach
- Deployment process

# Code Standards
- Naming conventions
- File organization
- Documentation requirements

# Common Commands
- Build: `npm run build`
- Test: `npm test`
- Deploy: `npm run deploy`

# Troubleshooting
- Common issues and solutions
- Debug commands
- Log locations
```

### Security Review Prompt File

```md
# Security Code Review

## Authentication & Authorization
- [ ] Verify user authentication is required
- [ ] Check role-based access controls
- [ ] Validate session management

## Input Validation
- [ ] Sanitize all user inputs
- [ ] Check for SQL injection vulnerabilities
- [ ] Validate file upload restrictions

## Data Protection
- [ ] Ensure sensitive data is encrypted
- [ ] Verify HTTPS usage
- [ ] Check for data exposure in logs

## Error Handling
- [ ] No sensitive information in error messages
- [ ] Proper exception handling
- [ ] Secure logging practices
```

---

## References

### GitHub Copilot
- [GitHub Copilot Documentation](https://docs.github.com/en/copilot)
- [Customizing Copilot Chat Responses](https://docs.github.com/en/copilot/concepts/about-customizing-github-copilot-chat-responses)
- [Prompt Engineering for Copilot Chat](https://docs.github.com/en/copilot/concepts/prompt-engineering-for-copilot-chat)

### Claude Code
- [Claude Code Documentation](https://docs.anthropic.com/en/docs/claude-code)
- [Claude Code GitHub Repository](https://github.com/anthropics/claude-code)
- [Anthropic API Documentation](https://docs.anthropic.com/en/api)

### Additional Resources
- [Prompt Engineering Guide](https://github.blog/2023-07-17-prompt-engineering-guide-generative-ai-llms/)
- [AI Coding Best Practices](https://github.blog/2024-03-25-how-to-use-github-copilot-in-your-ide-tips-tricks-and-best-practices/)

---

*This guide is based on official documentation and best practices as of July 2025. Both GitHub Copilot and Claude Code are rapidly evolving tools—check their respective documentation for the latest features and updates.*
