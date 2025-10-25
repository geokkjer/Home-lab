# GPTel Quick Reference Card

## Default Configuration (October 2025)
- **Backend**: GitHub Copilot Chat
- **Model**: Claude Sonnet 4.5 (October 2025 - Latest)
- **Streaming**: Enabled
- **Auth**: GitHub CLI (`gh auth login`)

## Essential Keybindings

| Key | Command | Description |
|-----|---------|-------------|
| `C-c g c` | `gptel` | Start new GPTel chat buffer |
| `C-c g s` | `gptel-send` | Send current region/prompt |
| `C-c g n` | `gptel-menu` | Open GPTel transient menu |
| `C-c g b` | `gptel-switch-backend` | Switch between backends |
| `C-c RET` | Send in buffer | Send message in GPTel buffer |

## Workflow Commands

| Key | Command | Use Case |
|-----|---------|----------|
| `C-c g e` | `gptel-explain-region` | Explain selected code |
| `C-c g v` | `gptel-review-region` | Review code for issues |
| `C-c g x` | `gptel-send-error-context` | Debug error with context |
| `C-c g p` | `gptel-project-context` | Project-specific chat |
| `C-c g m` | `gptel-commit-message` | Generate git commit msg |
| `C-c g t` | `gptel-quick-chat` | Quick query (no buffer) |
| `C-c g f` | `gptel-add-file-context` | Add file to context |

## Available Models (via GitHub Copilot)

### OpenAI Models
- `gpt-4o` - **Default**, most capable, multimodal
- `gpt-4o-mini` - Faster, cheaper, still very capable
- `o1-preview` - Advanced reasoning (preview)
- `o1-mini` - Faster reasoning model

### Anthropic Models
- `claude-3.5-sonnet` - Latest Claude, excellent coding
- `claude-3-opus` - Most capable Claude model

## Backend Options

1. **GitHub Copilot** (Default)
   - Models: Claude 4.5, GPT-5 (if available), GPT-4 series, Claude 3.x (deprecated)
   - Auth: GitHub CLI
   - No API keys needed
   - **Latest**: Claude Sonnet 4.5 (Oct 2025)

2. **Grey-area Ollama** (Local/Private)
   - Models: llama3.2, codellama, mistral, qwen2.5
   - Host: grey-area:11434 (via Tailscale)
   - Privacy-focused

3. **Local Ollama** (Fallback)
   - Same models as grey-area
   - Host: localhost:11434
   - Requires local Ollama running

## Common Workflows

### Code Review
1. Select code region
2. `C-c g v` (review-region)
3. Review suggestions in GPTel buffer

### Debugging
1. Position cursor on error line
2. `C-c g x` (send-error-context)
3. Get debugging suggestions

### Commit Messages
1. Stage changes: `git add`
2. `C-c g m` (commit-message)
3. Message copied to kill ring
4. `C-x g` (magit) → `c c` → `C-y` (yank)

### Project Chat
1. `C-c g p` (project-context)
2. Opens `*GPTel-ProjectName*` buffer
3. Auto-includes README context

### Quick Questions
1. `C-c g t` (quick-chat)
2. Enter query
3. Response appears in `*GPTel Quick Response*`

## In GPTel Buffer

| Key | Action |
|-----|--------|
| `C-c RET` | Send message |
| `C-c C-k` | Abort current request |
| `C-c C-r` | Rewrite and replace |
| `C-c C-b` | Switch backend/model |
| `q` | Bury buffer (in dedicated window) |

## Setup & Authentication

### First Time Setup
```bash
# 1. Rebuild system (installs gh CLI)
sudo nixos-rebuild switch

# 2. Authenticate with GitHub
gh auth login

# 3. Verify Copilot access
gh copilot --help

# 4. Start Emacs
emacs
```

### Verify Configuration
Check `*Messages*` buffer for:
```
GitHub Copilot configured as default backend
GPTel ready! Backends: GitHub Copilot, Grey-area Ollama. Default: Copilot
```

## Switching Models

### Via Menu (Recommended)
1. `C-c g n` - Open GPTel menu
2. Navigate to model selection
3. Choose model

### Via Command
1. `C-c g b` - Switch backend
2. Select from available backends

### In Buffer
- `C-c C-b` while in GPTel buffer

## Tips & Best Practices

### Context Management
- Use `C-c g f` to add relevant files
- Keep context focused - don't add too many files
- Start new sessions for new topics

### Model Selection
- **GPT-4o**: General purpose, best overall
- **GPT-4o-mini**: Quick queries, faster responses
- **Claude 3.5 Sonnet**: Complex code, refactoring
- **o1-preview**: Math, complex reasoning
- **Local Ollama**: Private/sensitive code

### Efficient Prompting
- Be specific about what you want
- Include relevant context in selection
- Use project context for project-specific questions
- Explain/review work best with focused selections

### Privacy
- Use grey-area/local Ollama for sensitive code
- GitHub Copilot sends data to OpenAI/Anthropic
- Check your organization's AI usage policy

## Troubleshooting

### "GitHub Copilot setup failed"
```bash
gh auth login
# Restart Emacs
```

### "No backends available"
- Check: `which gh` (should return path)
- Check: `gh auth status` (should show logged in)
- Check: `gh copilot --help` (should work)

### Grey-area Ollama not reachable
```bash
# Check Tailscale
tailscale status

# Test connectivity
ping grey-area

# Verify Ollama
curl http://grey-area:11434/api/tags
```

### Slow responses
- Try GPT-4o-mini instead of GPT-4o
- Check internet connection
- Disable streaming: `(setq gptel-stream nil)`

### Streaming issues
```elisp
;; Add to init.el or evaluate
(setq gptel-stream nil)
```

## Advanced Usage

### Custom Directives
```elisp
(gptel-make-directive
  (system "You are an expert Nix developer.")
  (user "Help me write NixOS configuration."))
```

### Programmatic Requests
```elisp
(gptel-request "Your prompt here"
  :callback (lambda (response info)
              (message "Got: %s" response)))
```

### Different Backends per Project
Create `.dir-locals.el` in project:
```elisp
((nil . ((gptel-backend . "Local Ollama"))))
```

## Resources

- GPTel Docs: Built-in, `M-x describe-package RET gptel`
- GitHub Copilot: https://github.com/features/copilot
- Integration Guide: `gptel-integration-guide.md`
- This reference: `GPTEL-QUICK-REFERENCE.md`

---

**Last Updated**: October 2025 (Claude 4.5 series, GPT-5 support, GPTel config with Claude Sonnet 4.5 default)