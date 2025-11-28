# GPTel Research for Literate Emacs Config

## Overview

GPTel is a modern LLM client for Emacs that provides buffer-native AI interaction with streaming responses. It supports multiple backends including GitHub Copilot, OpenAI, Anthropic, and Ollama.

## Why GPTel?

1. **Buffer-native**: Chat directly in Emacs buffers, no separate terminal
2. **No API key management**: GitHub Copilot handles authentication automatically
3. **Local privacy options**: Use Ollama for sensitive/private queries
4. **Context integration**: Automatically include files, code regions, org-mode content
5. **Streaming responses**: Real-time response updates
6. **Multiple model access**: Latest models through GitHub Copilot

## Backend Options

### 1. GitHub Copilot (Recommended Default)

- **Pros**: No API keys, authentication via `gh` CLI, access to latest models
- **Cons**: Requires GitHub Copilot subscription
- **Models**: Claude 4.5 series, GPT-4o, o1-preview/mini, Claude 3.x
- **Setup**: `gh auth login`

### 2. Ollama (Local/Private)

- **Pros**: Privacy-focused, runs locally, no data sent externally
- **Cons**: Requires local compute resources
- **Models**: llama3.2, codellama, mistral, qwen2.5
- **Setup**: Install and run Ollama service

### 3. Direct API (OpenAI/Anthropic)

- **Pros**: Direct access, full control
- **Cons**: Requires API keys, costs per token
- **Setup**: Set environment variables with API keys

## Minimal Configuration for Literate Config

For a simple, tangleable config, we want:

1. Install `gptel` via use-package
2. Configure a default backend (Copilot or Ollama)
3. Set up essential keybindings
4. Add basic workflow helpers

### Essential Keybindings

| Key | Function | Description |
|-----|----------|-------------|
| `C-c g c` | `gptel` | Start new GPTel chat buffer |
| `C-c g s` | `gptel-send` | Send current region/prompt |
| `C-c g n` | `gptel-menu` | Open GPTel transient menu |
| `C-c RET` | Send in buffer | Send message in GPTel buffer |

### Basic Configuration

```elisp
(use-package gptel
  :bind (("C-c g c" . gptel)
         ("C-c g s" . gptel-send)
         ("C-c g n" . gptel-menu))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-stream t)
  :config
  ;; Choose ONE of the following backends:
  
  ;; Option 1: GitHub Copilot (requires gh CLI)
  ;; (gptel-make-gh-copilot "Copilot"
  ;;   :stream t
  ;;   :models '(claude-sonnet-4 gpt-4o gpt-4o-mini))
  ;; (setq gptel-backend (gptel-get-backend "Copilot"))
  ;; (setq gptel-model 'gpt-4o)
  
  ;; Option 2: Local Ollama
  ;; (gptel-make-ollama "Ollama"
  ;;   :host "localhost:11434"
  ;;   :stream t
  ;;   :models '(llama3.2:latest codellama:latest))
  ;; (setq gptel-backend (gptel-get-backend "Ollama"))
  ;; (setq gptel-model 'llama3.2:latest)
  )
```

## Advanced Features (Optional)

### Code Explanation

```elisp
(defun my/gptel-explain-region ()
  "Explain the selected code."
  (interactive)
  (when (use-region-p)
    (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
      (gptel-request
       (format "Explain this code:\n\n```\n%s\n```" code)))))
```

### Commit Message Generation

```elisp
(defun my/gptel-commit-message ()
  "Generate commit message from staged changes."
  (interactive)
  (let ((diff (shell-command-to-string "git diff --cached")))
    (when (not (string-empty-p diff))
      (gptel-request
       (format "Write a commit message for:\n\n```diff\n%s\n```" diff)
       :callback (lambda (response info)
                   (kill-new (string-trim response))
                   (message "Commit message copied: %s" (string-trim response)))))))
```

## Prerequisites

### For GitHub Copilot

1. Install GitHub CLI: `gh` (available via package manager)
2. Authenticate: `gh auth login`
3. Verify: `gh copilot --help`

### For Ollama

1. Install Ollama: <https://ollama.ai>
2. Start service: `ollama serve`
3. Pull a model: `ollama pull llama3.2`

## Arch Linux Setup

```bash
# For GitHub CLI
sudo pacman -S github-cli
gh auth login

# For Ollama
yay -S ollama
systemctl --user start ollama
ollama pull llama3.2
```

## Recommendations for Literate Config

### Minimal (Start Here)

- Just add GPTel with Ollama backend (works offline, no API keys)
- Basic `C-c g` keybindings
- ~20 lines of config

### Standard

- GPTel with Copilot backend (best model access)
- Code explanation helper
- Project context integration
- ~40 lines of config

### Full (from existing NixOS config)

- Multiple backends with auto-detection
- All helper functions (explain, review, commit message)
- Magit integration
- ~150 lines of config

## Suggested Implementation

For the literate config, I recommend starting with the **Standard** level:

1. Use-package declaration with keybindings
2. Configure GitHub Copilot as primary (with Ollama fallback comment)
3. Add `gptel-explain-region` helper
4. Keep it simple and expandable

This keeps the config simple while providing real value. The full ai-integration.el from the NixOS config can be referenced for advanced features later.

## Files Reference

- Full integration: `/home/geir/Projects/home-lab/dotfiles/geir/emacs-config/modules/ai-integration.el`
- Quick reference: `/home/geir/Projects/home-lab/dotfiles/geir/emacs-config/GPTEL-QUICK-REFERENCE.md`
- Integration guide: `/home/geir/Projects/home-lab/dotfiles/geir/emacs-config/gptel-integration-guide.md`
