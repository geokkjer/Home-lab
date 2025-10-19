# GPTel Integration Guide for NixOS and Emacs

## Overview

GPTel is a simple and powerful LLM client for Emacs that brings AI conversations directly into your Emacs buffers. This setup uses GitHub Copilot Chat (no API keys needed) as the primary backend and your grey-area Ollama instance for local/private AI interactions, providing seamless integration with your existing workflows.

## Current State Analysis

This GPTel setup offers these advantages:
- **Buffer-native interaction**: Chat directly in Emacs buffers, no separate terminal
- **No API key management**: GitHub Copilot handles authentication automatically
- **Local privacy options**: Use grey-area Ollama for sensitive/private queries
- **Context integration**: Automatically include files, code regions, org-mode content
- **Streaming responses**: Real-time response updates
- **Multiple model access**: Latest GitHub models + local Ollama models
- **Better Org-mode integration**: Native support for images, files, and structured content

## Integration Strategy

GPTel will replace any existing terminal-based AI workflows with native Emacs buffer integration, providing a more seamless development experience.

## NixOS Integration

### 1. Update `emacs.nix`

GPTel is already added to your development package set in `emacs.nix`:

```nix
# In packageSets.development
development = epkgs:
  with epkgs; [
    # ... existing packages ...
    
    # AI Integration
    gptel
    
    # ... rest of packages ...
  ];
```

### 2. GitHub CLI Setup

GitHub CLI is already available in your global NixOS configuration, so no additional installation is needed.

### 3. Grey-area Ollama Access

Your grey-area machine already runs Ollama on port 11434 with network access enabled. The configuration automatically detects and connects via Tailscale to `grey-area:11434` or `grey-area.tail807ea.ts.net:11434`.

## Emacs Configuration

### 1. AI Integration Module

The `modules/ai-integration.el` module is already created and configured for GitHub Copilot + Ollama:

```elisp
;; Backend configuration function
(defun gptel-ai-integration-setup-backends ()
  "Configure GPTel backends: GitHub Copilot and grey-area Ollama."
  (let ((configured-backends '()))

    ;; GitHub Copilot Chat (primary - no API key needed)
    (condition-case nil
        (progn
          (gptel-make-gh-copilot "Copilot")
          (setq gptel-backend (gptel-get-backend "Copilot"))
          (setq gptel-model 'claude-3.5-sonnet) ; Default Copilot model
          (push "GitHub Copilot" configured-backends))
      (error (message "GitHub Copilot setup failed - you may need to authenticate with 'gh auth login'")))

    ;; Grey-area Ollama instance (local privacy-focused models)
    (condition-case nil
        (let* ((grey-area-hosts '("grey-area" "grey-area.tail807ea.ts.net"))
               (reachable-host (cl-find-if
                               (lambda (host)
                                 (zerop (call-process "ping" nil nil nil "-c" "1" "-W" "2" host)))
                               grey-area-hosts)))
          (when reachable-host
            (let ((grey-area-host (format "%s:11434" reachable-host)))
              (gptel-make-ollama "Grey-area Ollama"
                :host grey-area-host
                :stream t
                :models '(llama3.2:latest codellama:latest mistral:latest qwen2.5:latest))
              (push "Grey-area Ollama" configured-backends))))
      (error (message "Grey-area Ollama not reachable - check Tailscale connection")))

    ;; Display configured backends
    (if configured-backends
        (message "GPTel configured with backends: %s" (string-join configured-backends ", "))
      (message "GPTel: No backends available. Check GitHub authentication and Ollama connectivity"))))

;; Org mode integration
(use-package gptel
  :hook (org-mode . gptel-org-mode-setup))

(defun gptel-org-mode-setup ()
  "Setup GPTel specific bindings for Org mode."
  (local-set-key (kbd "C-c g f") #'gptel-org-set-topic))

;; Integration with development workflow
(defun gptel-commit-message ()
  "Generate commit message based on git diff."
  (interactive)
  (if (magit-anything-staged-p)
      (let ((diff (shell-command-to-string "git diff --staged")))
        (gptel-request
         (format "Based on this git diff, write a clear, concise commit message following conventional commits format:\n\n```diff\n%s\n```\n\nJust return the commit message, no explanation."
                 diff)
         :callback (lambda (response info)
                     (when (and response (string-match-p "^[a-zA-Z]" response))
                       (kill-new (string-trim response))
                       (message "Commit message copied to kill ring: %s" (string-trim response))))))
    (message "No staged changes found")))

;; Key bindings for common AI tasks
(global-set-key (kbd "C-c g e") #'gptel-explain-region)
(global-set-key (kbd "C-c g r") #'gptel-review-region)
(global-set-key (kbd "C-c g x") #'gptel-send-error-context)
(global-set-key (kbd "C-c g p") #'gptel-project-context)
(global-set-key (kbd "C-c g m") #'gptel-commit-message)

;; Optional: Auto-start GPTel in certain modes
(defun maybe-suggest-gptel ()
  "Suggest GPTel usage in programming modes."
  (when (and (derived-mode-p 'prog-mode)
             (> (buffer-size) 100)
             (not (gptel--in-response-p)))
    (run-with-idle-timer 30 nil
                         (lambda ()
                           (message "Tip: Use C-c g to interact with AI about this code")))))

;; Uncomment to enable suggestions
;; (add-hook 'prog-mode-hook #'maybe-suggest-gptel)

(provide 'ai-integration)
;;; ai-integration.el ends here
```

### 2. Update Module Loading

The `init-nix.el` has been updated to load the AI integration module:

```elisp
;; AI integration is loaded in GUI and default profiles
(pcase profile
  ("nox"
   ;; Minimal modules for terminal use - no AI to reduce dependencies
   (load-module "completion")
   (load-module "navigation")
   (load-module "development")
   (load-module "elisp-development"))
  
  ("gui"
   ;; Full module set for GUI development
   (load-module "ui")
   (load-module "completion")
   (load-module "navigation")
   (load-module "development")
   (load-module "elisp-development")
   (load-module "ai-integration"))
  
  (_ 
   ;; Default module loading
   (load-module "ui")
   (load-module "completion")
   (load-module "navigation")
   (load-module "ai-integration")))
```

### 3. Update NixOS Module Configuration

Update the `environment.etc` section in your `emacs.nix`:

```nix
environment.etc = {
  # ... existing module files ...
  
  "emacs/modules/ai-integration.el" = mkIf (cfg.profile == "gui") {
    source = ../../dotfiles/geir/emacs-config/modules/ai-integration.el;
    mode = "0644";
  };
};
```

## Authentication Setup

### GitHub Copilot Authentication

GitHub Copilot handles authentication automatically through GitHub CLI:

```bash
# Authenticate with GitHub (one-time setup)
gh auth login

# Verify Copilot access
gh copilot --help
```

### Grey-area Ollama

No authentication required - Ollama runs on your grey-area machine and is accessible over Tailscale. The configuration automatically detects connectivity to `grey-area:11434` or `grey-area.tail807ea.ts.net:11434`.

## Key Features and Workflows

### Primary Keybindings

| Keybinding | Function |
|-----------|----------|
| `C-c g c` | Start GPTel chat buffer |
| `C-c g s` | Send current region/prompt |
| `C-c g e` | Explain code region |
| `C-c g v` | Review code region |
| `C-c g x` | Send error context |
| `C-c g p` | Project-specific chat |
| `C-c g m` | Generate commit message |
| `C-c g t` | Quick chat query |
| `C-c g n` | Open GPTel menu |
| `C-c g b` | Switch backend |

### Key Advantages

1. **No API key management**: GitHub Copilot handles authentication automatically
2. **Latest models**: Access to GitHub's latest AI models through Copilot
3. **Local privacy**: Use grey-area Ollama for sensitive queries
4. **Streaming responses**: See responses as they're generated
5. **Region operations**: Easy to send/modify selected text
6. **Project awareness**: Automatic file and project context
7. **Backend switching**: Easy switching between cloud and local models

## Advanced Configuration

### Custom Directives

Create custom directives for common tasks:

```elisp
(gptel-make-directive
  (system "You are an expert Emacs Lisp developer. Provide concise, well-documented code.")
  (user "Help me write an Emacs Lisp function."))
```

### Tool Integration

If using tool-capable models:

```elisp
(setq gptel-tools
      '(("search_web" . my-web-search-function)
        ("read_file" . my-file-reader-function)))
```

### Model-Specific Settings

```elisp
(setq gptel-model-alist
      '((gpt-4o :max-tokens 4096 :temperature 0.1)
        (claude-3-sonnet :max-tokens 8192 :temperature 0.2)))
```

## Testing Your Setup

1. **GitHub Authentication**: Run `gh auth login` if not already authenticated
2. **Basic Test**: `M-x gptel` should open a GPTel buffer with Copilot
3. **Send Test**: Type a query and press `C-c RET`
4. **Backend Switch**: Press `C-c g b` to switch to Grey-area Ollama
5. **Region Test**: Select code and run `C-c g e` (explain region)
6. **Project Test**: `C-c g p` should create project-specific session

## Troubleshooting

### Common Issues

1. **GitHub Auth**: Run `gh auth login` and ensure Copilot access
2. **Grey-area Connection**: Check network connectivity to `grey-area.lan`
3. **Backend Not Found**: Use `C-c g b` to switch between available backends
4. **Streaming Issues**: Try disabling streaming: `(setq gptel-stream nil)`

### Debug Commands

```elisp
;; Check configured backends
(gptel-backend-name gptel-backend)

;; List all backends
gptel--known-backends

;; Test grey-area connectivity via Tailscale
(call-process "ping" nil nil nil "-c" "1" "grey-area")
```

## Recommended Usage Patterns

1. **Code Review**: Select code region, `C-c g r` for review
2. **Error Debugging**: Place cursor on error line, `C-c g x` for context
3. **Documentation**: Use in Org-mode for writing and documentation assistance
4. **Learning**: Ask questions about unfamiliar code patterns
5. **Commit Messages**: `C-c g m` for AI-generated commit messages

## Conclusion

This GPTel setup provides seamless AI integration with no API key management required. GitHub Copilot gives you access to the latest models, while grey-area Ollama provides local/private AI capabilities. The configuration automatically detects available backends and provides easy switching between them.

Start with GitHub Copilot for general queries and switch to grey-area Ollama for sensitive/private work. The buffer-native approach provides a natural development workflow within Emacs.

## Quick Start

1. Ensure Tailscale is connected: `tailscale status`
2. Ensure GitHub CLI is authenticated: `gh auth login`
3. Start Emacs and press `C-c g c`
4. Choose your backend with `C-c g b` if needed
5. Start chatting with AI directly in Emacs!