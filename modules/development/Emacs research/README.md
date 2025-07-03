# Modular Emacs Configuration with Machine Profiles

**Author:** Geir Okkenhaug Jerstad  
**Email:** geir@geokkjer.eu

A **modular Emacs configuration system** using Emacs Overlay + Flakes that provides different configurations for different machine types in a home-lab environment.

## Table of Contents

- [About](#about)
- [Machine Profiles](#machine-profiles)
- [Quick Start](#quick-start)
- [Nix Development Environment](#nix-development-environment)
- [Core Configuration (init.el)](#core-configuration-initel)
- [Available Modules](#available-modules)
- [How-To Guides](#how-to-guides)
- [Advanced Usage](#advanced-usage)
- [Conclusion](#conclusion)

## About

This is a **machine profile-aware Emacs configuration system** designed for consistent deployment across different types of machines in a home-lab environment. Each profile provides an appropriate level of functionality for its intended use case.

### Philosophy

- **Profile-Based**: Different machine types get appropriate Emacs configurations
- **Modular Architecture**: Features organized into logical modules
- **Reproducible**: Exact same environment using Nix flakes
- **Single-User Focused**: Optimized for home-lab admin workflow

### Machine Profiles

#### 🔧 Server Profile (Minimal)
- **Use Case**: Headless servers, emergency editing, basic configuration tasks
- **Features**: Essential editing, basic navigation, config file support
- **Packages**: ~15 minimal packages (vertico, consult, smartparens, nix-mode)
- **Startup**: < 1 second
- **Command**: `nix develop .#server`

#### 💻 Laptop Profile (Development + Portable)
- **Use Case**: Development on the go, balanced features vs battery life
- **Features**: Full LSP, Elisp development, Git integration, modern UI
- **Packages**: ~40 curated development packages
- **Startup**: < 3 seconds
- **Command**: `nix develop .#laptop`

#### 🖥️ Workstation Profile (Maximum Features)
- **Use Case**: Primary development machine, maximum productivity
- **Features**: All development tools, advanced features, performance optimized
- **Packages**: 50+ comprehensive package set
- **Startup**: < 3 seconds
- **Command**: `nix develop .#workstation`

## Quick Start

### 1. Try a Profile

```bash
# Minimal server profile
nix develop github:geokkjer/emacs-config#server

# Development laptop profile  
nix develop github:geokkjer/emacs-config#laptop

# Full workstation profile
nix develop github:geokkjer/emacs-config#workstation
```

### 2. Integrate with NixOS

```nix
# Add to your configuration.nix or home.nix
{
  inputs.emacs-config.url = "github:geokkjer/emacs-config";
  
  # System-wide installation
  environment.systemPackages = [ 
    inputs.emacs-config.packages.${system}.emacs-laptop
  ];
  
  # Or user-specific
  users.users.yourusername.packages = [
    inputs.emacs-config.packages.${system}.emacs-workstation
  ];
}
```

### 3. Check Available Options

```bash
# List all available outputs
nix flake show github:geokkjer/emacs-config

# Run specific profile
nix run github:geokkjer/emacs-config#server
nix run github:geokkjer/emacs-config#laptop  
nix run github:geokkjer/emacs-config#workstation
```

## Core Configuration (init.el)

The core configuration provides the minimal foundation needed for a productive Emacs experience.

### Performance Optimizations

We start with performance tweaks that make Emacs load faster and run smoother.

```elisp
;; Performance optimizations
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
```

**Why this approach?**
- Temporarily increase garbage collection threshold during startup
- Reset to reasonable value after initialization
- Provides startup time feedback for optimization

### Minimal UI Setup

Clean, distraction-free interface that's pleasant to look at.

```elisp
;; Basic UI setup - minimal but pleasant
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-face-attribute 'default nil :height 140)
(setq-default cursor-type 'bar)
```

**Design choices:**
- Remove visual clutter (toolbars, scrollbars)
- Readable font size (14pt)
- Modern bar cursor instead of block
- Conditional disabling (handles terminal Emacs gracefully)

### Package Management

Modern package management with MELPA, use-package, and quelpa for Git packages.

```elisp
;; Package management setup
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Install quelpa for packages not in ELPA/MELPA
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Install quelpa-use-package for integration
(unless (package-installed-p 'quelpa-use-package)
  (quelpa 'quelpa-use-package))
(require 'quelpa-use-package)

;; Install eat terminal (not available in ELPA/MELPA)
(unless (package-installed-p 'eat)
  (quelpa '(eat :fetcher git
                :url "https://codeberg.org/akib/emacs-eat.git"
                :files ("*.el" "dir"
                        "*.info" "*.texi"
                        "*.ti" ("e" "e/*")))))
```

**Why this package setup?**
- **package.el + MELPA**: Standard Emacs package management for most packages
- **use-package**: Declarative package configuration with lazy loading
- **quelpa**: Install packages directly from Git repositories (eat, claude-code)
- **Consistent approach**: All packages use the same installation system
- Automatic installation with fallback mechanisms

### Essential Org-mode

Immediate productivity with org-mode for documentation and literate programming.

```elisp
;; Essential org-mode setup for immediate productivity
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  ;; Better org-mode experience
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t)
  
  ;; Enable babel languages for literate config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))
```

**Org-mode configuration rationale:**
- Syntax highlighting in code blocks for better readability
- Preserve indentation for clean code extraction
- No confirmation for code evaluation (literate programming workflow)
- Essential keybindings for link management and agenda

### Module Loading System

The heart of our modular architecture.

```elisp
;; Module loading system
(defvar config-modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory containing configuration modules.")

(defun load-config-module (module)
  "Load a configuration MODULE from the modules directory."
  (let ((module-file (expand-file-name (format "%s.el" module) config-modules-dir)))
    (when (file-exists-p module-file)
      (load module-file)
      (message "Loaded module: %s" module))))

(defun load-config-modules (&rest modules)
  "Load multiple configuration MODULES."
  (dolist (module modules)
    (load-config-module module)))

;; Create modules directory if it doesn't exist
(unless (file-exists-p config-modules-dir)
  (make-directory config-modules-dir t))
```

**Module system benefits:**
- Separation of concerns
- Easy to enable/disable features
- Modular testing and debugging
- Clean organization

## Available Modules

Each module focuses on a specific aspect of the Emacs experience.

### UI Module (ui.el)

Modern, beautiful interface with professional themes and icons.

```elisp
;; Doom themes - Modern, well-designed color schemes
(use-package doom-themes
  :config
  (load-theme 'doom-monokai-pro t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Doom modeline - Attractive, informative status line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project))

;; All the icons - Beautiful file and mode icons
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))
```

**Package choices explained:**

- **doom-themes**: Professionally designed, consistent color schemes. doom-monokai-pro provides excellent contrast and readability
- **doom-modeline**: More informative than default modeline, shows git status, project info, and LSP status
- **all-the-icons**: Visual file type identification, integrates with many packages for enhanced UI

### Completion Module (completion.el)

Modern completion system that makes Emacs feel like a contemporary editor.

```elisp
;; Vertico - Vertical completion UI (replaces Ivy/Helm)
(use-package vertico
  :init (vertico-mode)
  :custom (vertico-cycle t))

;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :init (marginalia-mode))

;; Consult - Enhanced search and navigation commands
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)))

;; Orderless - Flexible completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;; Corfu - In-buffer completion popup
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  :init (global-corfu-mode))
```

**Why this completion stack?**

- **Vertico**: Simpler than Ivy/Helm, follows Emacs conventions, fast and reliable
- **Marginalia**: Adds helpful context (file sizes, dates, docstrings) to completion candidates
- **Consult**: Modern replacements for built-in commands with preview and enhanced functionality
- **Orderless**: Flexible matching - type words in any order, use space as AND operator
- **Corfu**: Lightweight in-buffer completion, works well with LSP

### Development Module (development.el)

Professional development environment with LSP, version control, and AI assistance.

```elisp
;; LSP Mode - Language Server Protocol support
(use-package lsp-mode
  :hook ((prog-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.5)
  (lsp-completion-provider :none))

;; LSP UI - Enhanced LSP interface
(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t))

;; GitHub Copilot - AI pair programming
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)))

;; Magit - Git interface
(use-package magit
  :bind ("C-x g" . magit-status))

;; Which Key - Discover keybindings
(use-package which-key
  :config (which-key-mode 1))
```

**Development tool rationale:**

- **LSP Mode**: Industry-standard language server protocol, provides IDE features across many languages
- **Copilot**: AI assistance for faster coding, especially useful for boilerplate and suggestions
- **Magit**: Universally acclaimed Git interface, makes version control intuitive and powerful
- **Which-key**: Invaluable for learning Emacs keybindings and discovering functionality

### Navigation Module (navigation.el)

Efficient file and project navigation for large codebases.

```elisp
;; Projectile - Project management
(use-package projectile
  :config (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Treemacs - File tree sidebar
(use-package treemacs
  :bind (("M-0" . treemacs-select-window)
         ("C-x t t" . treemacs))
  :custom (treemacs-width 30))

;; Ace Window - Quick window switching
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Winner mode - Window configuration undo/redo
(use-package winner
  :ensure nil
  :config (winner-mode 1))
```

**Navigation choices:**

- **Projectile**: Essential for working with projects, provides fuzzy finding, project-aware commands
- **Treemacs**: Visual file browser, especially useful when exploring unfamiliar codebases
- **Ace-window**: Quick window jumping without complex key combinations
- **Winner-mode**: Undo/redo for window configurations, saves time when layouts get messed up

### Claude Code Module (claude-code.el)

AI assistant integration bringing Claude directly into Emacs.

```elisp
;; Install claude-code via quelpa if not already installed
(unless (package-installed-p 'claude-code)
  (quelpa '(claude-code :fetcher github :repo "stevemolitor/claude-code.el")))

;; Claude Code - AI assistant integration
(use-package claude-code
  :ensure nil  ; Already installed via quelpa
  :bind-keymap ("C-c c" . claude-code-command-map)
  :custom
  (claude-code-terminal-type 'eat)  ; eat installed via quelpa in init.el
  (claude-code-notifications t))

;; Enhanced error handling for Claude Code
(defun claude-code-send-error-context ()
  "Send error at point with surrounding context to Claude."
  (interactive)
  (let* ((error-line (line-number-at-pos))
         (start (max 1 (- error-line 5)))
         (end (min (line-number-at-pos (point-max)) (+ error-line 5)))
         (context (buffer-substring-no-properties
                  (line-beginning-position (- start error-line))
                  (line-end-position (- end error-line)))))
    (claude-code-send-command 
     (format "I'm getting an error around line %d. Here's the context:\n\n```%s\n%s\n```\n\nCan you help me fix this?"
             error-line
             (or (file-name-extension (buffer-file-name)) "")
             context))))
```

**Claude Code integration benefits:**

- Direct AI assistance without leaving Emacs
- Uses eat terminal (installed via quelpa) for reliable performance
- Context-aware help with error debugging
- Project-specific Claude instances
- Enhanced error reporting with surrounding code context
- **Note**: Uses `C-c C-c` prefix to avoid conflict with org-capture (`C-c c`)

#### Key Claude Code Commands

| Command | Keybinding | Description |
|---------|------------|-------------|
| `claude-code` | `C-c C-c c` | Start Claude in current directory |
| `claude-code-send-region` | `C-c C-c r` | Send selected text to Claude |
| `claude-code-send-command` | `C-c C-c s` | Send a command to Claude |
| `claude-code-fix-error-at-point` | `C-c C-c e` | Fix error at current point |
| `claude-code-project-instance` | `C-c C-c p` | Start project-specific Claude instance |
| `claude-code-send-error-context` | `C-c C-c x` | Send error with surrounding context |

## Nix Development Environment

This repository includes a comprehensive Nix flake that provides a complete Emacs Lisp development environment with LSP support, advanced editing tools, and all necessary dependencies.

### 🚀 Quick Start with Nix

```bash
# Enter the development environment
nix develop

# Or run Emacs directly
nix run                    # GUI mode
nix run .#emacs-nw        # Terminal mode
```

### 🔧 What's Included

The Nix flake provides:

**Enhanced Emacs with curated packages:**
- LSP Mode with intelligent completion and navigation
- Specialized Emacs Lisp development tools (elisp-slime-nav, package-lint, helpful)
- Advanced editing features (smartparens, aggressive-indent, rainbow-delimiters)
- Modern completion framework (vertico, consult, marginalia)
- Git integration (magit, git-gutter, git-timemachine)
- Beautiful UI (doom-themes, doom-modeline, all-the-icons)

**Development tools:**
- Cask for package development
- File watchers (fswatch, entr)
- Text processing utilities (ripgrep, fd, fzf)
- Documentation tools (pandoc, texinfo)
- Spell checking and language support

**Specialized Elisp Module:**
The flake includes a new `elisp-development.el` module with enhanced features:
- Advanced navigation and symbol lookup
- Package linting and validation
- Enhanced debugging with edebug integration
- Live documentation with examples
- Automatic byte-compilation
- Project-specific development setup

### 🎯 Key Features for Elisp Development

| Feature | Tool | Description |
|---------|------|-------------|
| **Smart Navigation** | elisp-slime-nav | Jump to definitions with M-. |
| **Enhanced Help** | helpful + elisp-demos | Documentation with live examples |
| **Package Validation** | package-lint | Lint elisp packages for MELPA standards |
| **Auto Compilation** | auto-compile | Automatic byte-compilation on save |
| **Advanced Editing** | smartparens + aggressive-indent | Intelligent parentheses and formatting |
| **Syntax Checking** | flycheck | Real-time error detection |
| **Project Management** | projectile + treemacs | File navigation and project structure |

### 📁 Loading the Elisp Development Module

To enable all the specialized Emacs Lisp features, add this to your `init.el`:

```elisp
;; Load the specialized elisp development module
(load-config-module 'elisp-development)

;; Or load multiple development modules
(load-config-modules 'development 'elisp-development 'completion)
```

### ⌨️ Essential Key Bindings

| Key | Function | Description |
|-----|----------|-------------|
| `C-c C-e` | eval-last-sexp | Evaluate expression before cursor |
| `C-c C-b` | eval-buffer | Evaluate entire buffer |
| `C-c C-d` | describe-function-at-point | Enhanced documentation |
| `M-.` | jump to definition | Navigate to symbol definition |
| `C-h f` | helpful-callable | Enhanced function help with examples |
| `C-c l` | lsp-command-map | LSP commands and features |

### 🔄 Development Workflow

1. **Enter the environment**: `nix develop`
2. **Start Emacs**: `emacs` or `emacs -nw`
3. **Load elisp module**: Add `(load-config-module 'elisp-development)` to init.el
4. **Develop**: Edit .el files with full LSP support, syntax checking, and enhanced navigation
5. **Test**: Use `C-c C-e` for quick evaluation, `C-c C-z` for IELM REPL
6. **Package**: Use package-lint for validation, auto-compile handles byte-compilation

### 🛠️ Customization

The flake is designed to work with the existing modular configuration. You can:
- Add more Emacs packages by editing the `myEmacs` section in `flake.nix`
- Include additional system tools in the `developmentTools` section
- Customize the elisp-development module for your specific workflow

## How-To Guides

### Getting Started

1. **Deploy Configuration:**
   ```bash
   ./deploy-config.sh
   ```

2. **Start with Basic Setup:**
   Uncomment these lines in `~/.emacs.d/init.el`:
   ```elisp
   (load-config-modules 'ui 'completion)
   ```

3. **Add Development Tools:**
   ```elisp
   (load-config-module 'development)
   ```

### Quick Start Examples

**Minimal Setup (Just UI improvements):**
```elisp
(load-config-module 'ui)
```

**Writer's Setup (UI + Org-mode enhancements):**
```elisp
(load-config-modules 'ui 'completion)
```

**Developer Setup (Full stack):**
```elisp
(load-config-modules 'ui 'completion 'development 'navigation)
```

**AI-Enhanced Development:**
```elisp
(load-config-modules 'ui 'completion 'development 'navigation 'claude-code)
```

### Adding New Modules

Create a new module file in `modules/` directory:

```elisp
;;; my-module.el --- Custom module -*- lexical-binding: t; -*-

;; Your configuration here
(use-package some-package
  :ensure t
  :config
  ;; configuration)

(provide 'my-module)
;;; my-module.el ends here
```

Load it in `init.el`:
```elisp
(load-config-module 'my-module)
```

### Customizing Modules

Edit module files directly in `~/.emacs.d/modules/` to customize behavior:

- Modify keybindings
- Adjust package configurations
- Add new packages to existing modules
- Change themes or UI elements

### Package Manager Consistency

This configuration uses a **unified approach** to package management:

- **MELPA packages**: Installed via `package.el` and managed with `use-package`
- **Git-only packages**: Installed via `quelpa` (eat, claude-code)
- **No straight.el**: Avoids conflicts between package managers
- **Consistent syntax**: All packages use `use-package` for configuration

**Benefits:**
- No package manager conflicts
- Predictable installation behavior  
- Easy to debug package issues
- Works reliably across different systems

### Troubleshooting

| Problem | Solution |
|---------|----------|
| Package installation fails | Run `M-x package-refresh-contents` |
| Module not loading | Check file exists in `~/.emacs.d/modules/` |
| Performance issues | Disable modules one by one to identify culprit |
| Keybinding conflicts | Use `C-h k` to check what key does |
| Claude Code not working | Install Claude Code CLI: `pip install claude-code` |
| vterm compilation fails | Install cmake and build tools: `nix-shell -p cmake gcc` |
| Quelpa installation issues | Check internet connection, restart Emacs |

## Advanced Usage

### Performance Monitoring

Check startup time with:
```elisp
(message "Emacs loaded in %s" 
         (format "%.2f seconds" 
                 (float-time (time-subtract after-init-time before-init-time))))
```

### Module Development Best Practices

- Use `lexical-binding: t`
- Include proper commentary
- Use `use-package` consistently
- Provide the module at the end
- Keep focused on single concern

### Integration with Other Tools

The modular system works well with:
- Language-specific configurations
- Company-specific modules  
- Personal workflow modules
- Experimental feature modules

### File Structure

```
Emacs/
├── init.el                 # Core configuration
├── modules/
│   ├── ui.el              # UI enhancements
│   ├── completion.el      # Modern completion
│   ├── development.el     # Development tools
│   ├── navigation.el      # File/project navigation
│   └── claude-code.el     # AI assistant integration
├── deploy-config.sh       # Deployment script
├── README.md             # This file
├── CLAUDE.md             # Claude Code assistant guidance
├── TODO.md               # Project status
└── emacs.org             # Literate documentation
```

## Conclusion

This modular approach provides:

- **Flexibility**: Enable only what you need
- **Maintainability**: Easy to debug and modify
- **Performance**: Faster startup with selective loading
- **Organization**: Clear separation of concerns
- **Extensibility**: Simple to add new functionality

The configuration grows with your needs while maintaining simplicity and performance. Whether you're writing documentation, developing software, or doing research, you can customize your Emacs environment to match your workflow perfectly.

### Why This Approach?

Traditional Emacs configurations often become unwieldy monoliths that are hard to maintain and debug. This modular system solves that by:

1. **Separation of Concerns**: Each module handles one aspect of the experience
2. **Incremental Adoption**: Start minimal and add features as needed
3. **Easy Debugging**: Issues are isolated to specific modules
4. **Performance**: Only load what you use
5. **Documentation**: Every choice is explained and justified

Start with the core configuration and gradually enable modules as you discover what you need. Your Emacs will evolve with your workflow while staying fast and maintainable.