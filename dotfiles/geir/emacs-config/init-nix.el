;;; init.el --- Nix-integrated modular Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A Nix-integrated, modular Emacs configuration that leverages Nix-provided tools
;; and packages where possible, falling back to Emacs package manager only when needed.
;; Core setup: UI, Nix integration, modular loading

;;; Code:

;; Performance optimizations
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))
            (let ((profile (getenv "EMACS_PROFILE")))
              (message "Emacs loaded in %s with %d garbage collections (Profile: %s)."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done
                       (or profile "unknown")))))

;; Basic UI setup - minimal but pleasant
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-face-attribute 'default nil :height 110)
;; Set text width to 140 characters
(setq-default fill-column 140)
(setq-default cursor-type 'bar)

;; Nix Integration Setup
;; Configure Emacs to use Nix-provided tools when available
(defun nix-tool-path (tool-name)
  "Get the path to TOOL-NAME from Nix environment variables."
  (let ((env-var (concat (upcase tool-name) "_PATH")))
    (getenv env-var)))

;; Configure external tools to use Nix-provided binaries
(when-let ((rg-path (nix-tool-path "rg")))
  (setq consult-ripgrep-command rg-path))

(when-let ((ag-path (nix-tool-path "ag")))
  (setq ag-executable ag-path))

(when-let ((fd-path (nix-tool-path "fd")))
  (setq find-program fd-path))

(when-let ((sqlite-path (nix-tool-path "sqlite")))
  (setq org-roam-database-connector 'sqlite3)
  (setq org-roam-db-executable sqlite-path))

;; Language Server Configuration (for Nix-provided LSP servers)
(defun configure-nix-lsp-servers ()
  "Configure LSP to use Nix-provided language servers."
  (when (featurep 'lsp-mode)
    ;; Nix LSP server
    (when-let ((nil-path (nix-tool-path "nil_lsp")))
      (setq lsp-nix-nil-server-path nil-path))
    
    ;; Bash LSP server
    (when-let ((bash-lsp-path (nix-tool-path "bash_lsp")))
      (setq lsp-bash-language-server-path bash-lsp-path))
    
    ;; YAML LSP server
    (when-let ((yaml-lsp-path (nix-tool-path "yaml_lsp")))
      (setq lsp-yaml-language-server-path yaml-lsp-path))))

;; Configure format-all to use Nix-provided formatters
(defun configure-nix-formatters ()
  "Configure format-all to use Nix-provided formatters."
  (when (featurep 'format-all)
    ;; Shellcheck for shell scripts
    (when-let ((shellcheck-path (nix-tool-path "shellcheck")))
      (setq format-all-formatters
            (cons `(sh (shellcheck ,shellcheck-path))
                  format-all-formatters)))))

;; Package management setup
;; Note: With Nix integration, we rely less on package.el
;; Most packages come pre-installed via the flake
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

;; Only initialize package.el if we're not in a Nix environment
;; In Nix environments, packages are pre-installed
(unless (getenv "EMACS_PROFILE")
  (package-initialize)
  
  ;; Install use-package for non-Nix environments
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Quelpa setup for packages not available in standard repos
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Install quelpa-use-package for integration
(unless (package-installed-p 'quelpa-use-package)
  (quelpa 'quelpa-use-package))
(require 'quelpa-use-package)

;; Install eat terminal emulator (not available in standard repos)
(unless (package-installed-p 'eat)
  (quelpa '(eat :fetcher git
                :url "https://codeberg.org/akib/emacs-eat.git"
                :files ("*.el" "dir"
                        "*.info" "*.texi"
                        "*.ti" ("e" "e/*")))))

;; Configure use-package for Nix integration
(require 'use-package)
;; Don't auto-install packages in Nix environment - they're pre-provided
(setq use-package-always-ensure (not (getenv "EMACS_PROFILE")))

;; Essential packages that should be available in all profiles
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  ;; Ensure Nix environment is properly inherited
  (exec-path-from-shell-copy-envs '("NIX_PATH" "NIX_EMACS_PROFILE")))

(use-package diminish)
(use-package bind-key)

;; Basic editing improvements
(use-package which-key
  :config
  (which-key-mode 1))

;; Load profile-specific configuration based on Nix profile
(defun load-profile-config ()
  "Load configuration specific to the current Nix profile."
  (let ((profile (getenv "EMACS_PROFILE")))
    (pcase profile
      ("server"
       (message "Loading minimal server configuration...")
       ;; Minimal config - only essential features
       (setq gc-cons-threshold (* 2 1000 1000))) ; Lower memory usage
      
      ("laptop" 
       (message "Loading laptop development configuration...")
       ;; Laptop config - balanced features
       (setq auto-save-timeout 30)               ; More frequent saves
       (setq lsp-idle-delay 0.3))                ; Moderate LSP responsiveness
      
      ("workstation"
       (message "Loading workstation configuration...")
       ;; Workstation config - maximum performance
       (setq gc-cons-threshold (* 50 1000 1000)) ; Higher performance
       (setq lsp-idle-delay 0.1))                ; Fastest LSP response
      
      (_
       (message "Loading default configuration...")))))

;; Apply profile-specific settings
(load-profile-config)

;; Configure Nix integration after packages are loaded
(add-hook 'after-init-hook #'configure-nix-lsp-servers)
(add-hook 'after-init-hook #'configure-nix-formatters)

;; Org mode basic setup (always included)
(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t))

;; Module loading system
;; Load modules based on availability and profile
(defvar my-modules-dir
  (if (getenv "EMACS_PROFILE")
      "/etc/emacs/modules/" ; System modules for Nix environment
    (expand-file-name "modules/" user-emacs-directory)) ; User modules for non-Nix
  "Directory containing modular configuration files.")

(defun load-module (module-name)
  "Load MODULE-NAME from the modules directory."
  (let ((module-file (expand-file-name (concat module-name ".el") my-modules-dir)))
    (when (file-exists-p module-file)
      (load-file module-file)
      (message "Loaded module: %s" module-name))))

;; Load modules based on profile
(let ((profile (getenv "EMACS_PROFILE")))
  (pcase profile
    ("nox"
     ;; Minimal modules for terminal use
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
     (load-module "claude-code"))
    
    (_
     ;; Default module loading (non-Nix environment)
     (load-module "ui")
     (load-module "completion")
     (load-module "navigation"))))

;; Display startup information
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((profile (getenv "EMACS_PROFILE")))
              (message "=== Emacs Ready ===")
              (message "Profile: %s" (or profile "default"))
              (message "Nix Integration: %s" (if profile "enabled" "disabled"))
              (message "Modules loaded based on profile")
              (message "==================="))))

;;; init.el ends here
