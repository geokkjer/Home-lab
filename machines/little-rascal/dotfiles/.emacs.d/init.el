;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple, modular Emacs configuration tangled from Emacs.org

;;; Code:

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Install use-package if not available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Disable UI clutter
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Visual settings
(set-face-attribute 'default nil :height 140)
(setq-default cursor-type 'bar)
(global-hl-line-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Text width
(setq-default fill-column 80)

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)

;; Remember cursor position
(save-place-mode 1)

;; Delete selection when typing
(delete-selection-mode 1)

;; Require final newline
(setq require-final-newline t)

;; Show matching parentheses
(show-paren-mode 1)

;; Better scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; Store backups and auto-saves in temp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Don't create lock files
(setq create-lockfiles nil)

;; Keep recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 50)

(use-package doom-themes
  :config
  (load-theme 'doom-monokai-pro t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-icon t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package vertico
  :init (vertico-mode)
  :custom (vertico-cycle t))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  :init (global-corfu-mode))

(use-package which-key
  :diminish
  :config (which-key-mode 1))

(use-package diminish)

(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t)
  (setq org-ellipsis " ▾")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)

  ;; Enable org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "markdown")
  :config
  ;; Nice visual formatting
  (setq markdown-hide-urls t)
  (setq markdown-fontify-code-blocks-natively t))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package project)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package vterm
  :commands vterm
  :config
  ;; Allow copy/paste with standard bindings
  (setq vterm-copy-exclude-prompt t)
  ;; Scroll up with mouse wheel
  (setq vterm-scroll-limit 10000)
  :bind (("C-x t" . vterm)
         :map vterm-mode-map
         ("C-c C-c" . vterm-send-ctrl-c)
         ("C-c C-z" . vterm-send-ctrl-z)
         ("C-c C-l" . vterm-clear)))

(use-package tidal
  :mode ("\\.tidal\\'" . tidal-mode)
  :custom
  (tidal-interpreter "ghci")
  ;; Search common locations for BootTidal.hs
  (tidal-boot-script-path
   (or (and (file-exists-p "~/.local/share/SuperCollider/BootTidal.hs")
            (expand-file-name "~/.local/share/SuperCollider/BootTidal.hs"))
       (car (file-expand-wildcards "~/.cabal/share/tidal-*/BootTidal.hs"))
       (expand-file-name "~/.local/share/SuperCollider/BootTidal.hs")))
  :config
  ;; Visual feedback when hushing
  (defun my/tidal-hush-notify ()
    "Hush all patterns with visual notification."
    (interactive)
    (tidal-hush)
    (message "🔇 All patterns silenced"))
  :bind (:map tidal-mode-map
         ("C-c C-s" . tidal-run-line)
         ("C-c C-e" . tidal-run-multiple-lines)
         ("C-c C-r" . tidal-run-region)
         ("C-c C-h" . my/tidal-hush-notify)
         ("C-<return>" . tidal-run-line)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons consult corfu diminish doom-modeline doom-themes magit
                   marginalia markdown-mode nix-mode orderless tidal vertico
                   vterm))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook #'org-babel-tangle nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
