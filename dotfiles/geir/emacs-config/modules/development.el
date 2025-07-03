;;; development.el --- Development tools configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP, Copilot, and other development tools

;;; Code:

;; LSP Mode
(use-package lsp-mode
  :hook ((prog-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-completion-provider :none) ; Use corfu instead
  :config
  (lsp-enable-which-key-integration t))

;; LSP UI
(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil))

;; Which Key - helpful for discovering keybindings
(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3))

;; Magit - Git interface
(use-package magit
  :bind ("C-x g" . magit-status))

(provide 'development)
;;; development.el ends here