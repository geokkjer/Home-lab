;;; navigation.el --- Navigation and file management -*- lexical-binding: t; -*-

;;; Commentary:
;; File navigation, project management, and window management

;;; Code:

;; Dired improvements
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alhF")
  (dired-dwim-target t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;; Project management
(use-package projectile
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'default))

;; Treemacs - file tree
(use-package treemacs
  :bind (("M-0" . treemacs-select-window)
         ("C-x t 1" . treemacs-delete-other-windows)
         ("C-x t t" . treemacs)
         ("C-x t d" . treemacs-select-directory)
         ("C-x t B" . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag))
  :custom
  (treemacs-width 30))

;; Ace Window - quick window switching
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Winner mode - window configuration undo/redo
(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(provide 'navigation)
;;; navigation.el ends here