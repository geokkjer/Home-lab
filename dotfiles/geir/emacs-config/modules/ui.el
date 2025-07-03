;;; ui.el --- UI configuration module -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced UI configuration - themes, modeline, icons

;;; Code:

;; Doom themes
(use-package doom-themes
  :config
  (load-theme 'doom-monokai-pro t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project))

;; All the icons
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  ;; Install fonts if not already done
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(provide 'ui)
;;; ui.el ends here