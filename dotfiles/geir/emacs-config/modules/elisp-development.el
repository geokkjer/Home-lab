;;; elisp-development.el --- Enhanced Emacs Lisp development setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Specialized configuration for Emacs Lisp development
;; This module provides enhanced development tools specifically for .el files

;;; Code:

;; Enhanced Emacs Lisp mode with better defaults
(use-package elisp-mode
  :ensure nil ; Built-in package
  :mode "\\.el\\'"
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . show-paren-mode)
         (emacs-lisp-mode . electric-pair-mode))
  :bind (:map emacs-lisp-mode-map
         ("C-c C-e" . eval-last-sexp)
         ("C-c C-b" . eval-buffer)
         ("C-c C-r" . eval-region)
         ("C-c C-d" . describe-function-at-point))
  :config
  ;; Better indentation
  (setq lisp-indent-function 'lisp-indent-function)
  
  ;; Show function signatures in minibuffer
  (eldoc-mode 1))

;; Enhanced Elisp navigation
(use-package elisp-slime-nav
  :hook (emacs-lisp-mode . elisp-slime-nav-mode)
  :bind (:map elisp-slime-nav-mode-map
         ("M-." . elisp-slime-nav-find-elisp-thing-at-point)
         ("M-," . pop-tag-mark)))

;; Better parentheses handling
(use-package smartparens
  :hook (emacs-lisp-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil))

;; Rainbow delimiters for better paren visibility
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Aggressive indentation
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; Enhanced help and documentation
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h ." . helpful-at-point)))

;; Live examples for Elisp functions
(use-package elisp-demos
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; Package linting
(use-package package-lint
  :if (not (string-equal system-name "little-rascal"))
  :commands package-lint-current-buffer)

;; Flycheck for syntax checking
(use-package flycheck
  :if (not (string-equal system-name "little-rascal"))
  :hook (emacs-lisp-mode . flycheck-mode)
  :config
  ;; Enhanced Emacs Lisp checking
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Checkdoc for documentation linting
(use-package checkdoc
  :ensure nil ; Built-in
  :commands checkdoc)

;; Enhanced debugging
(use-package edebug
  :ensure nil ; Built-in
  :bind (:map emacs-lisp-mode-map
         ("C-c C-x C-d" . edebug-defun)
         ("C-c C-x C-b" . edebug-set-breakpoint)))

;; Package development helpers
(use-package auto-compile
  :if (and (package-installed-p 'auto-compile)
           (not (string-equal system-name "little-rascal")))
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Enhanced REPL interaction
(use-package ielm
  :ensure nil ; Built-in
  :bind ("C-c C-z" . ielm)
  :config
  (add-hook 'ielm-mode-hook 'eldoc-mode))

;; Highlight defined functions and variables
(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

;; Better search and replace for symbols
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Multiple cursors for batch editing
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; Custom functions for Elisp development
(defun elisp-eval-and-replace ()
  "Evaluate the sexp at point and replace it with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun elisp-describe-thing-at-point ()
  "Show the documentation for the thing at point."
  (interactive)
  (let ((thing (symbol-at-point)))
    (cond
     ((fboundp thing) (describe-function thing))
     ((boundp thing) (describe-variable thing))
     (t (message "No documentation found for %s" thing)))))

;; Key bindings for custom functions
(define-key emacs-lisp-mode-map (kbd "C-c C-x C-e") 'elisp-eval-and-replace)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'elisp-describe-thing-at-point)

;; Project-specific configurations
(defun setup-elisp-project ()
  "Set up development environment for Elisp projects."
  (interactive)
  (when (and buffer-file-name
             (string-match "\\.el\\'" buffer-file-name))
    ;; Add current directory to load-path for local requires
    (add-to-list 'load-path (file-name-directory buffer-file-name))
    
    ;; Set up package development if this looks like a package
    (when (or (file-exists-p "Cask")
              (file-exists-p "Eask")
              (string-match "-pkg\\.el\\'" buffer-file-name))
      (message "Elisp package development mode enabled"))))

(add-hook 'emacs-lisp-mode-hook 'setup-elisp-project)

;; Better compilation output
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (when buffer-file-name
              (setq-local compile-command
                          (format "emacs -batch -f batch-byte-compile %s"
                                  (shell-quote-argument buffer-file-name))))))

(provide 'elisp-development)
;;; elisp-development.el ends here
