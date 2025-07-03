;;; claude-code.el --- Claude Code CLI integration module -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration with Claude Code CLI for AI-assisted coding directly in Emacs
;; Provides terminal interface and commands for interacting with Claude AI

;;; Code:

;; Install claude-code via quelpa if not already installed
(unless (package-installed-p 'claude-code)
  (quelpa '(claude-code :fetcher git :url "https://github.com/stevemolitor/claude-code.el.git")))

;; Claude Code - AI assistant integration
(use-package claude-code
  :ensure nil  ; Already installed via quelpa
  :bind-keymap ("C-c C-c" . claude-code-command-map)
  :custom
  ;; Terminal backend preference (eat is now installed via quelpa)
  (claude-code-terminal-type 'eat)
  
  ;; Enable desktop notifications
  (claude-code-notifications t)
  
  ;; Startup delay to ensure proper initialization
  (claude-code-startup-delay 1.0)
  
  ;; Confirm before killing Claude sessions
  (claude-code-confirm-kill t)
  
  ;; Use modern keybinding style
  (claude-code-newline-and-send-style 'modern)
  
  :config
  (claude-code-mode)
  ;; Smart terminal detection - eat should be available via quelpa
  (defun claude-code-detect-best-terminal ()
    "Detect the best available terminal for Claude Code."
    (cond
     ((package-installed-p 'eat) 'eat)
     ((and (package-installed-p 'vterm) 
           (or (executable-find "cmake") 
               (file-exists-p "/usr/bin/cmake")
               (file-exists-p "/nix/store/*/bin/cmake")))
      'vterm)
     (t 'eat))) ; fallback to eat, should be installed
  
  ;; Set terminal type based on detection
  (setq claude-code-terminal-type (claude-code-detect-best-terminal))
  
  ;; Auto-start Claude in project root when opening coding files
  (defun claude-code-auto-start-maybe ()
    "Auto-start Claude Code if in a project and not already running."
    (when (and (derived-mode-p 'prog-mode)
               (project-current)
               (not (claude-code-running-p)))
      (claude-code)))
  
  ;; Optional: Auto-start when opening programming files
  ;; Uncomment the next line if you want this behavior
  ;; (add-hook 'prog-mode-hook #'claude-code-auto-start-maybe)
  
  ;; Add helpful message about Claude Code setup
  (message "Claude Code module loaded. Use C-c C-c c to start Claude, C-c C-c h for help"))

;; Terminal emulator for Claude Code (eat installed via quelpa in init.el)
(use-package eat
  :ensure nil  ; Already installed via quelpa
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t))

;; Alternative terminal emulator (if eat fails or user prefers vterm)
(use-package vterm
  :if (and (not (package-installed-p 'eat)) 
           (executable-find "cmake"))
  :custom
  (vterm-always-compile-module t)
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000))

;; Transient dependency for command menus
(use-package transient
  :ensure t)

;; Enhanced error handling for Claude Code integration
(defun claude-code-send-error-context ()
  "Send error at point with surrounding context to Claude."
  (interactive)
  (if (claude-code-running-p)
      (let* ((error-line (line-number-at-pos))
             (start (max 1 (- error-line 5)))
             (end (min (line-number-at-pos (point-max)) (+ error-line 5)))
             (context (buffer-substring-no-properties
                      (line-beginning-position (- start error-line))
                      (line-end-position (- end error-line)))))
        (claude-code-send-command 
         (format "I'm getting an error around line %d. Here's the context:\n\n```%s\n%s\n```\n\nCan you help me fix this?"
                 error-line
                 (or (and buffer-file-name (file-name-extension buffer-file-name)) "")
                 context)))
    (message "Claude Code is not running. Start it with C-c C-c c")))

;; Keybinding for enhanced error context
(global-set-key (kbd "C-c c x") #'claude-code-send-error-context)

;; Project-aware Claude instances
(defun claude-code-project-instance ()
  "Start or switch to Claude instance for current project."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (claude-code))
    (claude-code)))

;; Keybinding for project-specific Claude
(global-set-key (kbd "C-c c p") #'claude-code-project-instance)

(provide 'claude-code)
;;; claude-code.el ends here
