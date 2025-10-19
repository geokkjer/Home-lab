;;; ai-integration.el --- GPTel and AI integration module -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern AI integration using GPTel with multiple backend support
;; Provides buffer-native AI interaction with streaming responses
;; Supports multiple backends: OpenAI, Anthropic, Ollama, etc.

;;; Code:

;; GPTel - Modern LLM client for Emacs
(use-package gptel
  :bind-keymap ("C-c g" . gptel-menu-map)
  :bind (("C-c g c" . gptel)
         ("C-c g s" . gptel-send)
         ("C-c g r" . gptel-rewrite-and-replace)
         ("C-c g k" . gptel-abort)
         ("C-c g n" . gptel-menu)
         ("C-c g e" . gptel-explain-region)
         ("C-c g v" . gptel-review-region)
         ("C-c g x" . gptel-send-error-context)
         ("C-c g p" . gptel-project-context)
         ("C-c g m" . gptel-commit-message)
         ("C-c g t" . gptel-quick-chat)
         ("C-c g f" . gptel-add-file-context))

  :custom
  ;; Default model and backend (will be set by backend configuration)
  (gptel-model 'gpt-4o-mini)
  (gptel-default-mode 'org-mode)

  ;; Response behavior
  (gptel-stream t)
  (gptel-use-curl t)

  ;; UI preferences
  (gptel-display-buffer-action '(display-buffer-same-window))

  ;; Safety settings
  (gptel-crowdsource-default nil) ; Don't share by default

  :config
  ;; GitHub Copilot handles authentication automatically
  ;; No API key configuration needed

  ;; Backend configuration
  (gptel-ai-integration-setup-backends)

  ;; Post-response hook for additional processing
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response-marker)

  ;; Message about GPTel setup
  (message "GPTel AI integration loaded (Copilot + Ollama). Use C-c g c to start, C-c g n for menu"))

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
      (error (message "Grey-area Ollama not reachable - check network connection")))

    ;; Local Ollama fallback (if running locally)
    (condition-case nil
        (when (and (executable-find "ollama")
                   (zerop (call-process "ollama" nil nil nil "ps")))
          (gptel-make-ollama "Local Ollama"
            :host "localhost:11434"
            :stream t
            :models '(llama3.2:latest codellama:latest mistral:latest qwen2.5:latest))
          (push "Local Ollama" configured-backends))
      (error nil))

    ;; Display configured backends
    (if configured-backends
        (message "GPTel configured with backends: %s" (string-join configured-backends ", "))
      (message "GPTel: No backends available. Check GitHub authentication and Ollama connectivity"))))

;; Enhanced project integration
(defun gptel-project-context ()
  "Start GPTel with project context and appropriate naming."
  (interactive)
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (project-name (if project-root
                          (file-name-nondirectory (directory-file-name project-root))
                        "Global"))
         (buffer-name (format "*GPTel-%s*" project-name))
         (default-directory (or project-root default-directory)))

    ;; Switch to or create project-specific GPTel buffer
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (gptel buffer-name))

    ;; Add project context if it's a new session
    (when (and project-root (= (buffer-size) 0))
      (insert (format "# Project: %s\n\nWorking directory: %s\n\n"
                      project-name default-directory))
      (when (file-exists-p "README.md")
        (insert "## Project README\n")
        (insert-file-contents "README.md" nil 0 500)
        (insert "\n\n")))))

;; Error context helper (similar to claude-code functionality)
(defun gptel-send-error-context ()
  "Send error context around point to GPTel with smart context detection."
  (interactive)
  (let* ((error-line (line-number-at-pos))
         (start-line (max 1 (- error-line 15)))
         (end-line (min (line-number-at-pos (point-max)) (+ error-line 15)))
         (context (string-trim
                  (buffer-substring-no-properties
                   (save-excursion (goto-line start-line) (point))
                   (save-excursion (goto-line end-line) (point)))))
         (filename (if buffer-file-name
                      (file-name-nondirectory buffer-file-name)
                    (buffer-name)))
         (file-ext (when buffer-file-name
                    (file-name-extension buffer-file-name)))
         (language (or file-ext
                      (when (boundp 'major-mode)
                        (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
                      "text"))
         (prompt (format "I'm encountering an issue in %s around line %d. Here's the context:\n\n```%s\n%s\n```\n\nCan you help me understand what might be wrong and suggest a fix?"
                        filename error-line language context)))

    ;; Send to GPTel
    (gptel-request prompt :buffer (gptel-project-context))))

;; Code explanation helper
(defun gptel-explain-region (beg end)
  "Explain the selected region or function at point."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     ;; If no region, try to get current function
     (let ((bounds (bounds-of-thing-at-point 'defun)))
       (if bounds
           (list (car bounds) (cdr bounds))
         (list (point-at-bol) (point-at-eol))))))

  (let* ((code (buffer-substring-no-properties beg end))
         (language (or (when buffer-file-name
                        (file-name-extension buffer-file-name))
                      (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))
                      "text"))
         (prompt (format "Please explain this %s code in detail:\n\n```%s\n%s\n```\n\nExplain what it does, how it works, and any important patterns or concepts."
                        language language code)))

    (gptel-request prompt :buffer (gptel-project-context))))

;; Code review helper
(defun gptel-review-region (beg end)
  "Request code review for the selected region or function at point."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     ;; If no region, try to get current function
     (let ((bounds (bounds-of-thing-at-point 'defun)))
       (if bounds
           (list (car bounds) (cdr bounds))
         (list (point-at-bol) (point-at-eol))))))

  (let* ((code (buffer-substring-no-properties beg end))
         (language (or (when buffer-file-name
                        (file-name-extension buffer-file-name))
                      (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))
                      "text"))
         (prompt (format "Please review this %s code for:\n\n```%s\n%s\n```\n\n1. Potential bugs or issues\n2. Performance improvements\n3. Code style and best practices\n4. Security considerations\n5. Maintainability suggestions"
                        language language code)))

    (gptel-request prompt :buffer (gptel-project-context))))

;; Quick chat function for rapid queries
(defun gptel-quick-chat (query)
  "Quick GPTel query without opening a dedicated buffer."
  (interactive "sQuick query: ")
  (when (string-trim query)
    (gptel-request query
                   :callback (lambda (response info)
                              (when response
                                (with-current-buffer (get-buffer-create "*GPTel Quick Response*")
                                  (erase-buffer)
                                  (insert (format "Query: %s\n\n" query))
                                  (insert response)
                                  (goto-char (point-min))
                                  (display-buffer (current-buffer))))))))

;; Commit message generation
(defun gptel-commit-message ()
  "Generate commit message based on git diff."
  (interactive)
  (cond
   ;; Check if we're in a git repository
   ((not (locate-dominating-file default-directory ".git"))
    (message "Not in a git repository"))

   ;; Check if there are staged changes
   ((zerop (call-process "git" nil nil nil "diff" "--cached" "--quiet"))
    (message "No staged changes found"))

   ;; Generate commit message
   (t
    (let ((diff (shell-command-to-string "git diff --cached")))
      (if (string-empty-p diff)
          (message "No staged changes")
        (gptel-request
         (format "Based on this git diff, write a clear, concise commit message following conventional commits format (type(scope): description). Focus on what changed and why:\n\n```diff\n%s\n```\n\nReturn only the commit message, no explanation."
                 diff)
         :callback (lambda (response info)
                     (when (and response (not (string-empty-p (string-trim response))))
                       (let ((commit-msg (string-trim response)))
                         (kill-new commit-msg)
                         (message "Commit message copied: %s" commit-msg)
                         ;; Optionally show in a buffer for editing
                         (when current-prefix-arg
                           (with-current-buffer (get-buffer-create "*Commit Message*")
                             (erase-buffer)
                             (insert commit-msg)
                             (display-buffer (current-buffer)))))))))))))

;; File context integration
(defun gptel-add-file-context (file)
  "Add file contents as context to current GPTel session."
  (interactive "fAdd file context: ")
  (when (and (file-exists-p file) (file-readable-p file))
    (let* ((relative-path (file-relative-name file))
           (contents (with-temp-buffer
                      (insert-file-contents file nil 0 2000) ; Limit to first 2KB
                      (buffer-string))))
      (insert (format "\n## File: %s\n\n```%s\n%s\n```\n\n"
                      relative-path
                      (or (file-name-extension file) "")
                      contents)))))

;; Response processing
(defun gptel-end-of-response-marker (start end)
  "Add a subtle marker at the end of AI responses."
  (save-excursion
    (goto-char end)
    (unless (looking-back "\n\n" 2)
      (insert "\n"))
    (insert "---\n")))

;; Org mode integration
(with-eval-after-load 'org
  (defun gptel-org-set-topic ()
    "Set a topic/context for the current Org buffer session."
    (interactive)
    (let ((topic (read-string "Session topic: ")))
      (when (not (string-empty-p topic))
        (save-excursion
          (goto-char (point-min))
          (insert (format "#+TITLE: %s\n#+GPTel_CONTEXT: %s\n\n" topic topic))))))

  ;; Add GPTel bindings to Org mode
  (define-key org-mode-map (kbd "C-c g f") #'gptel-org-set-topic))

;; Development workflow integration
(with-eval-after-load 'magit
  ;; Add commit message generation to magit
  (define-key magit-status-mode-map (kbd "C-c g m") #'gptel-commit-message))

;; Programming mode enhancements
(defun gptel-programming-setup ()
  "Setup GPTel integrations for programming modes."
  (local-set-key (kbd "C-c g d") #'gptel-explain-region)
  (local-set-key (kbd "C-c g r") #'gptel-review-region)
  (local-set-key (kbd "C-c g x") #'gptel-send-error-context))

;; Add to programming mode hooks
(add-hook 'prog-mode-hook #'gptel-programming-setup)

;; Backend switching helper
(defun gptel-switch-backend ()
  "Switch between available GPTel backends."
  (interactive)
  (let ((backends (mapcar #'car gptel--known-backends)))
    (when backends
      (let ((choice (completing-read "Switch to backend: " backends)))
        (setq gptel-backend (gptel-get-backend choice))
        (message "Switched to %s backend" choice)))))

;; Add backend switching to global bindings
(global-set-key (kbd "C-c g b") #'gptel-switch-backend)

;; Optional: Helpful tips system
(defvar gptel-tips-shown nil
  "Whether GPTel tips have been shown in this session.")

(defun gptel-show-usage-tips ()
  "Show helpful GPTel usage tips."
  (interactive)
  (with-current-buffer (get-buffer-create "*GPTel Tips*")
    (erase-buffer)
    (insert "# GPTel Usage Tips\n\n")
    (insert "## Key Bindings:\n")
    (insert "- C-c g c: Start GPTel chat\n")
    (insert "- C-c g s: Send current region/prompt\n")
    (insert "- C-c g e: Explain code region\n")
    (insert "- C-c g v: Review code region\n")
    (insert "- C-c g x: Send error context\n")
    (insert "- C-c g p: Project-specific chat\n")
    (insert "- C-c g m: Generate commit message\n")
    (insert "- C-c g t: Quick chat\n")
    (insert "- C-c g n: Open GPTel menu\n")
    (insert "- C-c g b: Switch backend\n\n")
    (insert "## Backends:\n")
    (insert "- GitHub Copilot: Primary backend with latest models\n")
    (insert "- Grey-area Ollama: Local models on grey-area machine\n")
    (insert "- Local Ollama: Local models if running locally\n\n")
    (insert "## Tips:\n")
    (insert "- Use regions to focus AI attention on specific code\n")
    (insert "- Project context automatically includes README snippets\n")
    (insert "- Error context sends surrounding code for debugging\n")
    (insert "- Commit messages are generated from staged changes\n")
    (insert "- Quick chat is good for simple questions\n\n")
    (insert "## Security:\n")
    (insert "- GitHub Copilot handles authentication automatically\n")
    (insert "- Local Ollama models provide privacy\n")
    (insert "- No API keys stored locally\n")
    (org-mode)
    (display-buffer (current-buffer))))

;; Show tips on first GPTel usage (optional)
(defun gptel-maybe-show-tips ()
  "Show tips if this is the first GPTel usage."
  (unless gptel-tips-shown
    (setq gptel-tips-shown t)
    (run-with-timer 2 nil #'gptel-show-usage-tips)))

;; Uncomment to enable tips on first use
;; (add-hook 'gptel-mode-hook #'gptel-maybe-show-tips)

;; Provide the module
(provide 'ai-integration)

;;; ai-integration.el ends here
