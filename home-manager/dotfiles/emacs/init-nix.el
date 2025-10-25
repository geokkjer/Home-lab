;;; init-nix.el --- Home Manager managed Emacs init (placeholder)
;;; Commentary:
;; This is a placeholder Emacs init file intended to be deployed via Home Manager:
;; ~/.config/emacs/init-nix.el
;;
;; It implements a lightweight bootstrap that:
;; - honors an EMACS_PROFILE environment variable (set by Home Manager)
;; - loads modular config files from ~/.config/emacs/modules/
;; - loads profile-specific fragments from ~/.config/emacs/profiles/
;; - provides safe defaults so Emacs is usable before full migration
;;
;; Replace and extend the sections below with your real configuration. Keep the
;; modular loader stable; Home Manager will manage the files under ~/.config/emacs/.

;;; Code:

;; Ensure Emacs uses XDG directories if available (Home Manager will place files in ~/.config)
(when (boundp 'user-emacs-directory)
  ;; prefer XDG if present
  (setq user-emacs-directory
        (or (getenv "XDG_CONFIG_HOME")
            (expand-file-name ".config/" (getenv "HOME"))))
  ;; Ensure trailing slash for later path construction
  (unless (string-suffix-p "/" user-emacs-directory)
    (setq user-emacs-directory (concat user-emacs-directory "/"))))

;; Primary config base directory (where Home Manager will place files)
(defvar init-nix-config-dir (expand-file-name "emacs/" user-emacs-directory)
  "Base directory for Home Manager-managed Emacs configuration.")

;; Ensure directories exist (non-fatal if they don't)
(let ((modules-dir (expand-file-name "modules/" init-nix-config-dir))
      (profiles-dir (expand-file-name "profiles/" init-nix-config-dir)))
  (defvar init-nix-modules-dir modules-dir "Directory for modular Emacs config.")
  (defvar init-nix-profiles-dir profiles-dir "Directory for profile-specific config."))

;; Basic package bootstrap (placeholder)
;; If you use `straight.el`, `use-package`, or built-in package.el, replace this bootstrap.
(require 'package)
(setq package-enable-at-startup nil) ;; we call package-initialize explicitly
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(unless (file-exists-p package-user-dir)
  (make-directory package-user-dir t))
(package-initialize)

;; Ensure `use-package` exists as a convenience for later modules; don't error if offline.
(unless (package-installed-p 'use-package)
  (ignore-errors
    (package-refresh-contents)
    (package-install 'use-package)))
(eval-when-compile
  (require 'use-package nil t))
(setq use-package-always-ensure t)

;; Determine active profile: prefer EMACS_PROFILE env var set by Home Manager, fallback to "development"
(defvar init-nix-active-profile
  (or (getenv "EMACS_PROFILE") "development")
  "Active Emacs profile name used to select profile-specific configuration.")

;; Simple logger
(defun init-nix-log (fmt &rest args)
  "Log a formatted message FMT with ARGS to *Messages* prefixed by init-nix."
  (apply #'message (concat "[init-nix] " fmt) args))

(init-nix-log "Starting init for profile: %s" init-nix-active-profile)

;; Utility: load module if exists
(defun init-nix-load-file-if-exists (path)
  "Load Emacs Lisp file at PATH if it exists."
  (when (file-exists-p path)
    (condition-case err
        (progn
          (load-file path)
          (init-nix-log "Loaded %s" path)
          t)
      (error
       (init-nix-log "Error loading %s: %s" path err)
       nil))))

;; Load core modules (if present). Keep this list minimal; modules can require each other.
(let ((core-modules '("ui.el" "packages.el" "completion.el" "lsp.el")))
  (dolist (m core-modules)
    (let ((file (expand-file-name m init-nix-modules-dir)))
      (init-nix-load-file-if-exists file))))

;; Load profile-specific file: ~/.config/emacs/profiles/<profile>.el
(let ((profile-file (expand-file-name (concat init-nix-active-profile ".el") init-nix-profiles-dir)))
  (when (init-nix-load-file-if-exists profile-file)
    (init-nix-log "Profile %s loaded from %s" init-nix-active-profile profile-file)))

;; Backwards compatibility: load top-level init.el if present (for non-nix setups)
(let ((legacy-init (expand-file-name "init.el" (expand-file-name "~/.emacs.d/"))))
  (unless (file-exists-p (expand-file-name init-nix-config-dir))
    (init-nix-load-file-if-exists legacy-init)))

;; Basic UI defaults for placeholder
(unless (daemonp)
  ;; Only set UI defaults in non-daemon sessions as placeholders
  (when (display-graphic-p)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1))
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message ";; Emacs (Home Manager placeholder)\n"))

;; Defer heavy init until idle to keep startup snappy (placeholder)
(run-with-idle-timer
 1 nil
 (lambda ()
   (init-nix-log "Deferred init tasks running")
   ;; Example: ensure a few convenient packages are installed
   (dolist (pkg '(which-key magit))
     (unless (package-installed-p pkg)
       (ignore-errors
         (package-refresh-contents)
         (package-install pkg))))
   ;; Enable which-key if present
   (when (require 'which-key nil t)
     (which-key-mode))))

;; Final message
(init-nix-log "init-nix.el bootstrap complete — profile: %s" init-nix-active-profile)

(provide 'init-nix)
;;; init-nix.el ends here
