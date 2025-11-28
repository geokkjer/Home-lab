;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Increase garbage collection threshold during startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Disable package.el in favor of use-package setup later
(setq package-enable-at-startup nil)

;; Disable UI elements early to avoid flickering
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
