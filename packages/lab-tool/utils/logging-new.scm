;; utils/logging.scm - Logging facade (aggregates modular components)

(define-module (utils logging)
  #:use-module (utils logging format)
  #:use-module (utils logging level)
  #:use-module (utils logging state)
  #:use-module (utils logging output)
  #:use-module (utils logging core)
  #:use-module (utils logging spinner)
  #:re-export (;; Core logging functions
               log-debug
               log-info
               log-warn
               log-error
               log-success
               
               ;; State management
               get-current-log-level
               set-log-level!
               should-log?
               
               ;; Pure functions (for testing and functional composition)
               should-log-pure
               validate-log-level
               format-timestamp
               format-log-message
               get-color
               log-message-pure
               
               ;; Utilities
               with-spinner))

;; This module acts as a facade for logging functionality,
;; aggregating specialized modules that follow single responsibility:
;; - format: Pure formatting functions and color codes
;; - level: Pure log level management and validation
;; - state: Mutable state management for current log level
;; - output: Pure output formatting and port writing
;; - core: Main logging functions with side effects
;; - spinner: Progress indication for long operations
;;
;; Both pure and impure functions are available for maximum flexibility.
