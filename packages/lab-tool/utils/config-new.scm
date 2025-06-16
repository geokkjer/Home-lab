;; utils/config.scm - Configuration management facade

(define-module (utils config)
  #:use-module (utils config defaults)
  #:use-module (utils config loader)
  #:use-module (utils config accessor)
  #:use-module (utils config state)
  #:re-export (;; State management
               get-current-config
               set-current-config!
               reload-config!
               
               ;; Stateful accessors (work with current config)
               get-config-value
               get-machine-config
               get-all-machines
               get-ssh-config
               validate-machine-name
               get-homelab-root
               
               ;; Pure accessors (require explicit config parameter)
               get-config-value-pure
               get-machine-config-pure
               get-all-machines-pure
               get-ssh-config-pure
               validate-machine-name-pure
               
               ;; Loading functions
               load-config
               load-config-from-file
               
               ;; Default configuration
               default-config))

;; This module acts as a facade for configuration management,
;; aggregating specialized modules that follow single responsibility:
;; - defaults: Pure data definitions
;; - loader: File I/O operations  
;; - accessor: Pure configuration value access
;; - state: Mutable state management
;;
;; Both pure and impure functions are available, allowing callers
;; to choose the appropriate level of functional purity.
