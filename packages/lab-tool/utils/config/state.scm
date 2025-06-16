;; utils/config/state.scm - Configuration state management

(define-module (utils config state)
  #:use-module (utils config defaults)
  #:use-module (utils config loader)
  #:use-module (utils config accessor)
  #:use-module (utils logging)
  #:export (get-current-config
            set-current-config!
            reload-config!
            get-config-value
            get-machine-config
            get-all-machines
            get-ssh-config
            validate-machine-name
            get-homelab-root))

;; Mutable state: Current loaded configuration
(define current-config default-config)

;; Impure function: Get current configuration
(define (get-current-config)
  "Get current loaded configuration"
  current-config)

;; Impure function: Set current configuration
(define (set-current-config! config)
  "Set current configuration (impure)"
  (set! current-config config))

;; Impure function: Reload configuration from file
(define (reload-config! . args)
  "Reload configuration from file"
  (let ((new-config (apply load-config args)))
    (set-current-config! new-config)
    new-config))

;; Impure wrappers for pure accessor functions
(define (get-config-value path . default)
  "Get configuration value from current config"
  (apply get-config-value-pure current-config path default))

(define (get-machine-config machine-name)
  "Get machine configuration from current config"
  (get-machine-config-pure current-config machine-name))

(define (get-all-machines)
  "Get all machine names from current config"
  (get-all-machines-pure current-config))

(define (get-ssh-config machine-name)
  "Get SSH configuration from current config"
  (get-ssh-config-pure current-config machine-name))

(define (validate-machine-name machine-name)
  "Validate machine name against current config"
  (if (validate-machine-name-pure current-config machine-name)
      #t
      (begin
        (log-error "Unknown machine: ~a" machine-name)
        (log-error "Available machines: ~a" (string-join (get-all-machines) ", "))
        #f)))

(define (get-homelab-root)
  "Get home lab root directory from current config"
  (get-config-value '(homelab-root) "/home/geir/Home-lab"))

;; Initialize configuration on module load
(reload-config!)
