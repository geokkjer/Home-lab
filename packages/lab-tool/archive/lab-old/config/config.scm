;; lab/core/config.scm - Configuration functionality

(define-module (lab core config)
  #:use-module (ice-9 format)
  #:export (get-all-machines
            get-machine-config
            get-ssh-config
            get-homelab-root
            option-ref))

(define (option-ref options key default)
  "Get option value from options alist with default"
  (let ((value (assoc-ref options key)))
    (if value value default)))

(define (get-all-machines)
  "Get list of all machines"
  '(grey-area sleeper-service congenital-optimist reverse-proxy))

(define (get-machine-config machine-name)
  "Get configuration for a machine"
  `((services . (systemd ssh))
    (type . server)))

(define (get-ssh-config machine-name)
  "Get SSH configuration for a machine"
  (let ((hostname (symbol->string machine-name)))
    `((hostname . ,hostname)
      (user . "sma")
      (identity-file . "~/.ssh/id_ed25519_admin")
      (is-local . #f)))

(define (get-homelab-root)
  "Get home lab root directory"
  "/home/geir/Home-lab")
