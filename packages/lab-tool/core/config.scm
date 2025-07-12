;; core/config.scm - Pure config data and accessors

(define-module (core config)
  #:use-module (srfi srfi-1)  ; for fold
  #:export (default-config
            get-config-value
            host-configs
            get-host-config
            get-all-hosts
            validate-host-name
            get-ssh-config
            get-flake-path
            get-ssh-key))

;; Declarative configuration (source of truth)
(define default-config
  '((ssh-user . "sma")
    (ssh-key . "~/.ssh/id_ed25519_admin")
    (flake-path . "~/Projects/home-lab")
    (hosts . ((congenital-optimist (hostname . "congenital-optimist"))
              (sleeper-service (hostname . "sleeper-service"))
              (grey-area (hostname . "grey-area"))
              (reverse-proxy (hostname . "reverse-proxy"))
              (limiting-factor (hostname . "limiting-factor"))
              (little-rascal (hostname . "little-rascal"))))
    (deployment . ((default-mode . "boot")
                   (timeout . 300)
                   (retry-count . 3)))
    (monitoring . ((interval . 30)
                   (timeout . 10)))
    (mcp . ((port . 3001)
            (host . "localhost")
            (log-level . "info")))))

;; Accessors (pure, no mutation, no IO)
(define (get-config-value path . default)
  (let ((result (fold (lambda (key acc)
                        (if (and acc (list? acc))
                            (assoc-ref acc key)
                            #f))
                      default-config path)))
    (if result 
        result 
        (if (null? default) #f (car default)))))

(define (host-configs)
  (get-config-value '(hosts)))

(define (get-host-config host-name)
  (let ((host-symbol (if (symbol? host-name)
                         host-name
                         (string->symbol host-name))))
    (assoc-ref (host-configs) host-symbol)))

(define (get-all-hosts)
  (map (lambda (host-entry)
         (symbol->string (car host-entry)))
       (host-configs)))

(define (validate-host-name host-name)
  (let ((hosts (get-all-hosts)))
    (if (member host-name hosts)
        #t
        #f)))

(define (get-ssh-config host-name)
  (let ((host-config (get-host-config host-name))
        (ssh-user (get-config-value '(ssh-user) "sma"))
        (ssh-key (get-config-value '(ssh-key) "~/.ssh/id_ed25519_admin")))
    (if host-config
        (let ((hostname (assoc-ref host-config 'hostname)))
          `((hostname . ,hostname)
            (user . ,ssh-user)
            (ssh-user . ,ssh-user)
            (identity-file . ,ssh-key)))
        #f)))

(define (get-flake-path)
  (get-config-value '(flake-path) "~/Projects/home-lab"))

(define (get-ssh-key)
  (get-config-value '(ssh-key) "~/.ssh/id_ed25519_admin"))
