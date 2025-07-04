;; utils/config.scm - Declarative configuration for Home Lab Tool

(define-module (utils config)
  #:export (default-config
            get-config-value
            host-configs
            get-host-config
            get-all-hosts
            validate-host-name
            get-ssh-config))

;; Declarative configuration (source of truth)
(define default-config
  '((ssh-user . "sma")
    (hosts . ((congenital-optimist (hostname . "congenital-optimist"))
              (sleeper-service (hostname . "sleeper-service"))
              (grey-area (hostname . "grey-area"))
              (reverse-proxy (hostname . "reverse-proxy"))
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
        (ssh-user (get-config-value '(ssh-user) "sma")))
    (if host-config
        (let ((hostname (assoc-ref host-config 'hostname)))
          `((hostname . ,hostname)
            (user . ,ssh-user)
            (ssh-user . ,ssh-user)
            (identity-file . "~/.ssh/id_ed25519_admin")))
        #f)))
