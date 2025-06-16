;; utils/config/defaults.scm - Configuration defaults (pure data)

(define-module (utils config defaults)
  #:export (default-config))

;; Pure data: Default configuration structure
(define default-config
  `((homelab-root . "/home/geir/Home-lab")
    (machines . ((congenital-optimist 
                  (type . local)
                  (hostname . "localhost")
                  (services . (workstation development)))
                 (sleeper-service
                  (type . remote)
                  (hostname . "sleeper-service.tail807ea.ts.net")
                  (ssh-alias . "admin-sleeper")
                  (services . (nfs zfs storage)))
                 (grey-area
                  (type . remote)
                  (hostname . "grey-area.tail807ea.ts.net")
                  (ssh-alias . "admin-grey")
                  (services . (ollama forgejo git)))
                 (reverse-proxy
                  (type . remote)
                  (hostname . "reverse-proxy.tail807ea.ts.net")
                  (ssh-alias . "admin-reverse")
                  (services . (nginx proxy ssl)))))
    (deployment . ((default-mode . "boot")
                   (timeout . 300)
                   (retry-count . 3)))
    (monitoring . ((interval . 30)
                   (timeout . 10)))
    (mcp . ((port . 3001)
            (host . "localhost")
            (log-level . "info")))))
