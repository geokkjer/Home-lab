;; utils/config.scm - Configuration management for Home Lab Tool

(define-module (utils config)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (utils logging)
  #:use-module (utils json)
  #:export (load-config
            get-config-value
            machine-configs
            get-machine-config
            get-all-machines
            validate-machine-name
            get-homelab-root
            get-ssh-config))

;; Default configuration
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

;; Current loaded configuration
(define current-config default-config)

;; Load configuration from file or use defaults
(define (load-config . args)
  (let ((config-file (if (null? args) 
                         (string-append (getenv "HOME") "/.config/homelab/config.json")
                         (car args))))
    (if (file-exists? config-file)
        (begin
          (log-debug "Loading configuration from ~a" config-file)
          (catch #t
            (lambda ()
              (let ((json-data (call-with-input-file config-file get-string-all)))
                (set! current-config (json-string->scm-safe json-data))
                (log-info "Configuration loaded successfully")))
            (lambda (key . args)
              (log-warn "Failed to load config file, using defaults: ~a" key)
              (set! current-config default-config))))
        (begin
          (log-debug "No config file found, using defaults")
          (set! current-config default-config)))
    current-config))

;; Get a configuration value by path
(define (get-config-value path . default)
  (let ((result (fold (lambda (key acc)
                        (if (and acc (list? acc))
                            (assoc-ref acc key)
                            #f))
                      current-config path)))
    (if result 
        result 
        (if (null? default) #f (car default)))))

;; Get machine configurations
(define (machine-configs)
  (get-config-value '(machines)))

;; Get configuration for a specific machine
(define (get-machine-config machine-name)
  (let ((machine-symbol (if (symbol? machine-name)
                            machine-name
                            (string->symbol machine-name))))
    (assoc-ref (machine-configs) machine-symbol)))

;; Get list of all machine names
(define (get-all-machines)
  (map (lambda (machine-entry)
         (symbol->string (car machine-entry)))
       (machine-configs)))

;; Validate that a machine name exists
(define (validate-machine-name machine-name)
  (let ((machines (get-all-machines)))
    (if (member machine-name machines)
        #t
        (begin
          (log-error "Unknown machine: ~a" machine-name)
          (log-error "Available machines: ~a" (string-join machines ", "))
          #f))))

;; Get home lab root directory
(define (get-homelab-root)
  (get-config-value '(homelab-root) "/home/geir/Home-lab"))

;; Get SSH configuration for a machine
(define (get-ssh-config machine-name)
  (let ((machine-config (get-machine-config machine-name)))
    (if machine-config
        (let ((type (assoc-ref machine-config 'type))
              (hostname (assoc-ref machine-config 'hostname))
              (ssh-alias (assoc-ref machine-config 'ssh-alias)))
          `((type . ,type)
            (hostname . ,hostname)
            (ssh-alias . ,ssh-alias)
            (is-local . ,(eq? type 'local))))
        #f)))

;; Initialize configuration on module load
(load-config)
