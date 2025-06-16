;; utils/config/loader.scm - Configuration loading (file I/O operations)

(define-module (utils config loader)
  #:use-module (ice-9 textual-ports)
  #:use-module (utils logging)
  #:use-module (utils json)
  #:use-module (utils config defaults)
  #:export (load-config-from-file
            load-config))

;; Pure function: Parse configuration from JSON string
;; Input: json-string
;; Output: parsed configuration alist or #f if invalid
(define (parse-config-json json-string)
  "Pure function to parse configuration from JSON string"
  (catch #t
    (lambda () (json-string->scm-safe json-string))
    (lambda (key . args) #f)))

;; Pure function: Validate configuration structure
;; Input: config alist
;; Output: #t if valid, #f otherwise
(define (validate-config config)
  "Pure function to validate configuration structure"
  (and (list? config)
       (assoc-ref config 'homelab-root)
       (assoc-ref config 'machines)))

;; Impure function: Load configuration from file
;; Input: file-path string
;; Output: configuration alist or default-config if file doesn't exist/invalid
(define (load-config-from-file file-path)
  "Load configuration from file (with side effects: file I/O, logging)"
  (if (file-exists? file-path)
      (catch #t
        (lambda ()
          (log-debug "Loading configuration from ~a" file-path)
          (let* ((json-data (call-with-input-file file-path get-string-all))
                 (parsed-config (parse-config-json json-data)))
            (if (and parsed-config (validate-config parsed-config))
                (begin
                  (log-info "Configuration loaded successfully")
                  parsed-config)
                (begin
                  (log-warn "Invalid configuration file, using defaults")
                  default-config))))
        (lambda (key . args)
          (log-warn "Failed to load config file, using defaults: ~a" key)
          default-config))
      (begin
        (log-debug "No config file found at ~a, using defaults" file-path)
        default-config)))

;; Impure function: Load configuration with default path
(define (load-config . args)
  "Load configuration with optional file path"
  (let ((config-file (if (null? args) 
                         (string-append (getenv "HOME") "/.config/homelab/config.json")
                         (car args))))
    (load-config-from-file config-file)))
