;; utils/config/accessor.scm - Configuration value access (pure functions)

(define-module (utils config accessor)
  #:use-module (srfi srfi-1)
  #:export (get-config-value-pure
            get-machine-config-pure
            get-all-machines-pure
            get-ssh-config-pure
            validate-machine-name-pure))

;; Pure function: Get configuration value by path
;; Input: config alist, path list, optional default value
;; Output: configuration value or default
(define (get-config-value-pure config path . default)
  "Pure function to get configuration value by path"
  (let ((result (fold (lambda (key acc)
                        (if (and acc (list? acc))
                            (assoc-ref acc key)
                            #f))
                      config path)))
    (if result 
        result 
        (if (null? default) #f (car default)))))

;; Pure function: Get machine configurations
;; Input: config alist
;; Output: machines alist
(define (get-machine-configs-pure config)
  "Pure function to get machine configurations"
  (get-config-value-pure config '(machines)))

;; Pure function: Get configuration for specific machine
;; Input: config alist, machine-name (string or symbol)
;; Output: machine configuration alist or #f
(define (get-machine-config-pure config machine-name)
  "Pure function to get machine configuration"
  (let ((machine-symbol (if (symbol? machine-name)
                            machine-name
                            (string->symbol machine-name)))
        (machines (get-machine-configs-pure config)))
    (assoc-ref machines machine-symbol)))

;; Pure function: Get list of all machine names
;; Input: config alist
;; Output: list of machine name strings
(define (get-all-machines-pure config)
  "Pure function to get all machine names"
  (map (lambda (machine-entry)
         (symbol->string (car machine-entry)))
       (get-machine-configs-pure config)))

;; Pure function: Validate machine name exists
;; Input: config alist, machine-name string
;; Output: #t if valid, #f otherwise
(define (validate-machine-name-pure config machine-name)
  "Pure function to validate machine name"
  (let ((machines (get-all-machines-pure config)))
    (member machine-name machines)))

;; Pure function: Get SSH configuration for machine
;; Input: config alist, machine-name (string or symbol)
;; Output: SSH configuration alist or #f
(define (get-ssh-config-pure config machine-name)
  "Pure function to get SSH configuration for machine"
  (let ((machine-config (get-machine-config-pure config machine-name)))
    (if machine-config
        (let ((type (assoc-ref machine-config 'type))
              (hostname (assoc-ref machine-config 'hostname))
              (ssh-alias (assoc-ref machine-config 'ssh-alias)))
          `((type . ,type)
            (hostname . ,hostname)
            (ssh-alias . ,ssh-alias)
            (is-local . ,(eq? type 'local))))
        #f)))
