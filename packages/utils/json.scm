;; utils/json.scm - JSON processing utilities for Home Lab Tool

(define-module (utils json)
  #:use-module (json)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (utils logging)
  #:export (read-json-file
            write-json-file
            json-pretty-print
            scm->json-string
            json-string->scm-safe
            validate-json-schema
            merge-json-objects))

;; Read JSON from file with error handling
(define (read-json-file filename)
  (catch #t
    (lambda ()
      (log-debug "Reading JSON file: ~a" filename)
      (call-with-input-file filename
        (lambda (port)
          (json->scm port))))
    (lambda (key . args)
      (log-error "Failed to read JSON file ~a: ~a ~a" filename key args)
      #f)))

;; Write Scheme object to JSON file
(define (write-json-file filename obj . options)
  (let ((pretty (if (null? options) #t (car options))))
    (catch #t
      (lambda ()
        (log-debug "Writing JSON file: ~a" filename)
        (call-with-output-file filename
          (lambda (port)
            (if pretty
                (scm->json obj port #:pretty #t)
                (scm->json obj port))))
        #t)
      (lambda (key . args)
        (log-error "Failed to write JSON file ~a: ~a ~a" filename key args)
        #f))))

;; Pretty print JSON to current output port
(define (json-pretty-print obj)
  (scm->json obj (current-output-port) #:pretty #t)
  (newline))

;; Convert Scheme object to JSON string
(define (scm->json-string obj . options)
  (let ((pretty (if (null? options) #f (car options))))
    (catch #t
      (lambda ()
        (call-with-output-string
          (lambda (port)
            (if pretty
                (scm->json obj port #:pretty #t)
                (scm->json obj port)))))
      (lambda (key . args)
        (log-error "Failed to convert to JSON: ~a ~a" key args)
        #f))))

;; Safely convert JSON string to Scheme with error handling
(define (json-string->scm-safe json-str)
  (catch #t
    (lambda ()
      (json-string->scm json-str))
    (lambda (key . args)
      (log-error "Failed to parse JSON string: ~a ~a" key args)
      #f)))

;; Basic JSON schema validation
(define (validate-json-schema obj schema)
  "Validate JSON object against a simple schema.
   Schema format: ((required-keys ...) (optional-keys ...) (types ...))"
  (let ((required-keys (car schema))
        (optional-keys (if (> (length schema) 1) (cadr schema) '()))
        (type-specs (if (> (length schema) 2) (caddr schema) '())))
    
    ;; Check required keys
    (let ((missing-keys (filter (lambda (key)
                                  (not (assoc-ref obj key)))
                                required-keys)))
      (if (not (null? missing-keys))
          (begin
            (log-error "Missing required keys: ~a" missing-keys)
            #f)
          (begin
            ;; Check types if specified
            (let ((type-errors (filter-map
                               (lambda (type-spec)
                                 (let ((key (car type-spec))
                                       (expected-type (cadr type-spec)))
                                   (let ((value (assoc-ref obj key)))
                                     (if (and value (not (eq? (type-of value) expected-type)))
                                         (format #f "Key ~a: expected ~a, got ~a" 
                                                key expected-type (type-of value))
                                         #f))))
                               type-specs)))
              (if (not (null? type-errors))
                  (begin
                    (log-error "Type validation errors: ~a" type-errors)
                    #f)
                  #t)))))))

;; Merge two JSON objects (association lists)
(define (merge-json-objects obj1 obj2)
  "Merge two JSON objects, with obj2 values taking precedence"
  (let ((merged (copy-tree obj1)))
    (for-each (lambda (pair)
                (let ((key (car pair))
                      (value (cdr pair)))
                  (set! merged (assoc-set! merged key value))))
              obj2)
    merged))

;; Convert nested alist to flat key paths for easier access
(define (flatten-json-paths obj . prefix)
  "Convert nested object to flat list of (path . value) pairs"
  (let ((current-prefix (if (null? prefix) '() (car prefix))))
    (fold (lambda (pair acc)
            (let ((key (car pair))
                  (value (cdr pair)))
              (let ((new-path (append current-prefix (list key))))
                (if (and (list? value) (not (null? value)) (pair? (car value)))
                    ;; Nested object - recurse
                    (append (flatten-json-paths value new-path) acc)
                    ;; Leaf value
                    (cons (cons new-path value) acc)))))
          '()
          obj)))

;; Get nested value using path list
(define (json-path-ref obj path)
  "Get value from nested object using list of keys as path"
  (fold (lambda (key acc)
          (if (and acc (list? acc))
              (assoc-ref acc key)
              #f))
        obj path))
