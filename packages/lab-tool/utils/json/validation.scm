;; utils/json/validation.scm - Pure JSON validation functions

(define-module (utils json validation)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (validate-required-keys
            validate-types
            validate-json-schema))

;; Pure function: Check for required keys
;; Input: obj (alist), required-keys (list of symbols)
;; Output: list of missing keys (empty if all present)
(define (get-missing-keys obj required-keys)
  "Pure function to find missing required keys"
  (filter (lambda (key)
            (not (assoc-ref obj key)))
          required-keys))

;; Pure function: Validate required keys
;; Input: obj (alist), required-keys (list of symbols)
;; Output: #t if all present, #f otherwise
(define (validate-required-keys obj required-keys)
  "Pure function to validate required keys are present"
  (null? (get-missing-keys obj required-keys)))

;; Pure function: Check type specifications
;; Input: obj (alist), type-specs (list of (key expected-type) pairs)
;; Output: list of type error messages (empty if all valid)
(define (get-type-errors obj type-specs)
  "Pure function to find type validation errors"
  (filter-map
   (lambda (type-spec)
     (let ((key (car type-spec))
           (expected-type (cadr type-spec)))
       (let ((value (assoc-ref obj key)))
         (if (and value (not (eq? (type-of value) expected-type)))
             (format #f "Key ~a: expected ~a, got ~a" 
                     key expected-type (type-of value))
             #f))))
   type-specs))

;; Pure function: Validate types
;; Input: obj (alist), type-specs (list of (key expected-type) pairs)
;; Output: #t if all types valid, #f otherwise
(define (validate-types obj type-specs)
  "Pure function to validate object types"
  (null? (get-type-errors obj type-specs)))

;; Pure function: Complete schema validation
;; Input: obj (alist), schema (list with required-keys, optional-keys, types)
;; Output: (values valid? error-messages)
(define (validate-json-schema obj schema)
  "Pure function to validate JSON object against schema"
  (let ((required-keys (car schema))
        (optional-keys (if (> (length schema) 1) (cadr schema) '()))
        (type-specs (if (> (length schema) 2) (caddr schema) '())))
    
    (let ((missing-keys (get-missing-keys obj required-keys))
          (type-errors (get-type-errors obj type-specs)))
      
      (if (or (not (null? missing-keys)) (not (null? type-errors)))
          (values #f (append 
                      (if (not (null? missing-keys))
                          (list (format #f "Missing required keys: ~a" missing-keys))
                          '())
                      type-errors))
          (values #t '())))))
