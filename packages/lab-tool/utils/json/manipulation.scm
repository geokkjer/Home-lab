;; utils/json/manipulation.scm - Pure JSON manipulation functions

(define-module (utils json manipulation)
  #:use-module (srfi srfi-1)
  #:export (merge-json-objects
            flatten-json-paths
            json-path-ref
            json-path-set))

;; Pure function: Merge two JSON objects
;; Input: obj1 (alist), obj2 (alist)
;; Output: merged alist with obj2 values taking precedence
(define (merge-json-objects obj1 obj2)
  "Pure function to merge two JSON objects"
  (let ((merged (copy-tree obj1)))
    (fold (lambda (pair acc)
            (let ((key (car pair))
                  (value (cdr pair)))
              (assoc-set! acc key value)))
          merged
          obj2)))

;; Pure function: Convert nested alist to flat key paths
;; Input: obj (nested alist), optional prefix (list of keys)
;; Output: list of (path . value) pairs
(define (flatten-json-paths obj . prefix)
  "Pure function to flatten nested object to path-value pairs"
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

;; Pure function: Get nested value using path
;; Input: obj (nested alist), path (list of keys)
;; Output: value at path or #f if not found
(define (json-path-ref obj path)
  "Pure function to get value from nested object using key path"
  (fold (lambda (key acc)
          (if (and acc (list? acc))
              (assoc-ref acc key)
              #f))
        obj path))

;; Pure function: Set nested value using path
;; Input: obj (nested alist), path (list of keys), value
;; Output: new alist with value set at path
(define (json-path-set obj path value)
  "Pure function to set value in nested object using key path"
  (if (null? path)
      value
      (let* ((key (car path))
             (rest-path (cdr path))
             (current-value (assoc-ref obj key))
             (new-value (json-path-set (or current-value '()) rest-path value)))
        (assoc-set! (copy-tree obj) key new-value))))
