;; utils/json/serialize.scm - Pure JSON serialization functions

(define-module (utils json serialize)
  #:use-module (json)
  #:use-module (ice-9 textual-ports)
  #:export (scm->json-string-pure
            scm->json-string))

;; Pure function: Convert scheme object to JSON string
;; Input: obj (scheme object), pretty (boolean)
;; Output: JSON string or #f if conversion fails
(define (scm->json-string-pure obj pretty)
  "Pure function to convert scheme object to JSON string"
  (catch #t
    (lambda ()
      (call-with-output-string
        (lambda (port)
          (if pretty
              (scm->json obj port #:pretty #t)
              (scm->json obj port)))))
    (lambda (key . args) #f)))

;; Wrapper with optional pretty parameter
(define (scm->json-string obj . options)
  "Convert scheme object to JSON string with optional pretty printing"
  (let ((pretty (if (null? options) #f (car options))))
    (scm->json-string-pure obj pretty)))
