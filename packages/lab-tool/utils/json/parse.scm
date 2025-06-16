;; utils/json/parse.scm - Pure JSON parsing functions

(define-module (utils json parse)
  #:use-module (json)
  #:export (json-string->scm-safe
            parse-json-pure))

;; Pure function: Safely parse JSON string
;; Input: json-string
;; Output: parsed scheme object or #f if invalid
(define (parse-json-pure json-string)
  "Pure function to parse JSON string without side effects"
  (catch #t
    (lambda () 
      (if (string? json-string)
          (json-string->scm json-string)
          #f))
    (lambda (key . args) #f)))

;; Alias for compatibility
(define json-string->scm-safe parse-json-pure)
