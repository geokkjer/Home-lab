;; utils/json/pretty-print.scm - JSON pretty printing

(define-module (utils json pretty-print)
  #:use-module (json)
  #:export (json-pretty-print))

;; Impure function: Pretty print JSON to current output port
;; Input: obj (scheme object)
;; Output: unspecified (side effect: prints to current-output-port)
(define (json-pretty-print obj)
  "Pretty print JSON object to current output port"
  (scm->json obj (current-output-port) #:pretty #t)
  (newline))
