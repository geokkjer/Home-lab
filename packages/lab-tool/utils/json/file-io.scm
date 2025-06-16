;; utils/json/file-io.scm - JSON file I/O operations

(define-module (utils json file-io)
  #:use-module (json)
  #:use-module (ice-9 textual-ports)
  #:use-module (utils logging)
  #:export (read-json-file-pure
            write-json-file-pure
            read-json-file
            write-json-file))

;; Pure function: Read JSON from file without logging
;; Input: filename string
;; Output: parsed object or #f if failed
(define (read-json-file-pure filename)
  "Pure function to read JSON from file"
  (catch #t
    (lambda ()
      (call-with-input-file filename
        (lambda (port) (json->scm port))))
    (lambda (key . args) #f)))

;; Pure function: Write JSON to file without logging
;; Input: filename string, obj (scheme object), pretty boolean
;; Output: #t if successful, #f if failed
(define (write-json-file-pure filename obj pretty)
  "Pure function to write JSON to file"
  (catch #t
    (lambda ()
      (call-with-output-file filename
        (lambda (port)
          (if pretty
              (scm->json obj port #:pretty #t)
              (scm->json obj port))))
      #t)
    (lambda (key . args) #f)))

;; Impure wrapper: Read JSON file with logging
(define (read-json-file filename)
  "Read JSON from file with logging"
  (log-debug "Reading JSON file: ~a" filename)
  (let ((result (read-json-file-pure filename)))
    (if result
        (log-debug "Successfully read JSON file: ~a" filename)
        (log-error "Failed to read JSON file: ~a" filename))
    result))

;; Impure wrapper: Write JSON file with logging
(define (write-json-file filename obj . options)
  "Write JSON to file with logging"
  (let ((pretty (if (null? options) #t (car options))))
    (log-debug "Writing JSON file: ~a" filename)
    (let ((result (write-json-file-pure filename obj pretty)))
      (if result
          (log-debug "Successfully wrote JSON file: ~a" filename)
          (log-error "Failed to write JSON file: ~a" filename))
      result)))
