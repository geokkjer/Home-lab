;; utils/json.scm - JSON utilities facade

(define-module (utils json)
  #:use-module (utils json parse)
  #:use-module (utils json serialize)
  #:use-module (utils json file-io)
  #:use-module (utils json validation)
  #:use-module (utils json manipulation)
  #:use-module (utils json pretty-print)
  #:re-export (;; Parsing
               parse-json-pure
               json-string->scm-safe
               
               ;; Serialization
               scm->json-string-pure
               scm->json-string
               
               ;; File I/O (both pure and impure versions)
               read-json-file-pure
               write-json-file-pure
               read-json-file
               write-json-file
               
               ;; Validation (pure functions)
               validate-required-keys
               validate-types
               validate-json-schema
               
               ;; Manipulation (pure functions)
               merge-json-objects
               flatten-json-paths
               json-path-ref
               json-path-set
               
               ;; Pretty printing
               json-pretty-print))

;; This module acts as a facade for JSON functionality,
;; aggregating specialized modules that follow single responsibility:
;; - parse: Pure JSON string parsing
;; - serialize: Pure scheme-to-JSON conversion
;; - file-io: File reading/writing with pure and impure versions
;; - validation: Pure schema validation functions
;; - manipulation: Pure object manipulation functions
;; - pretty-print: Output formatting
;;
;; All functions are designed to be composable and testable,
;; with pure versions available for functional programming patterns.
