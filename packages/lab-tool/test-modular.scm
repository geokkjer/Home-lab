#!/usr/bin/env guile
!#

;; Test script for modular refactoring

(add-to-load-path "lab")

(use-modules (ice-9 format))

;; Test logging format module
(display "Testing logging format module...\n")
(catch #t
  (lambda ()
    (use-modules (utils logging format))
    (display "✅ Logging format module loaded\n")
    (let ((blue-color (get-color 'blue)))
      (format #t "Blue color code: ~a\n" blue-color)))
  (lambda (key . args)
    (format #t "❌ Failed to load logging format: ~a ~a\n" key args)))

;; Test config defaults module
(display "\nTesting config defaults module...\n")
(catch #t
  (lambda ()
    (use-modules (utils config defaults))
    (display "✅ Config defaults module loaded\n")
    (let ((config default-config))
      (format #t "Default homelab root: ~a\n" (assoc-ref config 'homelab-root))))
  (lambda (key . args)
    (format #t "❌ Failed to load config defaults: ~a ~a\n" key args)))

;; Test JSON parse module
(display "\nTesting JSON parse module...\n")
(catch #t
  (lambda ()
    (use-modules (utils json parse))
    (display "✅ JSON parse module loaded\n")
    (let ((result (parse-json-pure "{\"test\": true}")))
      (format #t "JSON parse test: ~a\n" result)))
  (lambda (key . args)
    (format #t "❌ Failed to load JSON parse: ~a ~a\n" key args)))

(display "\n🎉 Modular refactoring test complete!\n")
