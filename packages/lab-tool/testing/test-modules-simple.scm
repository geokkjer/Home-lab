#!/usr/bin/env guile
!#

;; TDD Simple Module Test for Lab Tool
;; Following K.I.S.S principles - test module loading only

(add-to-load-path ".")

(use-modules (ice-9 format))

(define (main)
  (format #t "🧪 LAB TOOL MODULE LOADING TEST\n")
  (format #t "=================================\n\n")

  ;; Test module loading
  (format #t "Testing module loading...\n")

  ;; Test 1: Lab modules
  (format #t "1. Lab core module: ")
  (catch #t
    (lambda () 
      (use-modules (lab core))
      (format #t "✅ LOADED\n"))
    (lambda (key . args) 
      (format #t "❌ FAILED: ~a\n" key)))

  (format #t "2. Lab machines module: ")
  (catch #t
    (lambda () 
      (use-modules (lab machines))
      (format #t "✅ LOADED\n"))
    (lambda (key . args) 
      (format #t "❌ FAILED: ~a\n" key)))

  (format #t "3. Lab deployment module: ")
  (catch #t
    (lambda () 
      (use-modules (lab deployment))
      (format #t "✅ LOADED\n"))
    (lambda (key . args) 
      (format #t "❌ FAILED: ~a\n" key)))

  ;; Test 2: Utils modules
  (format #t "4. Utils logging module: ")
  (catch #t
    (lambda () 
      (use-modules (utils logging))
      (format #t "✅ LOADED\n"))
    (lambda (key . args) 
      (format #t "❌ FAILED: ~a\n" key)))

  (format #t "5. Utils config module: ")
  (catch #t
    (lambda () 
      (use-modules (utils config))
      (format #t "✅ LOADED\n"))
    (lambda (key . args) 
      (format #t "❌ FAILED: ~a\n" key)))

  (format #t "\n🎉 Module loading test complete!\n"))

;; Run the main function
(main)
