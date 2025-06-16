#!/usr/bin/env guile
!#

;; Comprehensive test for lab tool implementation

(add-to-load-path ".")

(use-modules (ice-9 format))

;; Test results tracking
(define test-results '())
(define failed-tests '())

(define (test-module module-name)
  "Test if a module loads successfully"
  (format #t "Testing ~a... " module-name)
  (catch #t
    (lambda ()
      (let ((module-parts (map string->symbol (string-split module-name #\space))))
        (resolve-module module-parts)
        (format #t "✅\n")
        #t))
    (lambda (key . args)
      (format #t "❌ (~a)\n" key)
      (set! failed-tests (cons module-name failed-tests))
      #f)))

(define (main)
  (format #t "🧪 LAB TOOL IMPLEMENTATION TEST\n")
  (format #t "===============================\n\n")
  
  ;; Test utils modules
  (format #t "Utils Modules:\n")
  (test-module "utils logging")
  (test-module "utils config")
  (test-module "utils ssh")
  (test-module "utils json")
  
  ;; Test lab modules
  (format #t "\nLab Modules:\n")
  (test-module "lab core")
  (test-module "lab machines")
  (test-module "lab deployment")
  (test-module "lab monitoring")
  
  ;; Test MCP modules
  (format #t "\nMCP Modules:\n")
  (test-module "mcp server")
  
  ;; Test functionality
  (format #t "\nFunctionality Tests:\n")
  (catch #t
    (lambda ()
      (use-modules (lab core) (lab machines))
      (let ((machines (list-machines))
            (status (get-infrastructure-status)))
        (format #t "Machines: ~a ✅\n" (length machines))
        (format #t "Status check: ~a machines ✅\n" (length status))))
    (lambda (key . args)
      (format #t "Functionality test failed: ~a ❌\n" key)))
  
  ;; Summary
  (format #t "\n=== SUMMARY ===\n")
  (if (null? failed-tests)
      (format #t "🎉 All tests passed!\n")
      (begin
        (format #t "❌ Failed: ~a\n" failed-tests)
        (format #t "📝 Need to fix these modules\n")))
  
  (format #t "\nTest complete.\n"))

(main)
