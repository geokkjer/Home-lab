#!/usr/bin/env guile
!#

;; Comprehensive test script for Home Lab Guile implementation
;; Tests all modules and identifies bugs/missing functionality

(add-to-load-path ".")

(use-modules (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1)
             (srfi srfi-64)) ; Testing framework

;; Global test results
(define test-results '())
(define failed-tests '())

;; Utility functions for testing
(define (test-module-loading module-name)
  "Test if a module can be loaded without errors"
  (format #t "Testing module loading: ~a... " module-name)
  (catch #t
    (lambda ()
      (let ((module-parts (map string->symbol (string-split module-name #\space))))
        (if (resolve-module module-parts #:ensure #f)
            (begin
              (format #t "✅ PASS\n")
              #t)
            (begin
              (format #t "❌ FAIL - Module not found\n")
              (set! failed-tests (cons module-name failed-tests))
              #f))))
    (lambda (key . args)
      (format #t "❌ FAIL - ~a: ~a\n" key args)
      (set! failed-tests (cons module-name failed-tests))
      #f)))

(define (test-function-exists module-name function-name)
  "Test if a function exists in a module"
  (catch #t
    (lambda ()
      (let* ((module-parts (string-split module-name #\space))
             (module (resolve-module module-parts)))
        (if (module-defined? module (string->symbol function-name))
            (begin
              (format #t "  ✅ Function ~a exists\n" function-name)
              #t)
            (begin
              (format #t "  ❌ Function ~a missing\n" function-name)
              (set! failed-tests (cons (format #f "~a::~a" module-name function-name) failed-tests))
              #f))))
    (lambda (key . args)
      (format #t "  ❌ Error checking function ~a: ~a\n" function-name key)
      #f)))

(define (test-basic-functionality)
  "Test basic functionality of each module"
  (format #t "\n=== BASIC FUNCTIONALITY TESTS ===\n")
  
  ;; Test utils modules
  (format #t "\n--- Testing utils/logging ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils logging))
      (log-info "Testing logging functionality")
      (log-debug "Debug message test")
      (log-warn "Warning message test")
      (log-error "Error message test")
      (format #t "✅ Logging module functional\n"))
    (lambda (key . args)
      (format #t "❌ Logging module failed: ~a\n" key)))
  
  ;; Test config module (with JSON dependency fix)
  (format #t "\n--- Testing utils/config ---\n")
  (catch #t
    (lambda ()
      ;; Try to load config module
      (use-modules (utils config))
      (let ((machines (get-all-machines)))
        (format #t "✅ Config loaded, found ~a machines: ~a\n" 
                (length machines) machines))
      (let ((homelab-root (get-homelab-root)))
        (format #t "✅ Home lab root: ~a\n" homelab-root)))
    (lambda (key . args)
      (format #t "❌ Config module failed: ~a\n" key)))
  
  ;; Test SSH module
  (format #t "\n--- Testing utils/ssh ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils ssh))
      (format #t "✅ SSH module loaded\n")
      ;; Test SSH connection to local machine
      (let ((result (test-ssh-connection "congenital-optimist")))
        (format #t "✅ SSH test result for local machine: ~a\n" result)))
    (lambda (key . args)
      (format #t "❌ SSH module failed: ~a\n" key)))
  
  ;; Test core lab modules
  (format #t "\n--- Testing lab/core ---\n")
  (catch #t
    (lambda ()
      (use-modules (lab core))
      (format #t "✅ Lab core module loaded\n")
      ;; Test infrastructure status
      (let ((status (get-infrastructure-status)))
        (format #t "✅ Infrastructure status retrieved for ~a machines\n" 
                (length status))))
    (lambda (key . args)
      (format #t "❌ Lab core module failed: ~a\n" key)))
  
  ;; Test machines module
  (format #t "\n--- Testing lab/machines ---\n")
  (catch #t
    (lambda ()
      (use-modules (lab machines))
      (format #t "✅ Lab machines module loaded\n"))
    (lambda (key . args)
      (format #t "❌ Lab machines module failed: ~a\n" key)))
  
  ;; Test deployment module
  (format #t "\n--- Testing lab/deployment ---\n")
  (catch #t
    (lambda ()
      (use-modules (lab deployment))
      (format #t "✅ Lab deployment module loaded\n"))
    (lambda (key . args)
      (format #t "❌ Lab deployment module failed: ~a\n" key)))
  
  ;; Test monitoring module
  (format #t "\n--- Testing lab/monitoring ---\n")
  (catch #t
    (lambda ()
      (use-modules (lab monitoring))
      (format #t "✅ Lab monitoring module loaded\n"))
    (lambda (key . args)
      (format #t "❌ Lab monitoring module failed: ~a\n" key))))

(define (test-file-syntax file-path)
  "Test if a Scheme file has valid syntax"
  (catch #t
    (lambda ()
      (call-with-input-file file-path
        (lambda (port)
          (let loop ((expr (read port)))
            (unless (eof-object? expr)
              (loop (read port))))))
      (format #t "✅ ~a - syntax OK\n" file-path)
      #t)
    (lambda (key . args)
      (format #t "❌ ~a - syntax error: ~a\n" file-path key)
      (set! failed-tests (cons file-path failed-tests))
      #f)))

(define (find-scheme-files dir)
  "Find all .scm files in directory"
  (let ((files '()))
    (file-system-fold
     (lambda (file-name stat result) ; enter?
       #t)
     (lambda (file-name stat result) ; leaf
       (if (string-suffix? ".scm" file-name)
           (cons file-name result)
           result))
     (lambda (file-name stat result) ; down
       result)
     (lambda (file-name stat result) ; up  
       result)
     (lambda (file-name stat result) ; skip
       result)
     (lambda (file-name stat errno result) ; error
       (format #t "Error accessing ~a: ~a\n" file-name errno)
       result)
     files
     dir)
    files))

;; Main test execution
(define (main)
  (format #t "🧪 HOME LAB GUILE IMPLEMENTATION TEST SUITE\n")
  (format #t "==========================================\n")
  
  ;; Test 1: Syntax checking
  (format #t "\n=== SYNTAX CHECKING ===\n")
  (let ((scheme-files (find-scheme-files ".")))
    (for-each test-file-syntax scheme-files))
  
  ;; Test 2: Module loading
  (format #t "\n=== MODULE LOADING TESTS ===\n")
  (let ((modules '("utils logging" 
                   "utils config" 
                   "utils ssh"
                   "utils json"
                   "lab core"
                   "lab machines" 
                   "lab deployment"
                   "lab monitoring"
                   "mcp server")))
    (for-each test-module-loading modules))
  
  ;; Test 3: Basic functionality
  (test-basic-functionality)
  
  ;; Test 4: Integration tests
  (format #t "\n=== INTEGRATION TESTS ===\n")
  (catch #t
    (lambda ()
      (use-modules (utils config) (utils ssh) (lab core))
      (format #t "Testing machine configuration access...\n")
      (let ((machines (get-all-machines)))
        (for-each (lambda (machine)
                    (format #t "  - Testing ~a: " machine)
                    (let ((config (get-machine-config machine)))
                      (if config
                          (format #t "✅ Config found\n")
                          (format #t "❌ No config\n"))))
                  machines)))
    (lambda (key . args)
      (format #t "❌ Integration test failed: ~a\n" key)))
  
  ;; Test 5: Command line interface
  (format #t "\n=== CLI INTERFACE TESTS ===\n")
  (catch #t
    (lambda ()
      (load "home-lab-tool.scm")
      (format #t "✅ CLI script loaded successfully\n"))
    (lambda (key . args)
      (format #t "❌ CLI script failed: ~a\n" key)))
  
  ;; Summary
  (format #t "\n=== TEST SUMMARY ===\n")
  (if (null? failed-tests)
      (format #t "🎉 All tests passed!\n")
      (begin
        (format #t "❌ ~a test(s) failed:\n" (length failed-tests))
        (for-each (lambda (test)
                    (format #t "  - ~a\n" test))
                  failed-tests)))
  
  (format #t "\nTest complete.\n"))

;; Run the tests
(main)
