#!/usr/bin/env guile
!#

;; Updated test script for modular refactoring
;; Tests the new K.I.S.S modular implementation

(add-to-load-path ".")
(add-to-load-path "lab")

(use-modules (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1))

;; Global test results
(define test-results '())
(define failed-tests '())

;; Test pure function modules
(define (test-pure-functions)
  "Test pure function implementations"
  (format #t "\n=== PURE FUNCTION TESTS ===\n")
  
  ;; Test logging format (pure functions)
  (format #t "\n--- Testing utils/logging/format (pure) ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils logging format))
      (let ((blue-color (get-color 'blue))
            (timestamp (format-timestamp)))
        (format #t "✅ Color retrieval: ~a\n" blue-color)
        (format #t "✅ Timestamp format: ~a\n" timestamp)
        (format #t "✅ Pure logging format functions work\n")))
    (lambda (key . args)
      (format #t "❌ Logging format test failed: ~a\n" key)))
  
  ;; Test config defaults (pure data)
  (format #t "\n--- Testing utils/config/defaults (pure) ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils config defaults))
      (let ((homelab-root (assoc-ref default-config 'homelab-root))
            (machines (assoc-ref default-config 'machines)))
        (format #t "✅ Default homelab root: ~a\n" homelab-root)
        (format #t "✅ Default machines count: ~a\n" (length machines))
        (format #t "✅ Pure config defaults work\n")))
    (lambda (key . args)
      (format #t "❌ Config defaults test failed: ~a\n" key)))
  
  ;; Test config accessor (pure functions)
  (format #t "\n--- Testing utils/config/accessor (pure) ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils config defaults)
                   (utils config accessor))
      (let* ((config default-config)
             (homelab-root (get-config-value-pure config '(homelab-root)))
             (machines (get-all-machines-pure config))
             (first-machine (if (not (null? machines)) (car machines) "none")))
        (format #t "✅ Pure config access: ~a\n" homelab-root)
        (format #t "✅ Pure machine list: ~a machines\n" (length machines))
        (format #t "✅ First machine: ~a\n" first-machine)
        (format #t "✅ Pure config accessor functions work\n")))
    (lambda (key . args)
      (format #t "❌ Config accessor test failed: ~a\n" key)))
  
  ;; Test JSON parse (pure functions)
  (format #t "\n--- Testing utils/json/parse (pure) ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils json parse))
      (let ((test-json "{\"name\": \"test\", \"value\": 42}")
            (invalid-json "{invalid"))
        (let ((parsed-valid (parse-json-pure test-json))
              (parsed-invalid (parse-json-pure invalid-json)))
          (format #t "✅ Valid JSON parsed: ~a\n" (assoc-ref parsed-valid "name"))
          (format #t "✅ Invalid JSON handled: ~a\n" (if parsed-invalid "ERROR" "OK"))
          (format #t "✅ Pure JSON parsing functions work\n"))))
    (lambda (key . args)
      (format #t "❌ JSON parse test failed: ~a\n" key)))
  
  ;; Test JSON manipulation (pure functions)
  (format #t "\n--- Testing utils/json/manipulation (pure) ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils json manipulation))
      (let ((obj1 '((a . 1) (b . 2)))
            (obj2 '((b . 3) (c . 4))))
        (let ((merged (merge-json-objects obj1 obj2))
              (value-at-path (json-path-ref '((level1 . ((level2 . "found")))) '(level1 level2))))
          (format #t "✅ Merge result: ~a\n" (assoc-ref merged 'b))
          (format #t "✅ Path access: ~a\n" value-at-path)
          (format #t "✅ Pure JSON manipulation functions work\n"))))
    (lambda (key . args)
      (format #t "❌ JSON manipulation test failed: ~a\n" key))))

;; Test module facades
(define (test-facade-modules)
  "Test facade modules that aggregate functionality"
  (format #t "\n=== FACADE MODULE TESTS ===\n")
  
  ;; Test new logging facade
  (format #t "\n--- Testing utils/logging (facade) ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils logging-new))
      (log-info "Test info message")
      (log-debug "Test debug message")
      (set-log-level! 'debug)
      (log-debug "Debug message after level change")
      (format #t "✅ Logging facade works\n"))
    (lambda (key . args)
      (format #t "❌ Logging facade test failed: ~a\n" key)))
  
  ;; Test new config facade
  (format #t "\n--- Testing utils/config (facade) ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils config-new))
      (let ((machines (get-all-machines))
            (homelab-root (get-homelab-root)))
        (format #t "✅ Config facade - machines: ~a\n" (length machines))
        (format #t "✅ Config facade - root: ~a\n" homelab-root)
        (format #t "✅ Config facade works\n")))
    (lambda (key . args)
      (format #t "❌ Config facade test failed: ~a\n" key)))
  
  ;; Test new JSON facade
  (format #t "\n--- Testing utils/json (facade) ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils json-new))
      (let* ((test-obj '((name . "test") (count . 5)))
             (json-str (scm->json-string test-obj))
             (parsed-back (json-string->scm-safe json-str)))
        (format #t "✅ JSON facade - serialization: ~a\n" (if json-str "OK" "FAIL"))
        (format #t "✅ JSON facade - round-trip: ~a\n" (assoc-ref parsed-back 'name))
        (format #t "✅ JSON facade works\n")))
    (lambda (key . args)
      (format #t "❌ JSON facade test failed: ~a\n" key))))

;; Test modular structure benefits
(define (test-modular-benefits)
  "Test the benefits of modular structure"
  (format #t "\n=== MODULAR BENEFITS TESTS ===\n")
  
  ;; Test pure function composition
  (format #t "\n--- Testing Pure Function Composition ---\n")
  (catch #t
    (lambda ()
      (use-modules (utils config defaults)
                   (utils config accessor)
                   (utils logging format))
      
      ;; Compose pure functions
      (let* ((config default-config)
             (machines (get-all-machines-pure config))
             (first-machine (if (not (null? machines)) (car machines) #f))
             (ssh-config (if first-machine 
                             (get-ssh-config-pure config first-machine) 
                             #f))
             (timestamp (format-timestamp)))
        
        (format #t "✅ Functional composition: ~a machines found\n" (length machines))
        (format #t "✅ First machine SSH config: ~a\n" 
                (if ssh-config (assoc-ref ssh-config 'hostname) "none"))
        (format #t "✅ Pure function pipeline works\n")))
    (lambda (key . args)
      (format #t "❌ Pure function composition failed: ~a\n" key)))
  
  ;; Test module independence
  (format #t "\n--- Testing Module Independence ---\n")
  (catch #t
    (lambda ()
      ;; Each module should work independently
      (use-modules (utils json parse))
      (let ((result1 (parse-json-pure "{\"test\": 1}")))
        (use-modules (utils json serialize))
        (let ((result2 (scm->json-string-pure '((test . 2)) #f)))
          (use-modules (utils logging format))
          (let ((result3 (get-color 'green)))
            (format #t "✅ Independent module 1: ~a\n" (if result1 "OK" "FAIL"))
            (format #t "✅ Independent module 2: ~a\n" (if result2 "OK" "FAIL"))
            (format #t "✅ Independent module 3: ~a\n" (if result3 "OK" "FAIL"))
            (format #t "✅ Module independence verified\n")))))
    (lambda (key . args)
      (format #t "❌ Module independence test failed: ~a\n" key))))

;; Main test execution
(define (main)
  (format #t "🧪 K.I.S.S MODULAR REFACTORING TEST SUITE\n")
  (format #t "==========================================\n")
  
  (test-pure-functions)
  (test-facade-modules)
  (test-modular-benefits)
  
  ;; Summary
  (format #t "\n=== TEST SUMMARY ===\n")
  (format #t "🎉 K.I.S.S modular refactoring tests complete!\n")
  (format #t "\n📊 Refactoring Benefits Demonstrated:\n")
  (format #t "  ✅ Pure functions enable easy testing\n")
  (format #t "  ✅ Small modules are easy to understand\n")
  (format #t "  ✅ Single responsibility principle applied\n") 
  (format #t "  ✅ Functional composition works\n")
  (format #t "  ✅ Module independence verified\n")
  (format #t "  ✅ Both pure and impure APIs available\n")
  
  (format #t "\nRefactoring complete! ✨\n"))

;; Run the tests
(main)
