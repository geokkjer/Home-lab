#!/usr/bin/env guile
!#

;; TDD Integration Test for Lab Tool
;; Following K.I.S.S principles - test complete functionality

(add-to-load-path ".")

(use-modules (ice-9 format)
             (utils logging))

(format #t "🧪 LAB TOOL INTEGRATION TEST\n")
(format #t "=============================\n\n")

;; Simple test framework
(define test-count 0)
(define passed-count 0)

(define (simple-test name thunk)
  "Simple test runner"
  (set! test-count (+ test-count 1))
  (format #t "Test ~a: ~a..." test-count name)
  (let ((result (catch #t thunk
                       (lambda (key . args) #f))))
    (if result
        (begin
          (set! passed-count (+ passed-count 1))
          (format #t " ✅ PASS\n"))
        (format #t " ❌ FAIL\n"))))

(define (test-summary)
  "Print test summary"
  (format #t "\n=== Test Summary ===\n")
  (format #t "Passed: ~a/~a\n" passed-count test-count)
  (if (= passed-count test-count)
      (format #t "🎉 All tests passed!\n")
      (format #t "❌ Some tests failed\n")))

;; Core functionality tests
(simple-test "Help command works"
  (lambda () (= 0 (system "./main.scm help >/dev/null 2>&1"))))

(simple-test "Status command works"
  (lambda () (= 0 (system "./main.scm status >/dev/null 2>&1"))))

(simple-test "Machines command works"
  (lambda () (= 0 (system "./main.scm machines >/dev/null 2>&1"))))

(simple-test "Test-modules command works"
  (lambda () (= 0 (system "./main.scm test-modules >/dev/null 2>&1"))))

(simple-test "Invalid command returns error"
  (lambda () (not (= 0 (system "./main.scm invalid >/dev/null 2>&1")))))

;; Module loading tests
(simple-test "Lab core module loads"
  (lambda ()
    (catch #t
      (lambda () (use-modules (lab core)) #t)
      (lambda (key . args) #f))))

(simple-test "Lab machines module loads"
  (lambda ()
    (catch #t
      (lambda () (use-modules (lab machines)) #t)
      (lambda (key . args) #f))))

(simple-test "Lab deployment module loads"
  (lambda ()
    (catch #t
      (lambda () (use-modules (lab deployment)) #t)
      (lambda (key . args) #f))))

;; Utility module tests
(simple-test "Utils logging module loads"
  (lambda ()
    (catch #t
      (lambda () (use-modules (utils logging)) #t)
      (lambda (key . args) #f))))

(simple-test "Utils config module loads"
  (lambda ()
    (catch #t
      (lambda () (use-modules (utils config)) #t)
      (lambda (key . args) #f))))

(simple-test "Utils ssh module loads"
  (lambda ()
    (catch #t
      (lambda () (use-modules (utils ssh)) #t)
      (lambda (key . args) #f))))

;; Function availability tests
(simple-test "Basic deployment functions available"
  (lambda ()
    (catch #t
      (lambda ()
        (use-modules (lab deployment))
        (and (defined? 'deploy-machine)
             (defined? 'update-flake)
             (defined? 'option-ref)))
      (lambda (key . args) #f))))

(simple-test "Basic machine functions available"
  (lambda ()
    (catch #t
      (lambda ()
        (use-modules (lab machines))
        (and (defined? 'list-machines)
             (defined? 'validate-machine-name)))
      (lambda (key . args) #f))))

(simple-test "Basic core functions available"
  (lambda ()
    (catch #t
      (lambda ()
        (use-modules (lab core))
        (and (defined? 'get-infrastructure-status)))
      (lambda (key . args) #f))))

(test-summary)
