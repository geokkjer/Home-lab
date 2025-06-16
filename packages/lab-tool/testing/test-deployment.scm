#!/usr/bin/env guile
!#

;; TDD Test for Deployment Functionality
;; Following K.I.S.S principles - test one thing at a time

(add-to-load-path ".")

(use-modules (ice-9 format)
             (utils logging))

;; Simple test framework if srfi-64 not available
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

;; Test 1: Can we load deployment module without syntax errors?
(simple-test "Load deployment module"
  (lambda ()
    (catch #t
      (lambda () 
        (use-modules (lab deployment))
        #t)
      (lambda (key . args) #f))))

;; Test 2: Can we call option-ref function?
(simple-test "option-ref function exists"
  (lambda ()
    (catch #t
      (lambda ()
        (use-modules (lab deployment))
        (and (defined? 'option-ref)
             (procedure? option-ref)))
      (lambda (key . args) #f))))

;; Test 3: Basic option-ref functionality
(simple-test "option-ref basic functionality"
  (lambda ()
    (catch #t
      (lambda ()
        (use-modules (lab deployment))
        (let ((options '((dry-run . #t) (mode . "test"))))
          (and (equal? (option-ref options 'dry-run #f) #t)
               (equal? (option-ref options 'mode "boot") "test")
               (equal? (option-ref options 'missing "default") "default"))))
      (lambda (key . args) #f))))

(test-summary)
