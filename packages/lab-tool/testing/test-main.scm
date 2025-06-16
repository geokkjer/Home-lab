#!/usr/bin/env guile
!#

;; TDD Test for Main.scm - Command functionality
;; Following K.I.S.S principles - test one thing at a time

(add-to-load-path ".")

(use-modules (ice-9 format)
             (utils logging))

(format #t "🧪 MAIN.SCM FUNCTIONALITY TEST\n")
(format #t "==============================\n\n")

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

;; Test 1: Can we run main.scm help command?
(simple-test "main.scm help command"
  (lambda ()
    (= 0 (system "./main.scm help >/dev/null 2>&1"))))

;; Test 2: Can we run main.scm status command?
(simple-test "main.scm status command"
  (lambda ()
    (= 0 (system "./main.scm status >/dev/null 2>&1"))))

;; Test 3: Can we run main.scm machines command?
(simple-test "main.scm machines command"
  (lambda ()
    (= 0 (system "./main.scm machines >/dev/null 2>&1"))))

;; Test 4: Test invalid command handling
(simple-test "main.scm invalid command handling"
  (lambda ()
    (not (= 0 (system "./main.scm invalid-command >/dev/null 2>&1")))))

(test-summary)
