#!/usr/bin/env guile
!#

;; TDD Test for Missing Functions
;; Following K.I.S.S principles - test one thing at a time

(add-to-load-path ".")

(use-modules (ice-9 format)
             (utils logging))

(format #t "🧪 MISSING FUNCTIONS TEST\n")
(format #t "==========================\n\n")

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

;; Test 1: Test get-color function exists (should be in utils/logging)
(simple-test "get-color function exists"
  (lambda ()
    (catch #t
      (lambda ()
        (use-modules (utils logging))
        (and (defined? 'get-color)
             (procedure? get-color)))
      (lambda (key . args) #f))))

;; Test 2: Test get-all-machines-pure function exists (should be in utils/config)
(simple-test "get-all-machines-pure function exists"
  (lambda ()
    (catch #t
      (lambda ()
        (use-modules (utils config accessor))
        (and (defined? 'get-all-machines-pure)
             (procedure? get-all-machines-pure)))
      (lambda (key . args) #f))))

;; Test 3: Test get-color basic functionality
(simple-test "get-color basic functionality"
  (lambda ()
    (catch #t
      (lambda ()
        (use-modules (utils logging))
        (let ((blue (get-color 'blue))
              (reset (get-color 'reset)))
          (and (string? blue)
               (string? reset)
               (> (string-length blue) 0)
               (> (string-length reset) 0))))
      (lambda (key . args) #f))))

(test-summary)
