#!/usr/bin/env guile
!#

;; Simple functionality test

(add-to-load-path ".")

(use-modules (ice-9 format)
             (lab core)
             (lab machines)
             (utils logging))

(format #t "🧪 LAB TOOL FUNCTIONALITY TEST\n")
(format #t "===============================\n\n")

;; Test basic functionality
(format #t "Testing core functionality:\n")
(let ((machines (list-machines)))
  (format #t "✅ Found ~a machines: ~a\n" (length machines) machines))

(let ((status (get-infrastructure-status)))
  (format #t "✅ Infrastructure status: ~a machines checked\n" (length status)))

(format #t "\n🎉 Basic functionality working!\n")
