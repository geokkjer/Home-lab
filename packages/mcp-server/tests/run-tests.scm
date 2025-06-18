;; Test Suite Main Runner for MCP Protocol Core
;; This module orchestrates the execution of all test suites

(define-module (tests run-tests)
  #:use-module (srfi srfi-64)
  #:use-module (tests jsonrpc-tests)
  #:use-module (tests protocol-tests)
  #:use-module (tests transport-tests)
  #:use-module (tests router-tests)
  #:use-module (tests validation-tests)
  #:use-module (tests error-handling-tests)
  #:use-module (tests integration-tests)
  #:use-module (tests protocol-compliance-tests)
  #:export (run-all-tests
            run-unit-tests
            run-integration-tests
            run-compliance-tests))

;; Test suite configuration
(define *test-config*
  `((verbose . #t)
    (stop-on-failure . #f)
    (parallel . #f)
    (coverage . #t)))

;; Main test runner
(define (run-all-tests)
  "Run all test suites for the MCP Protocol Core"
  (test-begin "MCP Protocol Core Test Suite")
  
  (display "🧪 Running MCP Protocol Core Test Suite\n")
  (display "=====================================\n\n")
  
  ;; Unit tests
  (display "📋 Running Unit Tests...\n")
  (run-unit-tests)
  
  ;; Integration tests
  (display "\n🔗 Running Integration Tests...\n")
  (run-integration-tests)
  
  ;; Protocol compliance tests
  (display "\n📜 Running Protocol Compliance Tests...\n")
  (run-compliance-tests)
  
  (display "\n✅ Test Suite Complete!\n")
  
  ;; Display summary before test-end
  (display-test-summary)
  
  (test-end "MCP Protocol Core Test Suite"))

(define (run-unit-tests)
  "Run all unit test suites"
  (test-begin "Unit Tests")
  
  (display "  • JSON-RPC Tests...\n")
  (run-jsonrpc-tests)
  
  (display "  • Protocol Tests...\n")
  (run-protocol-tests)
  
  (display "  • Transport Tests...\n")
  (run-transport-tests)
  
  (display "  • Router Tests...\n")
  (run-router-tests)
  
  (display "  • Validation Tests...\n")
  (run-validation-tests)
  
  (display "  • Error Handling Tests...\n")
  (run-error-handling-tests)
  
  (test-end "Unit Tests"))

(define (run-integration-tests)
  "Run integration test suites"
  (test-begin "Integration Tests")
  
  (display "  • Full Server Integration...\n")
  (run-server-integration-tests)
  
  (test-end "Integration Tests"))

(define (run-compliance-tests)
  "Run protocol compliance test suites"
  (test-begin "Protocol Compliance Tests")
  
  (display "  • MCP 2024-11-05 Specification...\n")
  (run-mcp-compliance-tests)
  
  (test-end "Protocol Compliance Tests"))

(define (display-test-summary)
  "Display a summary of test results"
  (let* ((runner (test-runner-current))
         (passed (test-runner-pass-count runner))
         (failed (test-runner-fail-count runner))
         (skipped (test-runner-skip-count runner)))
    
    (display "\n📊 Test Summary:\n")
    (display "================\n")
    (format #t "  ✅ Passed:  ~a\n" passed)
    (format #t "  ❌ Failed:  ~a\n" failed)
    (format #t "  ⏭️  Skipped: ~a\n" skipped)
    (format #t "  📈 Total:   ~a\n" (+ passed failed skipped))
    
    (if (> failed 0)
        (begin
          (display "\n🚨 Some tests failed! Please review the output above.\n")
          (exit 1))
        (display "\n🎉 All tests passed!\n"))))

;; Convenience function for running tests from command line
(define (main args)
  "Main entry point for running tests"
  (cond
   ((and (> (length args) 1) (string=? (cadr args) "unit"))
    (run-unit-tests))
   ((and (> (length args) 1) (string=? (cadr args) "integration"))
    (run-integration-tests))
   ((and (> (length args) 1) (string=? (cadr args) "compliance"))
    (run-compliance-tests))
   (else
    (run-all-tests))))

;; Run tests when script is executed directly
(main (command-line))
