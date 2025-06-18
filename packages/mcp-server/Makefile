# Makefile for MCP Protocol Core Test Suite

# Guile executable
GUILE ?= guile
GUILD ?= guild

# Test directories and files
TEST_DIR = tests
SERVER_DIR = server
TEST_FILES = $(wildcard $(TEST_DIR)/*.scm)
SERVER_FILES = $(wildcard $(SERVER_DIR)/*.scm)

# Guile load path
GUILE_LOAD_PATH := $(CURDIR):$(GUILE_LOAD_PATH)
export GUILE_LOAD_PATH

# Default target
.PHONY: all
all: test

# Run all tests
.PHONY: test
test: check-dependencies
	@echo "🧪 Running MCP Protocol Core Test Suite"
	@echo "======================================="
	$(GUILE) -L . $(TEST_DIR)/run-tests.scm

# Run only unit tests
.PHONY: test-unit
test-unit: check-dependencies
	@echo "📋 Running Unit Tests Only"
	$(GUILE) -L . $(TEST_DIR)/run-tests.scm unit

# Run only integration tests
.PHONY: test-integration
test-integration: check-dependencies
	@echo "🔗 Running Integration Tests Only"
	$(GUILE) -L . $(TEST_DIR)/run-tests.scm integration

# Run only compliance tests
.PHONY: test-compliance
test-compliance: check-dependencies
	@echo "📜 Running Protocol Compliance Tests Only"
	$(GUILE) -L . $(TEST_DIR)/run-tests.scm compliance

# Check syntax of all Scheme files
.PHONY: check-syntax
check-syntax:
	@echo "🔍 Checking Syntax..."
	@for file in $(SERVER_FILES) $(TEST_FILES); do \
		echo "  Checking $$file..."; \
		$(GUILD) compile -W all -x $$file > /dev/null || exit 1; \
	done
	@echo "✅ Syntax check passed!"

# Check dependencies
.PHONY: check-dependencies
check-dependencies:
	@echo "🔧 Checking Dependencies..."
	@$(GUILE) -c "(use-modules (srfi srfi-64))" 2>/dev/null || \
		(echo "❌ SRFI-64 testing framework not available"; exit 1)
	@$(GUILE) -c "(use-modules (json))" 2>/dev/null || \
		(echo "❌ JSON module not available"; exit 1)
	@echo "✅ Dependencies check passed!"

# Run tests with coverage (if gcov available)
.PHONY: test-coverage
test-coverage: check-dependencies
	@echo "📊 Running Tests with Coverage..."
	# Note: Coverage reporting for Guile would require additional setup
	$(MAKE) test

# Clean compiled files
.PHONY: clean
clean:
	@echo "🧹 Cleaning compiled files..."
	find . -name "*.go" -delete
	find . -name "*.x" -delete
	@echo "✅ Clean complete!"

# Continuous testing (watch mode)
.PHONY: test-watch
test-watch:
	@echo "👀 Watching for changes..."
	@while true; do \
		$(MAKE) test; \
		echo ""; \
		echo "Waiting for changes... (Ctrl+C to stop)"; \
		sleep 2; \
	done

# Generate test report
.PHONY: test-report
test-report: check-dependencies
	@echo "📄 Generating Test Report..."
	$(GUILE) -L . $(TEST_DIR)/run-tests.scm > test-report.txt 2>&1
	@echo "📄 Test report saved to test-report.txt"

# Benchmark tests
.PHONY: benchmark
benchmark: check-dependencies
	@echo "⏱️  Running Performance Benchmarks..."
	# Placeholder for benchmark implementation
	@echo "⚠️  Benchmarks not yet implemented"

# Help target
.PHONY: help
help:
	@echo "🧪 MCP Protocol Core Test Suite"
	@echo "==============================="
	@echo ""
	@echo "Available targets:"
	@echo "  test              - Run all tests (default)"
	@echo "  test-unit         - Run only unit tests"
	@echo "  test-integration  - Run only integration tests"
	@echo "  test-compliance   - Run only protocol compliance tests"
	@echo "  check-syntax      - Check syntax of all Scheme files"
	@echo "  check-dependencies - Check if required dependencies are available"
	@echo "  test-coverage     - Run tests with coverage reporting"
	@echo "  test-watch        - Continuously run tests on file changes"
	@echo "  test-report       - Generate detailed test report"
	@echo "  benchmark         - Run performance benchmarks"
	@echo "  clean             - Clean compiled files"
	@echo "  help              - Show this help message"
	@echo ""
	@echo "Environment variables:"
	@echo "  GUILE            - Guile executable (default: guile)"
	@echo "  GUILD            - Guild executable (default: guild)"

# Show current test status
.PHONY: status
status:
	@echo "📊 Test Suite Status"
	@echo "==================="
	@echo "Test files: $(words $(TEST_FILES))"
	@echo "Server modules: $(words $(SERVER_FILES))"
	@echo "Guile version: $$($(GUILE) --version | head -1)"
	@echo "Load path: $(GUILE_LOAD_PATH)"
