#!/usr/bin/env bash
# GPTel Setup Verification Script
# Verifies GPTel integration with GitHub Copilot and grey-area Ollama

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0
WARNINGS=0

print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  GPTel Setup Verification${NC}"
    echo -e "${BLUE}  (GitHub Copilot + grey-area Ollama)${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo
}

print_section() {
    echo -e "${BLUE}## $1${NC}"
}

print_pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((PASSED++))
}

print_fail() {
    echo -e "${RED}✗${NC} $1"
    ((FAILED++))
}

print_warn() {
    echo -e "${YELLOW}⚠${NC} $1"
    ((WARNINGS++))
}

print_info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

check_emacs_availability() {
    print_section "Emacs Environment"

    if command -v emacs &> /dev/null; then
        EMACS_VERSION=$(emacs --version | head -n1)
        print_pass "Emacs found: $EMACS_VERSION"

        # Check if it's the expected Nix version
        if [[ "$EMACS_VERSION" == *"29"* ]] || [[ "$EMACS_VERSION" == *"30"* ]]; then
            print_pass "Emacs version is modern (29+ recommended)"
        else
            print_warn "Emacs version might be old, consider upgrading"
        fi
    else
        print_fail "Emacs not found in PATH"
        return 1
    fi

    # Check EMACS_PROFILE
    if [[ -n "${EMACS_PROFILE:-}" ]]; then
        print_pass "EMACS_PROFILE set to: $EMACS_PROFILE"
    else
        print_warn "EMACS_PROFILE not set (non-Nix environment?)"
    fi

    echo
}

check_dependencies() {
    print_section "Dependencies"

    if command -v curl &> /dev/null; then
        CURL_VERSION=$(curl --version | head -n1)
        print_pass "curl found: $CURL_VERSION"

        # Check if curl supports HTTP/2 (better for streaming)
        if curl --version | grep -q "HTTP2"; then
            print_pass "curl supports HTTP/2 (good for streaming)"
        else
            print_warn "curl doesn't support HTTP/2 (streaming might be slower)"
        fi
    else
        print_fail "curl not found (required for GPTel)"
    fi

    # Check for GitHub CLI (globally configured)
    if command -v gh &> /dev/null; then
        GH_VERSION=$(gh --version | head -n1)
        print_pass "GitHub CLI found (globally configured): $GH_VERSION"

        # Check if authenticated
        if gh auth status &> /dev/null; then
            print_pass "GitHub CLI authenticated"

            # Check Copilot access
            if gh copilot --help &> /dev/null; then
                print_pass "GitHub Copilot access available"
            else
                print_warn "GitHub Copilot not available (subscription required)"
            fi
        else
            print_fail "GitHub CLI not authenticated (run 'gh auth login')"
        fi
    else
        print_fail "GitHub CLI not found - check your global NixOS configuration"
    fi

    if command -v git &> /dev/null; then
        print_pass "git found (for commit message generation)"
    else
        print_warn "git not found (commit message generation won't work)"
    fi

    # Check grey-area connectivity via Tailscale
    GREY_AREA_HOST=""
    for host in "grey-area" "grey-area.tail807ea.ts.net"; do
        if ping -c 1 -W 2 "$host" &> /dev/null; then
            GREY_AREA_HOST="$host"
            print_pass "$host reachable via Tailscale"
            break
        fi
    done

    if [[ -n "$GREY_AREA_HOST" ]]; then
        # Check Ollama on grey-area
        if curl -s --connect-timeout 5 "http://$GREY_AREA_HOST:11434/api/tags" &> /dev/null; then
            print_pass "grey-area Ollama service responding"

            # Try to get model list
            OLLAMA_RESPONSE=$(curl -s --connect-timeout 5 "http://$GREY_AREA_HOST:11434/api/tags")
            if echo "$OLLAMA_RESPONSE" | grep -q "models"; then
                MODEL_COUNT=$(echo "$OLLAMA_RESPONSE" | grep -o '"name"' | wc -l)
                print_pass "grey-area Ollama has $MODEL_COUNT model(s) available"
            else
                print_warn "grey-area Ollama responding but no models found"
            fi
        else
            print_warn "grey-area Ollama not responding on port 11434"
        fi
    else
        print_warn "grey-area not reachable via Tailscale (check connection)"
    fi

    # Check for local ollama (optional)
    if command -v ollama &> /dev/null; then
        if ollama list &> /dev/null; then
            OLLAMA_MODELS=$(ollama list | tail -n +2 | wc -l)
            print_pass "local ollama running with $OLLAMA_MODELS model(s)"
        else
            print_info "local ollama found but not running"
        fi
    else
        print_info "local ollama not found (grey-area Ollama available)"
    fi

    echo
}

check_emacs_config() {
    print_section "Emacs Configuration"

    # Check if ai-integration module exists
    if [[ -f "modules/ai-integration.el" ]]; then
        print_pass "ai-integration.el module found"
    else
        print_fail "ai-integration.el module not found"
    fi

    # Check if module is referenced in init file
    if [[ -f "init-nix.el" ]]; then
        if grep -q "ai-integration" init-nix.el; then
            print_pass "ai-integration referenced in init-nix.el"
        else
            print_fail "ai-integration not referenced in init-nix.el"
        fi
    else
        print_fail "init-nix.el not found"
    fi

    # Check NixOS module configuration
    if [[ -f "../../modules/development/emacs.nix" ]]; then
        if grep -q "gptel" "../../modules/development/emacs.nix"; then
            print_pass "gptel package included in emacs.nix"
        else
            print_fail "gptel package not found in emacs.nix"
        fi

        if grep -q "ai-integration.el" "../../modules/development/emacs.nix"; then
            print_pass "ai-integration module configured in emacs.nix"
        else
            print_fail "ai-integration module not configured in emacs.nix"
        fi
    else
        print_warn "emacs.nix not found at expected location"
    fi

    echo
}

check_authentication() {
    print_section "Authentication & Connectivity"

    # GitHub authentication status
    if command -v gh &> /dev/null; then
        if gh auth status &> /dev/null; then
            GH_USER=$(gh api user --jq '.login' 2>/dev/null || echo "unknown")
            print_pass "GitHub authenticated as: $GH_USER"

            # Check Copilot subscription
            if gh copilot --help &> /dev/null; then
                print_pass "GitHub Copilot subscription active"
            else
                print_warn "GitHub Copilot not available (check subscription)"
            fi
        else
            print_fail "GitHub not authenticated"
            print_info "Run 'gh auth login' to authenticate"
        fi
    else
        print_fail "GitHub CLI not available - check your global NixOS configuration"
    fi

    # Grey-area Ollama connectivity via Tailscale
    GREY_AREA_HOST=""
    for host in "grey-area" "grey-area.tail807ea.ts.net"; do
        if ping -c 1 -W 2 "$host" &> /dev/null; then
            GREY_AREA_HOST="$host"
            print_pass "$host network connectivity OK via Tailscale"
            break
        fi
    done

    if [[ -n "$GREY_AREA_HOST" ]]; then
        if curl -s --connect-timeout 5 "http://$GREY_AREA_HOST:11434/api/version" &> /dev/null; then
            OLLAMA_VERSION=$(curl -s --connect-timeout 5 "http://$GREY_AREA_HOST:11434/api/version" | grep -o '"version":"[^"]*"' | cut -d'"' -f4 2>/dev/null || echo "unknown")
            print_pass "grey-area Ollama service accessible (version: $OLLAMA_VERSION)"
        else
            print_warn "grey-area Ollama service not responding"
        fi
    else
        print_fail "Cannot reach grey-area via Tailscale"
        print_info "Check Tailscale connection: tailscale status"
    fi

    echo
}

test_emacs_integration() {
    print_section "Emacs Integration Test"

    # Create a temporary Emacs test
    TEMP_TEST=$(mktemp)
    cat > "$TEMP_TEST" << 'EOF'
(condition-case err
    (progn
      ;; Set load path to current directory
      (add-to-list 'load-path default-directory)
      (add-to-list 'load-path (expand-file-name "modules" default-directory))

      ;; Try to load gptel
      (require 'gptel nil t)
      (if (featurep 'gptel)
          (message "SUCCESS: gptel package loaded")
        (message "FAIL: gptel package not available"))

      ;; Try to load ai-integration
      (if (file-exists-p "modules/ai-integration.el")
          (progn
            (require 'ai-integration nil t)
            (if (featurep 'ai-integration)
                (message "SUCCESS: ai-integration module loaded")
              (message "FAIL: ai-integration module failed to load")))
        (message "FAIL: ai-integration.el not found"))

      ;; Check key bindings
      (if (fboundp 'gptel)
          (message "SUCCESS: gptel function available")
        (message "FAIL: gptel function not available"))

      (message "TEST_COMPLETE"))
  (error (message "ERROR: %s" (error-message-string err))))
EOF

    # Run the test
    if emacs --batch --load "$TEMP_TEST" 2>/dev/null | grep -q "SUCCESS.*gptel package loaded"; then
        print_pass "GPTel package loads successfully"
    else
        print_fail "GPTel package failed to load"
    fi

    if emacs --batch --load "$TEMP_TEST" 2>/dev/null | grep -q "SUCCESS.*ai-integration module loaded"; then
        print_pass "ai-integration module loads successfully"
    else
        print_fail "ai-integration module failed to load"
    fi

    if emacs --batch --load "$TEMP_TEST" 2>/dev/null | grep -q "SUCCESS.*gptel function available"; then
        print_pass "GPTel functions are available"
    else
        print_fail "GPTel functions are not available"
    fi

    rm -f "$TEMP_TEST"
    echo
}

test_network_connectivity() {
    print_section "Network Connectivity"

    # Test GitHub API connectivity
    if curl -s --connect-timeout 5 https://api.github.com > /dev/null 2>&1; then
        print_pass "GitHub API endpoint reachable"
    else
        print_warn "GitHub API endpoint not reachable (network/firewall?)"
    fi

    # Test grey-area Ollama detailed connectivity
    GREY_AREA_HOST=""
    for host in "grey-area" "grey-area.tail807ea.ts.net"; do
        if ping -c 1 -W 1 "$host" &> /dev/null; then
            GREY_AREA_HOST="$host"
            break
        fi
    done

    if [[ -n "$GREY_AREA_HOST" ]] && curl -s --connect-timeout 5 "http://$GREY_AREA_HOST:11434/api/tags" > /dev/null 2>&1; then
        print_pass "grey-area Ollama API responding to model queries via Tailscale"
    else
        print_warn "grey-area Ollama API not responding (check Tailscale connection or service)"
    fi

    echo
}

print_summary() {
    print_section "Summary"

    echo -e "Results:"
    echo -e "  ${GREEN}Passed: $PASSED${NC}"
    if [[ $WARNINGS -gt 0 ]]; then
        echo -e "  ${YELLOW}Warnings: $WARNINGS${NC}"
    fi
    if [[ $FAILED -gt 0 ]]; then
        echo -e "  ${RED}Failed: $FAILED${NC}"
    fi

    echo

    if [[ $FAILED -eq 0 ]]; then
        if [[ $WARNINGS -eq 0 ]]; then
            echo -e "${GREEN}🎉 GPTel setup looks perfect!${NC}"
            echo -e "Try: ${BLUE}emacs${NC} then ${BLUE}M-x gptel${NC}"
        else
            echo -e "${YELLOW}⚠ GPTel setup mostly working with some warnings${NC}"
            echo -e "Try: ${BLUE}emacs${NC} then ${BLUE}M-x gptel${NC}"
        fi
    else
        echo -e "${RED}❌ GPTel setup has issues that need attention${NC}"
        echo
        echo "Next steps:"
        echo "1. Fix any failed checks above"
        echo "2. Run 'gh auth login' if GitHub authentication failed"
        echo "3. Check grey-area.lan network connectivity if needed"
        echo "4. Rebuild your NixOS configuration if required"
        echo "5. Re-run this script to verify"
    fi

    echo
    echo -e "${BLUE}Quick Start:${NC}"
    echo "1. Ensure GitHub authentication: gh auth login"
    echo "2. Start Emacs"
    echo "3. Press C-c g c (or M-x gptel)"
    echo "4. Use C-c g b to switch between Copilot and Ollama"
    echo "5. Start chatting!"
    echo
    echo -e "${BLUE}Key Bindings:${NC}"
    echo "  C-c g c  - Start GPTel"
    echo "  C-c g b  - Switch backend (Copilot/Ollama)"
    echo "  C-c g e  - Explain code"
    echo "  C-c g v  - Review code"
    echo "  C-c g x  - Send error context"
    echo "  C-c g p  - Project context"
    echo "  C-c g m  - Generate commit message"
}

main() {
    print_header
    check_emacs_availability
    check_dependencies
    check_emacs_config
    check_authentication
    test_emacs_integration
    test_network_connectivity
    print_summary
}

# Run main function
main "$@"
