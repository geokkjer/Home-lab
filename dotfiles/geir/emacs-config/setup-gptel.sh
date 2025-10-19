#!/usr/bin/env bash
# GPTel Setup Script for GitHub Copilot + grey-area Ollama
# Simple one-time setup to get GPTel working with your backends

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_header() {
    clear
    echo -e "${BLUE}╔════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║        GPTel Setup Assistant           ║${NC}"
    echo -e "${BLUE}║   GitHub Copilot + grey-area Ollama    ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════╝${NC}"
    echo
}

print_step() {
    echo -e "${BLUE}▶ $1${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

wait_for_user() {
    echo
    read -p "Press Enter to continue..."
    echo
}

check_prerequisites() {
    print_step "Step 1: Checking Prerequisites"
    echo

    local all_good=true

    # Check if we're in the right directory
    if [[ ! -f "modules/ai-integration.el" ]]; then
        print_error "Not in the emacs-config directory"
        echo "Please run this script from: ~/Projects/home-lab/dotfiles/geir/emacs-config/"
        exit 1
    fi

    # Check for GitHub CLI (should be globally available)
    if command -v gh &> /dev/null; then
        print_success "GitHub CLI found (globally configured)"
    else
        print_error "GitHub CLI not found - this shouldn't happen with your global config"
        print_info "Your NixOS config should provide GitHub CLI globally"
        all_good=false
    fi

    # Check for curl
    if command -v curl &> /dev/null; then
        print_success "curl found"
    else
        print_error "curl not found (required for GPTel)"
        all_good=false
    fi

    # Check Emacs
    if command -v emacs &> /dev/null; then
        print_success "Emacs found"
    else
        print_error "Emacs not found"
        all_good=false
    fi

    if ! $all_good; then
        echo
        print_error "Prerequisites missing. This may indicate a configuration issue."
        print_info "Your NixOS config should provide all required tools globally."
        exit 1
    fi

    echo
    print_success "All prerequisites found!"
    wait_for_user
}

setup_github_auth() {
    print_step "Step 2: GitHub Authentication"
    echo

    # Check if already authenticated
    if gh auth status &> /dev/null; then
        local gh_user=$(gh api user --jq '.login' 2>/dev/null || echo "unknown")
        print_success "Already authenticated with GitHub as: $gh_user"

        # Check Copilot access
        if gh copilot --help &> /dev/null; then
            print_success "GitHub Copilot access confirmed"
            return 0
        else
            print_warning "GitHub Copilot not available"
            print_info "You may need a GitHub Copilot subscription"
            print_info "Visit: https://github.com/features/copilot"
            return 1
        fi
    fi

    print_info "GitHub authentication required for Copilot access"
    echo
    echo "This will open your browser to authenticate with GitHub."
    echo "Make sure you have a GitHub Copilot subscription."
    echo
    read -p "Continue with GitHub authentication? (y/N): " -n 1 -r
    echo

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        print_info "Starting GitHub authentication..."

        if gh auth login --web; then
            print_success "GitHub authentication successful!"

            # Verify Copilot access
            if gh copilot --help &> /dev/null; then
                print_success "GitHub Copilot access confirmed"
                return 0
            else
                print_warning "GitHub Copilot not available"
                print_info "You may need to enable Copilot in your GitHub settings"
                return 1
            fi
        else
            print_error "GitHub authentication failed"
            return 1
        fi
    else
        print_warning "Skipping GitHub authentication"
        print_info "You can run 'gh auth login' manually later"
        return 1
    fi
}

test_grey_area_connectivity() {
    print_step "Step 3: Testing grey-area Ollama Connectivity"
    echo

    print_info "Testing connection to grey-area Ollama (trying Tailscale hostnames)..."

    # Try both bare hostname and Tailscale FQDN
    local grey_area_host=""
    for host in "grey-area" "grey-area.tail807ea.ts.net"; do
        if ping -c 1 -W 2 "$host" &> /dev/null; then
            grey_area_host="$host"
            print_success "$host is reachable via Tailscale"
            break
        fi
    done

    if [[ -n "$grey_area_host" ]]; then
        if curl -s --connect-timeout 5 "http://$grey_area_host:11434/api/version" &> /dev/null; then
            local ollama_version=$(curl -s --connect-timeout 5 "http://$grey_area_host:11434/api/version" | grep -o '"version":"[^"]*"' | cut -d'"' -f4 2>/dev/null || echo "unknown")
            print_success "grey-area Ollama service responding (version: $ollama_version)"

            # Get available models
            local model_response=$(curl -s --connect-timeout 5 "http://$grey_area_host:11434/api/tags" 2>/dev/null)
            if echo "$model_response" | grep -q "models"; then
                local model_count=$(echo "$model_response" | grep -o '"name"' | wc -l)
                print_success "Found $model_count model(s) available on grey-area"

                # List some models
                print_info "Available models:"
                echo "$model_response" | grep -o '"name":"[^"]*"' | head -5 | sed 's/"name":"//; s/"//; s/^/  - /'
                if [[ $model_count -gt 5 ]]; then
                    echo "  ... and $((model_count - 5)) more"
                fi
            else
                print_warning "grey-area Ollama responding but no models found"
                print_info "You may need to pull some models on grey-area"
            fi
            return 0
        else
            print_error "grey-area Ollama service not responding on port 11434"
            print_info "Check if Ollama service is running on grey-area"
            return 1
        fi
    else
        print_error "Cannot reach grey-area via Tailscale"
        print_info "Check Tailscale connection and try: tailscale status"
        print_info "Also try: ping grey-area"
        return 1
    fi
}

test_emacs_integration() {
    print_step "Step 4: Testing Emacs Integration"
    echo

    print_info "Testing if GPTel loads correctly in Emacs..."

    # Create a test script
    local test_script=$(mktemp)
    cat > "$test_script" << 'EOF'
(condition-case err
    (progn
      ;; Set load path
      (add-to-list 'load-path default-directory)
      (add-to-list 'load-path (expand-file-name "modules" default-directory))

      ;; Try to require gptel
      (require 'gptel nil t)
      (if (featurep 'gptel)
          (progn
            (message "SUCCESS: gptel package available")
            ;; Try to load ai-integration
            (require 'ai-integration nil t)
            (if (featurep 'ai-integration)
                (message "SUCCESS: ai-integration module loaded")
              (message "FAIL: ai-integration module failed")))
        (message "FAIL: gptel package not available"))

      (message "EMACS_TEST_COMPLETE"))
  (error (message "ERROR: %s" (error-message-string err))))
EOF

    if emacs --batch --load "$test_script" 2>/dev/null | grep -q "SUCCESS.*gptel package available"; then
        print_success "GPTel package loads successfully"

        if emacs --batch --load "$test_script" 2>/dev/null | grep -q "SUCCESS.*ai-integration module loaded"; then
            print_success "ai-integration module loads successfully"
        else
            print_error "ai-integration module failed to load"
        fi
    else
        print_error "GPTel package failed to load"
        print_info "You may need to rebuild your NixOS configuration"
    fi

    rm -f "$test_script"
}

show_usage_guide() {
    print_step "Step 5: Usage Guide"
    echo

    echo -e "${BLUE}🎉 Setup Complete! Here's how to use GPTel:${NC}"
    echo
    echo -e "${GREEN}Key Bindings:${NC}"
    echo "  C-c g c  - Start GPTel chat buffer"
    echo "  C-c g b  - Switch between backends (Copilot ↔ Ollama)"
    echo "  C-c g e  - Explain selected code"
    echo "  C-c g v  - Review selected code"
    echo "  C-c g x  - Send error context for debugging"
    echo "  C-c g p  - Start project-specific chat"
    echo "  C-c g m  - Generate git commit message"
    echo "  C-c g t  - Quick chat (without opening buffer)"
    echo "  C-c g n  - Open GPTel menu for more options"
    echo
    echo -e "${GREEN}Backend Info:${NC}"
    echo "  • GitHub Copilot: Latest AI models, requires internet"
    echo "  • grey-area Ollama: Local models, private, works offline"
    echo
    echo -e "${GREEN}Quick Start:${NC}"
    echo "  1. Open Emacs"
    echo "  2. Press C-c g c to start GPTel"
    echo "  3. Type your question and press C-c RET to send"
    echo "  4. Use C-c g b to switch backends as needed"
    echo
    echo -e "${GREEN}Tips:${NC}"
    echo "  • Select code before using C-c g e or C-c g v"
    echo "  • Use grey-area Ollama for sensitive/private queries"
    echo "  • Project context (C-c g p) includes README info"
    echo "  • Commit messages (C-c g m) analyze your git diff"
    echo
}

main() {
    print_header

    echo "This script will help you set up GPTel with:"
    echo "  • GitHub Copilot Chat (cloud AI)"
    echo "  • grey-area Ollama (local AI)"
    echo

    wait_for_user

    check_prerequisites

    local github_ok=false
    local ollama_ok=false

    if setup_github_auth; then
        github_ok=true
    fi

    if test_grey_area_connectivity; then
        ollama_ok=true
    fi

    wait_for_user
    test_emacs_integration
    wait_for_user

    # Summary
    echo
    print_step "Setup Summary"
    echo

    if $github_ok; then
        print_success "GitHub Copilot: Ready"
    else
        print_warning "GitHub Copilot: Not available"
    fi

    if $ollama_ok; then
        print_success "grey-area Ollama: Ready"
    else
        print_warning "grey-area Ollama: Not available"
    fi

    if $github_ok || $ollama_ok; then
        echo
        show_usage_guide
        echo
        print_success "GPTel setup complete! Try it out in Emacs with C-c g c"
    else
        echo
        print_error "No backends are available. Please fix the issues above."
        echo
        echo "Manual steps:"
        echo "  1. For GitHub Copilot: Run 'gh auth login'"
        echo "  2. For Ollama: Check grey-area machine and network"
        echo "  3. Re-run this script: ./setup-gptel.sh"
    fi

    echo
    print_info "For detailed verification, run: ./verify-gptel-setup.sh"
    echo
}

# Run main function
main "$@"
