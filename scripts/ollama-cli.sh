#!/usr/bin/env bash
# Ollama Home Lab CLI Tool
# Provides convenient commands for managing Ollama in the home lab environment

set -euo pipefail

# Configuration
OLLAMA_HOST="${OLLAMA_HOST:-127.0.0.1}"
OLLAMA_PORT="${OLLAMA_PORT:-11434}"
OLLAMA_URL="http://${OLLAMA_HOST}:${OLLAMA_PORT}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Helper functions
print_success() { echo -e "${GREEN}✓${NC} $1"; }
print_error() { echo -e "${RED}✗${NC} $1"; }
print_info() { echo -e "${BLUE}ℹ${NC} $1"; }
print_warning() { echo -e "${YELLOW}⚠${NC} $1"; }

# Check if ollama service is running
check_service() {
    if ! systemctl is-active --quiet ollama; then
        print_error "Ollama service is not running"
        echo "Start it with: sudo systemctl start ollama"
        exit 1
    fi
}

# Wait for API to be ready
wait_for_api() {
    local timeout=30
    local count=0
    
    while ! curl -s --connect-timeout 2 "$OLLAMA_URL/api/tags" >/dev/null 2>&1; do
        if [ $count -ge $timeout ]; then
            print_error "Timeout waiting for Ollama API"
            exit 1
        fi
        echo "Waiting for Ollama API..."
        sleep 1
        ((count++))
    done
}

# Commands
cmd_status() {
    echo "Ollama Service Status"
    echo "===================="
    
    if systemctl is-active --quiet ollama; then
        print_success "Service is running"
        
        # Service details
        echo
        echo "Service Information:"
        systemctl show ollama --property=MainPID,ActiveState,LoadState,SubState | sed 's/^/  /'
        
        # Memory usage
        memory=$(systemctl show ollama --property=MemoryCurrent --value)
        if [[ "$memory" != "[not set]" ]] && [[ -n "$memory" ]]; then
            memory_mb=$((memory / 1024 / 1024))
            echo "  Memory: ${memory_mb}MB"
        fi
        
        # API status
        echo
        if curl -s --connect-timeout 5 "$OLLAMA_URL/api/tags" >/dev/null; then
            print_success "API is responding"
        else
            print_error "API is not responding"
        fi
        
        # Model count
        models=$(curl -s "$OLLAMA_URL/api/tags" 2>/dev/null | jq '.models | length' 2>/dev/null || echo "0")
        echo "  Models installed: $models"
        
    else
        print_error "Service is not running"
        echo "Start with: sudo systemctl start ollama"
    fi
}

cmd_models() {
    check_service
    wait_for_api
    
    echo "Installed Models"
    echo "================"
    
    models_json=$(curl -s "$OLLAMA_URL/api/tags")
    model_count=$(echo "$models_json" | jq '.models | length')
    
    if [ "$model_count" -eq 0 ]; then
        print_warning "No models installed"
        echo
        echo "Install a model with: $0 pull <model>"
        echo "Popular models:"
        echo "  llama3.3:8b    - General purpose (4.7GB)"
        echo "  codellama:7b    - Code assistance (3.8GB)"
        echo "  mistral:7b      - Fast inference (4.1GB)"
        echo "  qwen2.5:7b     - Multilingual (4.4GB)"
    else
        printf "%-25s %-10s %-15s %s\n" "NAME" "SIZE" "MODIFIED" "ID"
        echo "$(printf '%*s' 80 '' | tr ' ' '-')"
        
        echo "$models_json" | jq -r '.models[] | [.name, (.size / 1024 / 1024 / 1024 | floor | tostring + "GB"), (.modified_at | split("T")[0]), .digest[7:19]] | @tsv' | \
        while IFS=$'\t' read -r name size modified id; do
            printf "%-25s %-10s %-15s %s\n" "$name" "$size" "$modified" "$id"
        done
    fi
}

cmd_pull() {
    if [ $# -eq 0 ]; then
        print_error "Usage: $0 pull <model>"
        echo
        echo "Popular models:"
        echo "  llama3.3:8b    - Meta's latest Llama model"
        echo "  codellama:7b    - Code-focused model"
        echo "  mistral:7b      - Mistral AI's efficient model"
        echo "  gemma2:9b       - Google's Gemma model"
        echo "  qwen2.5:7b     - Multilingual model"
        echo "  phi4:14b        - Microsoft's reasoning model"
        exit 1
    fi
    
    check_service
    wait_for_api
    
    model="$1"
    print_info "Pulling model: $model"
    
    # Check if model already exists
    if ollama list | grep -q "^$model"; then
        print_warning "Model $model is already installed"
        read -p "Continue anyway? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 0
        fi
    fi
    
    # Pull the model
    ollama pull "$model"
    print_success "Model $model pulled successfully"
}

cmd_remove() {
    if [ $# -eq 0 ]; then
        print_error "Usage: $0 remove <model>"
        echo
        echo "Available models:"
        ollama list | tail -n +2 | awk '{print "  " $1}'
        exit 1
    fi
    
    check_service
    
    model="$1"
    
    # Confirm removal
    print_warning "This will permanently remove model: $model"
    read -p "Are you sure? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 0
    fi
    
    ollama rm "$model"
    print_success "Model $model removed"
}

cmd_chat() {
    if [ $# -eq 0 ]; then
        # List available models for selection
        models_json=$(curl -s "$OLLAMA_URL/api/tags" 2>/dev/null)
        model_count=$(echo "$models_json" | jq '.models | length' 2>/dev/null || echo "0")
        
        if [ "$model_count" -eq 0 ]; then
            print_error "No models available"
            echo "Install a model first: $0 pull llama3.3:8b"
            exit 1
        fi
        
        echo "Available models:"
        echo "$models_json" | jq -r '.models[] | "  \(.name)"' 2>/dev/null
        echo
        read -p "Enter model name: " model
    else
        model="$1"
    fi
    
    check_service
    wait_for_api
    
    print_info "Starting chat with $model"
    print_info "Type 'exit' or press Ctrl+C to quit"
    echo
    
    ollama run "$model"
}

cmd_test() {
    check_service
    wait_for_api
    
    echo "Running Ollama Tests"
    echo "==================="
    
    # Get first available model
    first_model=$(curl -s "$OLLAMA_URL/api/tags" 2>/dev/null | jq -r '.models[0].name // empty' 2>/dev/null)
    
    if [[ -z "$first_model" ]]; then
        print_error "No models available for testing"
        echo "Install a model first: $0 pull llama3.3:8b"
        exit 1
    fi
    
    print_info "Testing with model: $first_model"
    
    # Test 1: API connectivity
    echo
    echo "Test 1: API Connectivity"
    if curl -s "$OLLAMA_URL/api/tags" >/dev/null; then
        print_success "API is responding"
    else
        print_error "API connectivity failed"
        exit 1
    fi
    
    # Test 2: Model listing
    echo
    echo "Test 2: Model Listing"
    if models=$(ollama list 2>/dev/null); then
        model_count=$(echo "$models" | wc -l)
        print_success "Can list models ($((model_count - 1)) found)"
    else
        print_error "Cannot list models"
        exit 1
    fi
    
    # Test 3: Simple generation
    echo
    echo "Test 3: Text Generation"
    print_info "Generating response (this may take a moment)..."
    
    start_time=$(date +%s)
    response=$(echo "Hello" | ollama run "$first_model" --nowordwrap 2>/dev/null | head -c 100)
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    
    if [[ -n "$response" ]]; then
        print_success "Text generation successful (${duration}s)"
        echo "Response: ${response}..."
    else
        print_error "Text generation failed"
        exit 1
    fi
    
    # Test 4: API generation
    echo
    echo "Test 4: API Generation"
    api_response=$(curl -s -X POST "$OLLAMA_URL/api/generate" \
        -H "Content-Type: application/json" \
        -d "{\"model\": \"$first_model\", \"prompt\": \"Hello\", \"stream\": false}" \
        2>/dev/null | jq -r '.response // empty' 2>/dev/null)
    
    if [[ -n "$api_response" ]]; then
        print_success "API generation successful"
    else
        print_error "API generation failed"
        exit 1
    fi
    
    echo
    print_success "All tests passed!"
}

cmd_logs() {
    echo "Ollama Service Logs"
    echo "=================="
    echo "Press Ctrl+C to exit"
    echo
    
    journalctl -u ollama -f --output=short-iso
}

cmd_monitor() {
    # Use the monitoring script if available
    monitor_script="/home/geir/Home-lab/scripts/monitor-ollama.sh"
    if [[ -x "$monitor_script" ]]; then
        "$monitor_script" "$@"
    else
        print_error "Monitoring script not found: $monitor_script"
        echo "Running basic status check instead..."
        cmd_status
    fi
}

cmd_restart() {
    print_info "Restarting Ollama service..."
    sudo systemctl restart ollama
    
    print_info "Waiting for service to start..."
    sleep 3
    
    if systemctl is-active --quiet ollama; then
        print_success "Service restarted successfully"
        wait_for_api
        print_success "API is ready"
    else
        print_error "Service failed to start"
        echo "Check logs with: $0 logs"
        exit 1
    fi
}

cmd_help() {
    cat << EOF
Ollama Home Lab CLI Tool

Usage: $0 <command> [arguments]

Commands:
  status              Show service status and basic information
  models              List installed models
  pull <model>        Download and install a model
  remove <model>      Remove an installed model
  chat [model]        Start interactive chat (prompts for model if not specified)
  test                Run basic functionality tests
  logs                Show live service logs
  monitor [options]   Run comprehensive monitoring (see monitor --help)
  restart             Restart the Ollama service
  help                Show this help message

Examples:
  $0 status                   # Check service status
  $0 models                   # List installed models  
  $0 pull llama3.3:8b        # Install Llama 3.3 8B model
  $0 chat codellama:7b       # Start chat with CodeLlama
  $0 test                     # Run functionality tests
  $0 monitor --test-inference # Run monitoring with inference test

Environment Variables:
  OLLAMA_HOST         Ollama host (default: 127.0.0.1)
  OLLAMA_PORT         Ollama port (default: 11434)

Popular Models:
  llama3.3:8b         Meta's latest Llama model (4.7GB)
  codellama:7b        Code-focused model (3.8GB)
  mistral:7b          Fast, efficient model (4.1GB)
  gemma2:9b           Google's Gemma model (5.4GB)
  qwen2.5:7b          Multilingual model (4.4GB)
  phi4:14b            Microsoft's reasoning model (8.4GB)

For more models, visit: https://ollama.ai/library
EOF
}

# Main command dispatcher
main() {
    if [ $# -eq 0 ]; then
        cmd_help
        exit 0
    fi
    
    command="$1"
    shift
    
    case "$command" in
        status|stat)
            cmd_status "$@"
            ;;
        models|list)
            cmd_models "$@"
            ;;
        pull|install)
            cmd_pull "$@"
            ;;
        remove|rm|delete)
            cmd_remove "$@"
            ;;
        chat|run)
            cmd_chat "$@"
            ;;
        test|check)
            cmd_test "$@"
            ;;
        logs|log)
            cmd_logs "$@"
            ;;
        monitor|mon)
            cmd_monitor "$@"
            ;;
        restart)
            cmd_restart "$@"
            ;;
        help|--help|-h)
            cmd_help
            ;;
        *)
            print_error "Unknown command: $command"
            echo "Use '$0 help' for available commands"
            exit 1
            ;;
    esac
}

main "$@"
