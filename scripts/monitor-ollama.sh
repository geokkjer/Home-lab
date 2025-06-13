#!/usr/bin/env bash
# Ollama Monitoring Script
# Provides comprehensive monitoring of Ollama service health and performance

set -euo pipefail

# Configuration
OLLAMA_HOST="${OLLAMA_HOST:-127.0.0.1}"
OLLAMA_PORT="${OLLAMA_PORT:-11434}"
OLLAMA_URL="http://${OLLAMA_HOST}:${OLLAMA_PORT}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Functions
print_header() {
    echo -e "${BLUE}=== $1 ===${NC}"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

check_service_status() {
    print_header "Service Status"
    
    if systemctl is-active --quiet ollama; then
        print_success "Ollama service is running"
        
        # Get service uptime
        started=$(systemctl show ollama --property=ActiveEnterTimestamp --value)
        if [[ -n "$started" ]]; then
            echo "  Started: $started"
        fi
        
        # Get service memory usage
        memory=$(systemctl show ollama --property=MemoryCurrent --value)
        if [[ "$memory" != "[not set]" ]] && [[ -n "$memory" ]]; then
            memory_mb=$((memory / 1024 / 1024))
            echo "  Memory usage: ${memory_mb}MB"
        fi
        
    else
        print_error "Ollama service is not running"
        echo "  Try: sudo systemctl start ollama"
        return 1
    fi
}

check_api_connectivity() {
    print_header "API Connectivity"
    
    if curl -s --connect-timeout 5 "$OLLAMA_URL/api/tags" >/dev/null; then
        print_success "API is responding"
        
        # Get API version if available
        version=$(curl -s "$OLLAMA_URL/api/version" 2>/dev/null | jq -r '.version // "unknown"' 2>/dev/null || echo "unknown")
        if [[ "$version" != "unknown" ]]; then
            echo "  Version: $version"
        fi
    else
        print_error "API is not responding"
        echo "  URL: $OLLAMA_URL"
        return 1
    fi
}

check_models() {
    print_header "Installed Models"
    
    models_json=$(curl -s "$OLLAMA_URL/api/tags" 2>/dev/null)
    if [[ $? -eq 0 ]] && [[ -n "$models_json" ]]; then
        model_count=$(echo "$models_json" | jq '.models | length' 2>/dev/null || echo "0")
        
        if [[ "$model_count" -gt 0 ]]; then
            print_success "$model_count models installed"
            
            echo "$models_json" | jq -r '.models[]? | "  \(.name) (\(.size | . / 1024 / 1024 / 1024 | floor)GB) - Modified: \(.modified_at)"' 2>/dev/null || {
                echo "$models_json" | jq -r '.models[]?.name // "Unknown model"' 2>/dev/null | sed 's/^/  /'
            }
        else
            print_warning "No models installed"
            echo "  Try: ollama pull llama3.3:8b"
        fi
    else
        print_error "Could not retrieve model list"
        return 1
    fi
}

check_disk_space() {
    print_header "Disk Space"
    
    ollama_dir="/var/lib/ollama"
    if [[ -d "$ollama_dir" ]]; then
        # Get disk usage for ollama directory
        usage=$(du -sh "$ollama_dir" 2>/dev/null | cut -f1 || echo "unknown")
        available=$(df -h "$ollama_dir" | tail -1 | awk '{print $4}' || echo "unknown")
        
        echo "  Ollama data usage: $usage"
        echo "  Available space: $available"
        
        # Check if we're running low on space
        available_bytes=$(df "$ollama_dir" | tail -1 | awk '{print $4}' || echo "0")
        if [[ "$available_bytes" -lt 10485760 ]]; then # Less than 10GB
            print_warning "Low disk space (less than 10GB available)"
        else
            print_success "Sufficient disk space available"
        fi
    else
        print_warning "Ollama data directory not found: $ollama_dir"
    fi
}

check_model_downloads() {
    print_header "Model Download Status"
    
    if systemctl is-active --quiet ollama-model-download; then
        print_warning "Model download in progress"
        echo "  Check progress: journalctl -u ollama-model-download -f"
    elif systemctl is-enabled --quiet ollama-model-download; then
        if systemctl show ollama-model-download --property=Result --value | grep -q "success"; then
            print_success "Model downloads completed successfully"
        else
            result=$(systemctl show ollama-model-download --property=Result --value)
            print_warning "Model download service result: $result"
            echo "  Check logs: journalctl -u ollama-model-download"
        fi
    else
        print_warning "Model download service not enabled"
    fi
}

check_health_monitoring() {
    print_header "Health Monitoring"
    
    if systemctl is-enabled --quiet ollama-health-check; then
        last_run=$(systemctl show ollama-health-check --property=LastTriggerUSec --value)
        if [[ "$last_run" != "n/a" ]] && [[ -n "$last_run" ]]; then
            last_run_human=$(date -d "@$((last_run / 1000000))" 2>/dev/null || echo "unknown")
            echo "  Last health check: $last_run_human"
        fi
        
        if systemctl show ollama-health-check --property=Result --value | grep -q "success"; then
            print_success "Health checks passing"
        else
            result=$(systemctl show ollama-health-check --property=Result --value)
            print_warning "Health check result: $result"
        fi
    else
        print_warning "Health monitoring not enabled"
    fi
}

test_inference() {
    print_header "Inference Test"
    
    # Get first available model
    first_model=$(curl -s "$OLLAMA_URL/api/tags" 2>/dev/null | jq -r '.models[0].name // empty' 2>/dev/null)
    
    if [[ -n "$first_model" ]]; then
        echo "  Testing with model: $first_model"
        
        start_time=$(date +%s.%N)
        response=$(curl -s -X POST "$OLLAMA_URL/api/generate" \
            -H "Content-Type: application/json" \
            -d "{\"model\": \"$first_model\", \"prompt\": \"Hello\", \"stream\": false}" \
            2>/dev/null | jq -r '.response // empty' 2>/dev/null)
        end_time=$(date +%s.%N)
        
        if [[ -n "$response" ]]; then
            duration=$(echo "$end_time - $start_time" | bc 2>/dev/null || echo "unknown")
            print_success "Inference test successful"
            echo "  Response time: ${duration}s"
            echo "  Response: ${response:0:100}${response:100:1:+...}"
        else
            print_error "Inference test failed"
            echo "  Try: ollama run $first_model 'Hello'"
        fi
    else
        print_warning "No models available for testing"
    fi
}

show_recent_logs() {
    print_header "Recent Logs (last 10 lines)"
    
    echo "Service logs:"
    journalctl -u ollama --no-pager -n 5 --output=short-iso | sed 's/^/  /'
    
    if [[ -f "/var/log/ollama.log" ]]; then
        echo "Application logs:"
        tail -5 /var/log/ollama.log 2>/dev/null | sed 's/^/  /' || echo "  No application logs found"
    fi
}

show_performance_stats() {
    print_header "Performance Statistics"
    
    # CPU usage (if available)
    if command -v top >/dev/null; then
        cpu_usage=$(top -b -n1 -p "$(pgrep ollama || echo 1)" 2>/dev/null | tail -1 | awk '{print $9}' || echo "unknown")
        echo "  CPU usage: ${cpu_usage}%"
    fi
    
    # Memory usage details
    if [[ -f "/sys/fs/cgroup/system.slice/ollama.service/memory.current" ]]; then
        memory_current=$(cat /sys/fs/cgroup/system.slice/ollama.service/memory.current)
        memory_mb=$((memory_current / 1024 / 1024))
        echo "  Memory usage: ${memory_mb}MB"
        
        if [[ -f "/sys/fs/cgroup/system.slice/ollama.service/memory.max" ]]; then
            memory_max=$(cat /sys/fs/cgroup/system.slice/ollama.service/memory.max)
            if [[ "$memory_max" != "max" ]]; then
                memory_max_mb=$((memory_max / 1024 / 1024))
                usage_percent=$(( (memory_current * 100) / memory_max ))
                echo "  Memory limit: ${memory_max_mb}MB (${usage_percent}% used)"
            fi
        fi
    fi
    
    # Load average
    if [[ -f "/proc/loadavg" ]]; then
        load_avg=$(cat /proc/loadavg | cut -d' ' -f1-3)
        echo "  System load: $load_avg"
    fi
}

# Main execution
main() {
    echo -e "${BLUE}Ollama Service Monitor${NC}"
    echo "Timestamp: $(date)"
    echo "Host: ${OLLAMA_HOST}:${OLLAMA_PORT}"
    echo

    # Run all checks
    check_service_status || exit 1
    echo
    
    check_api_connectivity || exit 1
    echo
    
    check_models
    echo
    
    check_disk_space
    echo
    
    check_model_downloads
    echo
    
    check_health_monitoring
    echo
    
    check_performance_stats
    echo
    
    # Only run inference test if requested
    if [[ "${1:-}" == "--test-inference" ]]; then
        test_inference
        echo
    fi
    
    # Only show logs if requested
    if [[ "${1:-}" == "--show-logs" ]] || [[ "${2:-}" == "--show-logs" ]]; then
        show_recent_logs
        echo
    fi
    
    print_success "Monitoring complete"
}

# Help function
show_help() {
    echo "Ollama Service Monitor"
    echo
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  --test-inference    Run a simple inference test"
    echo "  --show-logs        Show recent service logs"
    echo "  --help             Show this help message"
    echo
    echo "Environment variables:"
    echo "  OLLAMA_HOST        Ollama host (default: 127.0.0.1)"
    echo "  OLLAMA_PORT        Ollama port (default: 11434)"
    echo
    echo "Examples:"
    echo "  $0                          # Basic monitoring"
    echo "  $0 --test-inference         # Include inference test"
    echo "  $0 --show-logs              # Include recent logs"
    echo "  $0 --test-inference --show-logs  # Full monitoring"
}

# Handle command line arguments
case "${1:-}" in
    --help|-h)
        show_help
        exit 0
        ;;
    *)
        main "$@"
        ;;
esac
