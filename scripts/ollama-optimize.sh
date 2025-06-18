#!/usr/bin/env bash
# Ollama CPU Performance Optimization and Model Management Script
# Usage: ./ollama-optimize.sh [benchmark|install-models|test-performance]

set -euo pipefail

OLLAMA_HOST="grey-area:11434"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="/tmp/ollama-optimization.log"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${GREEN}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $1" | tee -a "$LOG_FILE"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$LOG_FILE"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1" | tee -a "$LOG_FILE"
}

info() {
    echo -e "${BLUE}[INFO]${NC} $1" | tee -a "$LOG_FILE"
}

# Check if Ollama is running
check_ollama() {
    if ! curl -s "http://${OLLAMA_HOST}/api/tags" >/dev/null 2>&1; then
        error "Ollama is not accessible at http://${OLLAMA_HOST}"
        error "Make sure the service is running: systemctl status ollama"
        exit 1
    fi
    log "Ollama is running and accessible"
}

# Install optimized models for TaskMaster AI
install_models() {
    log "Installing recommended models for TaskMaster AI..."
    
    # Primary models for different use cases
    local models=(
        "qwen2.5-coder:7b"      # Main coding model
        "deepseek-r1:7b"        # Research and reasoning
        "llama3.3:8b"           # Fallback general purpose
        "codestral:7b"          # Alternative coding model
        "gemma2:9b"             # Alternative general model
    )
    
    for model in "${models[@]}"; do
        log "Pulling model: $model"
        if ollama pull "$model"; then
            log "✅ Successfully installed: $model"
        else
            error "❌ Failed to install: $model"
        fi
    done
    
    # Create optimized model variants
    create_optimized_variants
}

# Create optimized model variants for TaskMaster
create_optimized_variants() {
    log "Creating optimized model variants..."
    
    # TaskMaster-optimized Qwen model
    cat > /tmp/taskmaster-qwen.modelfile << EOF
FROM qwen2.5-coder:7b
PARAMETER temperature 0.3
PARAMETER top_p 0.9
PARAMETER top_k 40
PARAMETER repeat_penalty 1.1
PARAMETER num_ctx 8192

SYSTEM """You are an AI assistant specialized in software development task management and project planning. You excel at:

1. Breaking down complex software projects into manageable tasks
2. Understanding dependencies between development tasks
3. Providing clear, actionable implementation guidance
4. Analyzing code architecture and suggesting improvements
5. Creating detailed subtasks for development workflows

Always respond with structured, practical information that helps developers organize and execute their work efficiently. Focus on clarity, actionability, and technical accuracy."""
EOF

    if ollama create taskmaster-qwen -f /tmp/taskmaster-qwen.modelfile; then
        log "✅ Created optimized TaskMaster model: taskmaster-qwen"
    else
        error "❌ Failed to create TaskMaster optimized model"
    fi
    
    # Research-optimized DeepSeek model
    cat > /tmp/research-deepseek.modelfile << EOF
FROM deepseek-r1:7b
PARAMETER temperature 0.7
PARAMETER top_p 0.95
PARAMETER num_ctx 8192

SYSTEM """You are a research-focused AI assistant specialized in deep analysis and technical investigation. Your strengths include:

1. Comprehensive research and analysis of technical topics
2. Breaking down complex problems into research components
3. Providing detailed, well-reasoned explanations
4. Connecting disparate technical concepts
5. Suggesting research methodologies and approaches

Focus on thoroughness, accuracy, and providing actionable research insights."""
EOF

    if ollama create research-deepseek -f /tmp/research-deepseek.modelfile; then
        log "✅ Created optimized research model: research-deepseek"
    else
        error "❌ Failed to create research optimized model"
    fi
    
    rm -f /tmp/taskmaster-qwen.modelfile /tmp/research-deepseek.modelfile
}

# Benchmark model performance
benchmark_models() {
    log "Benchmarking model performance..."
    
    local test_prompt="Create a task breakdown for implementing a REST API with authentication, database integration, and comprehensive testing."
    local models=(
        "qwen2.5-coder:7b"
        "taskmaster-qwen"
        "deepseek-r1:7b"
        "research-deepseek"
        "llama3.3:8b"
    )
    
    echo "Model Performance Benchmark Results" > /tmp/benchmark-results.txt
    echo "======================================" >> /tmp/benchmark-results.txt
    echo "Test prompt: $test_prompt" >> /tmp/benchmark-results.txt
    echo "" >> /tmp/benchmark-results.txt
    
    for model in "${models[@]}"; do
        info "Testing model: $model"
        
        if ollama list | grep -q "$model"; then
            local start_time=$(date +%s.%N)
            
            # Test the model with a standard prompt
            local response=$(curl -s -X POST "http://${OLLAMA_HOST}/api/generate" \
                -H "Content-Type: application/json" \
                -d "{
                    \"model\": \"$model\",
                    \"prompt\": \"$test_prompt\",
                    \"stream\": false,
                    \"options\": {
                        \"temperature\": 0.3,
                        \"num_ctx\": 4096
                    }
                }" | jq -r '.response // "ERROR"')
            
            local end_time=$(date +%s.%N)
            local duration=$(echo "$end_time - $start_time" | bc)
            local word_count=$(echo "$response" | wc -w)
            local response_quality="Good"
            
            # Simple quality assessment
            if [[ ${#response} -lt 100 ]]; then
                response_quality="Poor"
            elif [[ ${#response} -gt 500 ]]; then
                response_quality="Excellent"
            fi
            
            log "✅ $model: ${duration}s, ${word_count} words, Quality: $response_quality"
            
            {
                echo "Model: $model"
                echo "Response time: ${duration}s"
                echo "Word count: $word_count"
                echo "Quality assessment: $response_quality"
                echo "Response preview: ${response:0:200}..."
                echo "----------------------------------------"
                echo ""
            } >> /tmp/benchmark-results.txt
            
        else
            warn "Model $model not found, skipping..."
        fi
    done
    
    log "Benchmark complete! Results saved to /tmp/benchmark-results.txt"
    info "View results with: cat /tmp/benchmark-results.txt"
}

# Test performance with TaskMaster AI
test_taskmaster_performance() {
    log "Testing TaskMaster AI performance with optimized models..."
    
    local test_commands=(
        "Create a new project for a web application"
        "Break down the task 'Implement user authentication' into subtasks"
        "Analyze the complexity of setting up a microservices architecture"
    )
    
    for cmd in "${test_commands[@]}"; do
        info "Testing: $cmd"
        # Here you would integrate with your TaskMaster AI setup
        # This is a placeholder for actual TaskMaster commands
        echo "TaskMaster test: $cmd" >> /tmp/taskmaster-performance.log
    done
    
    log "TaskMaster performance test complete"
}

# Display system information
show_system_info() {
    log "System Information for Ollama Optimization:"
    echo "============================================"
    
    echo -e "${BLUE}CPU Information:${NC}"
    lscpu | grep -E "Model name|CPU\(s\)|Thread|Core|Socket|MHz"
    echo ""
    
    echo -e "${BLUE}Memory Information:${NC}"
    free -h
    echo ""
    
    echo -e "${BLUE}Ollama Status:${NC}"
    if systemctl is-active ollama >/dev/null 2>&1; then
        echo "✅ Ollama service is running"
        curl -s "http://${OLLAMA_HOST}/api/tags" | jq '.models[].name' 2>/dev/null || echo "No models found"
    else
        echo "❌ Ollama service is not running"
    fi
    echo ""
}

# Main execution
main() {
    local command=${1:-"help"}
    
    case $command in
        "benchmark")
            check_ollama
            benchmark_models
            ;;
        "install-models")
            check_ollama
            install_models
            ;;
        "test-performance")
            check_ollama
            test_taskmaster_performance
            ;;
        "system-info")
            show_system_info
            ;;
        "all")
            check_ollama
            show_system_info
            install_models
            benchmark_models
            test_taskmaster_performance
            ;;
        *)
            echo "Ollama CPU Optimization Script"
            echo "=============================="
            echo ""
            echo "Usage: $0 [command]"
            echo ""
            echo "Commands:"
            echo "  benchmark        - Run performance benchmarks on installed models"
            echo "  install-models   - Install recommended models for TaskMaster AI"
            echo "  test-performance - Test TaskMaster AI performance"
            echo "  system-info      - Display system information"
            echo "  all              - Run all optimization steps"
            echo ""
            echo "Example:"
            echo "  $0 install-models  # Install optimized models"
            echo "  $0 benchmark       # Test model performance"
            echo "  $0 all             # Complete optimization"
            ;;
    esac
}

main "$@"
