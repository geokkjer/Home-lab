#!/usr/bin/env bash
# Update TaskMaster AI model configuration with optimized models
# This script updates the TaskMaster configuration to use the best performing models

set -euo pipefail

TASKMASTER_CONFIG_DIR="/home/geir/Home-lab/.taskmaster"
CONFIG_FILE="$TASKMASTER_CONFIG_DIR/config.json"

log() {
    echo -e "\033[0;32m[$(date +'%H:%M:%S')]\033[0m $1"
}

error() {
    echo -e "\033[0;31m[ERROR]\033[0m $1"
}

# Create backup of current config
backup_config() {
    if [[ -f "$CONFIG_FILE" ]]; then
        cp "$CONFIG_FILE" "$CONFIG_FILE.backup.$(date +%Y%m%d_%H%M%S)"
        log "Created backup of current configuration"
    fi
}

# Update TaskMaster configuration
update_taskmaster_config() {
    log "Updating TaskMaster AI model configuration..."
    
    # Check if TaskMaster is installed and configured
    if [[ ! -d "$TASKMASTER_CONFIG_DIR" ]]; then
        log "Initializing TaskMaster configuration directory..."
        mkdir -p "$TASKMASTER_CONFIG_DIR"
    fi
    
    # Create or update the configuration file
    cat > "$CONFIG_FILE" << EOF
{
  "models": {
    "main": {
      "provider": "openai",
      "model": "qwen2.5-coder:7b",
      "baseUrl": "http://grey-area:11434/v1",
      "description": "Primary model optimized for coding and task management"
    },
    "research": {
      "provider": "openai", 
      "model": "deepseek-r1:7b",
      "baseUrl": "http://grey-area:11434/v1",
      "description": "Enhanced research and reasoning model"
    },
    "fallback": {
      "provider": "openai",
      "model": "llama3.3:8b", 
      "baseUrl": "http://grey-area:11434/v1",
      "description": "Reliable fallback model for general tasks"
    }
  },
  "performance": {
    "contextWindow": 8192,
    "temperature": 0.3,
    "maxTokens": 4096,
    "streamResponses": true
  },
  "ollama": {
    "host": "grey-area",
    "port": 11434,
    "timeout": 60000,
    "retries": 3
  }
}
EOF
    
    log "✅ TaskMaster configuration updated with optimized models"
    log "📍 Configuration file: $CONFIG_FILE"
}

# Verify configuration
verify_config() {
    log "Verifying TaskMaster configuration..."
    
    if [[ -f "$CONFIG_FILE" ]]; then
        if jq . "$CONFIG_FILE" >/dev/null 2>&1; then
            log "✅ Configuration file is valid JSON"
            
            # Display current configuration
            echo ""
            echo "Current TaskMaster Model Configuration:"
            echo "======================================"
            jq -r '.models | to_entries[] | "  \(.key | ascii_upcase): \(.value.model) (\(.value.description))"' "$CONFIG_FILE"
            echo ""
        else
            error "❌ Configuration file contains invalid JSON"
            return 1
        fi
    else
        error "❌ Configuration file not found"
        return 1
    fi
}

# Test connection to Ollama
test_ollama_connection() {
    log "Testing connection to Ollama service..."
    
    local host="grey-area"
    local port="11434"
    
    if curl -s "http://${host}:${port}/api/tags" >/dev/null 2>&1; then
        log "✅ Successfully connected to Ollama at ${host}:${port}"
        
        # List available models
        local models=$(curl -s "http://${host}:${port}/api/tags" | jq -r '.models[].name' 2>/dev/null || echo "")
        if [[ -n "$models" ]]; then
            echo ""
            echo "Available models on Ollama:"
            echo "$models" | sed 's/^/  - /'
            echo ""
        fi
    else
        error "❌ Cannot connect to Ollama at ${host}:${port}"
        error "Make sure Ollama service is running: systemctl status ollama"
        return 1
    fi
}

# Main execution
main() {
    echo "TaskMaster AI Model Configuration Update"
    echo "======================================="
    echo ""
    
    backup_config
    update_taskmaster_config
    verify_config
    test_ollama_connection
    
    echo ""
    log "🎉 TaskMaster AI configuration update complete!"
    echo ""
    echo "Next steps:"
    echo "1. Restart TaskMaster AI service if running"
    echo "2. Test the new configuration with: task-master models"
    echo "3. Run model benchmarks with: ./scripts/ollama-optimize.sh benchmark"
    echo ""
}

main "$@"
