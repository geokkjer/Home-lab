# Ollama Integration Examples

This document provides practical examples of integrating Ollama into your home lab development workflow.

## Development Workflow Integration

### 1. Git Hooks for Code Review

Create a pre-commit hook that uses Ollama for code review:

```bash
#!/usr/bin/env bash
# .git/hooks/pre-commit

# Check if ollama is available
if ! command -v ollama &> /dev/null; then
    echo "Ollama not available, skipping AI code review"
    exit 0
fi

# Get the diff of staged changes
staged_diff=$(git diff --cached)

if [[ -n "$staged_diff" ]]; then
    echo "🤖 Running AI code review..."
    
    # Use CodeLlama for code review
    review_result=$(echo "$staged_diff" | ollama run codellama:7b "Review this code diff for potential issues, security concerns, and improvements. Be concise:")
    
    if [[ -n "$review_result" ]]; then
        echo "AI Code Review Results:"
        echo "======================="
        echo "$review_result"
        echo
        
        read -p "Continue with commit? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Commit aborted by user"
            exit 1
        fi
    fi
fi
```

### 2. Documentation Generation

Create a script to generate documentation for your NixOS modules:

```bash
#!/usr/bin/env bash
# scripts/generate-docs.sh

module_file="$1"
if [[ ! -f "$module_file" ]]; then
    echo "Usage: $0 <nix-module-file>"
    exit 1
fi

echo "Generating documentation for $module_file..."

# Read the module content
module_content=$(cat "$module_file")

# Generate documentation using Ollama
documentation=$(echo "$module_content" | ollama run llama3.3:8b "Generate comprehensive documentation for this NixOS module. Include:
1. Overview and purpose
2. Configuration options
3. Usage examples
4. Security considerations
5. Troubleshooting tips

Module content:")

# Save to documentation file
doc_file="${module_file%.nix}.md"
echo "$documentation" > "$doc_file"

echo "Documentation saved to: $doc_file"
```

### 3. Configuration Analysis

Analyze your NixOS configurations for best practices:

```bash
#!/usr/bin/env bash
# scripts/analyze-config.sh

config_file="$1"
if [[ ! -f "$config_file" ]]; then
    echo "Usage: $0 <configuration.nix>"
    exit 1
fi

echo "Analyzing NixOS configuration: $config_file"

config_content=$(cat "$config_file")

analysis=$(echo "$config_content" | ollama run mistral:7b "Analyze this NixOS configuration for:
1. Security best practices
2. Performance optimizations
3. Potential issues
4. Recommended improvements
5. Missing common configurations

Configuration:")

echo "Configuration Analysis"
echo "====================="
echo "$analysis"
```

## Service Integration Examples

### 1. Forgejo Integration

Create webhooks in Forgejo that trigger AI-powered code reviews:

```bash
#!/usr/bin/env bash
# scripts/forgejo-webhook-handler.sh

# Webhook handler for Forgejo push events
# Place this in your web server and configure Forgejo to call it

payload=$(cat)
branch=$(echo "$payload" | jq -r '.ref | split("/") | last')
repo=$(echo "$payload" | jq -r '.repository.name')

if [[ "$branch" == "main" || "$branch" == "master" ]]; then
    echo "Analyzing push to $repo:$branch"
    
    # Get the commit diff
    commit_sha=$(echo "$payload" | jq -r '.after')
    
    # Fetch the diff (you'd need to implement this based on your Forgejo API)
    diff_content=$(get_commit_diff "$repo" "$commit_sha")
    
    # Analyze with Ollama
    analysis=$(echo "$diff_content" | ollama run codellama:7b "Analyze this commit for potential issues:")
    
    # Post results back to Forgejo (implement based on your needs)
    post_comment_to_commit "$repo" "$commit_sha" "$analysis"
fi
```

### 2. System Monitoring Integration

Enhance your monitoring with AI-powered log analysis:

```bash
#!/usr/bin/env bash
# scripts/ai-log-analyzer.sh

service="$1"
if [[ -z "$service" ]]; then
    echo "Usage: $0 <service-name>"
    exit 1
fi

echo "Analyzing logs for service: $service"

# Get recent logs
logs=$(journalctl -u "$service" --since "1 hour ago" --no-pager)

if [[ -n "$logs" ]]; then
    analysis=$(echo "$logs" | ollama run llama3.3:8b "Analyze these system logs for:
1. Error patterns
2. Performance issues
3. Security concerns
4. Recommended actions

Logs:")

    echo "AI Log Analysis for $service"
    echo "============================"
    echo "$analysis"
else
    echo "No recent logs found for $service"
fi
```

## Home Assistant Integration (if deployed)

### 1. Smart Home Automation

If you deploy Home Assistant on grey-area, integrate it with Ollama:

```yaml
# configuration.yaml for Home Assistant
automation:
  - alias: "AI System Health Report"
    trigger:
      platform: time
      at: "09:00:00"
    action:
      - service: shell_command.generate_health_report
      - service: notify.telegram  # or your preferred notification service
        data:
          title: "Daily System Health Report"
          message: "{{ states('sensor.ai_health_report') }}"

shell_command:
  generate_health_report: "/home/geir/Home-lab/scripts/ai-health-report.sh"
```

```bash
#!/usr/bin/env bash
# scripts/ai-health-report.sh

# Collect system metrics
uptime_info=$(uptime)
disk_usage=$(df -h / | tail -1)
memory_usage=$(free -h | grep Mem)
load_avg=$(cat /proc/loadavg)

# Service statuses
ollama_status=$(systemctl is-active ollama)
jellyfin_status=$(systemctl is-active jellyfin)
forgejo_status=$(systemctl is-active forgejo)

# Generate AI summary
report=$(cat << EOF | ollama run mistral:7b "Summarize this system health data and provide recommendations:"
System Uptime: $uptime_info
Disk Usage: $disk_usage
Memory Usage: $memory_usage
Load Average: $load_avg

Service Status:
- Ollama: $ollama_status
- Jellyfin: $jellyfin_status
- Forgejo: $forgejo_status
EOF
)

echo "$report" > /tmp/health_report.txt
echo "$report"
```

## Development Tools Integration

### 1. VS Code/Editor Integration

Create editor snippets that use Ollama for code generation:

```bash
#!/usr/bin/env bash
# scripts/code-assistant.sh

action="$1"
input_file="$2"

case "$action" in
    "explain")
        code_content=$(cat "$input_file")
        ollama run codellama:7b "Explain this code in detail:" <<< "$code_content"
        ;;
    "optimize")
        code_content=$(cat "$input_file")
        ollama run codellama:7b "Suggest optimizations for this code:" <<< "$code_content"
        ;;
    "test")
        code_content=$(cat "$input_file")
        ollama run codellama:7b "Generate unit tests for this code:" <<< "$code_content"
        ;;
    "document")
        code_content=$(cat "$input_file")
        ollama run llama3.3:8b "Generate documentation comments for this code:" <<< "$code_content"
        ;;
    *)
        echo "Usage: $0 {explain|optimize|test|document} <file>"
        exit 1
        ;;
esac
```

### 2. Terminal Integration

Add shell functions for quick AI assistance:

```bash
# Add to your .zshrc or .bashrc

# AI-powered command explanation
explain() {
    if [[ -z "$1" ]]; then
        echo "Usage: explain <command>"
        return 1
    fi
    
    echo "Explaining command: $*"
    echo "$*" | ollama run llama3.3:8b "Explain this command in detail, including options and use cases:"
}

# AI-powered error debugging
debug() {
    if [[ -z "$1" ]]; then
        echo "Usage: debug <error_message>"
        return 1
    fi
    
    echo "Debugging: $*"
    echo "$*" | ollama run llama3.3:8b "Help debug this error message and suggest solutions:"
}

# Quick code review
review() {
    if [[ -z "$1" ]]; then
        echo "Usage: review <file>"
        return 1
    fi
    
    if [[ ! -f "$1" ]]; then
        echo "File not found: $1"
        return 1
    fi
    
    echo "Reviewing file: $1"
    cat "$1" | ollama run codellama:7b "Review this code for potential issues and improvements:"
}

# Generate commit messages
gitmsg() {
    diff_content=$(git diff --cached)
    if [[ -z "$diff_content" ]]; then
        echo "No staged changes found"
        return 1
    fi
    
    echo "Generating commit message..."
    message=$(echo "$diff_content" | ollama run mistral:7b "Generate a concise commit message for these changes:")
    echo "Suggested commit message:"
    echo "$message"
    
    read -p "Use this message? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        git commit -m "$message"
    fi
}
```

## API Integration Examples

### 1. Monitoring Dashboard

Create a simple web dashboard that shows AI-powered insights:

```python
#!/usr/bin/env python3
# scripts/ai-dashboard.py

import requests
import json
from datetime import datetime
import subprocess

OLLAMA_URL = "http://localhost:11434"

def get_system_metrics():
    """Collect system metrics"""
    uptime = subprocess.check_output(['uptime'], text=True).strip()
    df = subprocess.check_output(['df', '-h', '/'], text=True).split('\n')[1]
    memory = subprocess.check_output(['free', '-h'], text=True).split('\n')[1]
    
    return {
        'timestamp': datetime.now().isoformat(),
        'uptime': uptime,
        'disk': df,
        'memory': memory
    }

def analyze_metrics_with_ai(metrics):
    """Use Ollama to analyze system metrics"""
    prompt = f"""
    Analyze these system metrics and provide insights:
    
    Timestamp: {metrics['timestamp']}
    Uptime: {metrics['uptime']}
    Disk: {metrics['disk']}
    Memory: {metrics['memory']}
    
    Provide a brief summary and any recommendations.
    """
    
    response = requests.post(f"{OLLAMA_URL}/api/generate", json={
        "model": "mistral:7b",
        "prompt": prompt,
        "stream": False
    })
    
    if response.status_code == 200:
        return response.json().get('response', 'No analysis available')
    else:
        return "AI analysis unavailable"

def main():
    print("System Health Dashboard")
    print("=" * 50)
    
    metrics = get_system_metrics()
    analysis = analyze_metrics_with_ai(metrics)
    
    print(f"Timestamp: {metrics['timestamp']}")
    print(f"Uptime: {metrics['uptime']}")
    print(f"Disk: {metrics['disk']}")
    print(f"Memory: {metrics['memory']}")
    print()
    print("AI Analysis:")
    print("-" * 20)
    print(analysis)

if __name__ == "__main__":
    main()
```

### 2. Slack/Discord Bot Integration

Create a bot that provides AI assistance in your communication channels:

```python
#!/usr/bin/env python3
# scripts/ai-bot.py

import requests
import json

def ask_ollama(question, model="llama3.3:8b"):
    """Send question to Ollama and get response"""
    response = requests.post("http://localhost:11434/api/generate", json={
        "model": model,
        "prompt": question,
        "stream": False
    })
    
    if response.status_code == 200:
        return response.json().get('response', 'No response available')
    else:
        return "AI service unavailable"

# Example usage in a Discord bot
# @bot.command()
# async def ask(ctx, *, question):
#     response = ask_ollama(question)
#     await ctx.send(f"🤖 AI Response: {response}")

# Example usage in a Slack bot
# @app.command("/ask")
# def handle_ask_command(ack, respond, command):
#     ack()
#     question = command['text']
#     response = ask_ollama(question)
#     respond(f"🤖 AI Response: {response}")
```

## Performance Tips

### 1. Model Selection Based on Task

```bash
# Use appropriate models for different tasks
alias code-review='ollama run codellama:7b'
alias quick-question='ollama run mistral:7b'
alias detailed-analysis='ollama run llama3.3:8b'
alias general-chat='ollama run llama3.3:8b'
```

### 2. Batch Processing

```bash
#!/usr/bin/env bash
# scripts/batch-analysis.sh

# Process multiple files efficiently
files=("$@")

for file in "${files[@]}"; do
    if [[ -f "$file" ]]; then
        echo "Processing: $file"
        cat "$file" | ollama run codellama:7b "Briefly review this code:" > "${file}.review"
    fi
done

echo "Batch processing complete. Check .review files for results."
```

These examples demonstrate practical ways to integrate Ollama into your daily development workflow, home lab management, and automation tasks. Start with simple integrations and gradually build more sophisticated automations based on your needs.
