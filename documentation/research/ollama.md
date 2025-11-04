# Ollama on NixOS - Home Lab Research

## Overview

Ollama is a lightweight, open-source tool for running large language models (LLMs) locally. It provides an easy way to get up and running with models like Llama 3.3, Mistral, Codellama, and many others on your local machine.

## Key Features

- **Local LLM Hosting**: Run models entirely on your infrastructure
- **API Compatibility**: OpenAI-compatible API endpoints
- **Model Management**: Easy downloading and switching between models
- **Resource Management**: Automatic memory management and model loading/unloading
- **Multi-modal Support**: Text, code, and vision models
- **Streaming Support**: Real-time response streaming

## Architecture Benefits for Home Lab

### Self-Hosted AI Infrastructure
- **Privacy**: All AI processing happens locally - no data sent to external services
- **Cost Control**: No per-token or per-request charges
- **Always Available**: No dependency on external API availability
- **Customization**: Full control over model selection and configuration

### Integration Opportunities
- **Development Assistance**: Code completion and review for your Forgejo repositories
- **Documentation Generation**: AI-assisted documentation for your infrastructure
- **Chat Interface**: Personal AI assistant for technical questions
- **Automation**: AI-powered automation scripts and infrastructure management

## Resource Requirements

### Minimum Requirements
- **RAM**: 8GB (for smaller models like 7B parameters)
- **Storage**: 4-32GB per model (varies by model size)
- **CPU**: Modern multi-core processor
- **GPU**: Optional but recommended for performance

### Recommended for Home Lab
- **RAM**: 16-32GB for multiple concurrent models
- **Storage**: NVMe SSD for fast model loading
- **GPU**: NVIDIA GPU with 8GB+ VRAM for optimal performance

## Model Categories

### Text Generation Models
- **Llama 3.3** (8B, 70B): General purpose, excellent reasoning
- **Mistral** (7B, 8x7B): Fast inference, good code understanding
- **Gemma 2** (2B, 9B, 27B): Google's efficient models
- **Qwen 2.5** (0.5B-72B): Multilingual, strong coding abilities

### Code-Specific Models
- **Code Llama** (7B, 13B, 34B): Meta's code-focused models
- **DeepSeek Coder** (1.3B-33B): Excellent for programming tasks
- **Starcoder2** (3B, 7B, 15B): Multi-language code generation

### Specialized Models
- **Phi-4** (14B): Microsoft's efficient reasoning model
- **Nous Hermes** (8B, 70B): Fine-tuned for helpful responses
- **OpenChat** (7B): Optimized for conversation

## NixOS Integration

### Native Package Support
```nix
# Ollama is available in nixpkgs
environment.systemPackages = [ pkgs.ollama ];
```

### Systemd Service
- Automatic service management
- User/group isolation
- Environment variable configuration
- Restart policies

### Configuration Management
- Declarative service configuration
- Environment variables via Nix
- Integration with existing infrastructure

## Security Considerations

### Network Security
- Default binding to localhost (127.0.0.1:11434)
- Configurable network binding
- No authentication by default (intended for local use)
- Consider reverse proxy for external access

### Resource Isolation
- Dedicated user/group for service
- Memory and CPU limits via systemd
- File system permissions
- Optional container isolation

### Model Security
- Models downloaded from official sources
- Checksum verification
- Local storage of sensitive prompts/responses

## Performance Optimization

### Hardware Acceleration
- **CUDA**: NVIDIA GPU acceleration
- **ROCm**: AMD GPU acceleration (limited support)
- **Metal**: Apple Silicon acceleration (macOS)
- **OpenCL**: Cross-platform GPU acceleration

### Memory Management
- Automatic model loading/unloading
- Configurable context length
- Memory-mapped model files
- Swap considerations for large models

### Storage Optimization
- Fast SSD storage for model files
- Model quantization for smaller sizes
- Shared model storage across users

## API and Integration

### REST API
```bash
# Generate text
curl -X POST http://localhost:11434/api/generate \
  -H "Content-Type: application/json" \
  -d '{"model": "llama3.3", "prompt": "Why is the sky blue?", "stream": false}'

# List models
curl http://localhost:11434/api/tags

# Model information
curl http://localhost:11434/api/show -d '{"name": "llama3.3"}'
```

### OpenAI Compatible API
```bash
# Chat completion
curl http://localhost:11434/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "llama3.3",
    "messages": [{"role": "user", "content": "Hello!"}]
  }'
```

### Client Libraries
- **Python**: `ollama` package
- **JavaScript**: `ollama` npm package
- **Go**: Native API client
- **Rust**: `ollama-rs` crate

## Deployment Recommendations for Grey Area

### Primary Deployment
Deploy Ollama on `grey-area` alongside your existing services:

**Advantages:**
- Leverages existing application server infrastructure
- Integrates with Forgejo for code assistance
- Shared with media services for content generation
- Centralized management

**Considerations:**
- Resource sharing with Jellyfin and other services
- Potential memory pressure during concurrent usage
- Good for general-purpose AI tasks

### Alternative: Dedicated AI Server
Consider deploying on a dedicated machine if resources become constrained:

**When to Consider:**
- Heavy model usage impacting other services
- Need for GPU acceleration
- Multiple users requiring concurrent access
- Development of AI-focused applications

## Monitoring and Observability

### Metrics to Track
- **Memory Usage**: Model loading and inference memory
- **Response Times**: Model inference latency
- **Request Volume**: API call frequency
- **Model Usage**: Which models are being used
- **Resource Utilization**: CPU/GPU usage during inference

### Integration with Existing Stack
- Prometheus metrics export (if available)
- Log aggregation with existing logging infrastructure
- Health checks for service monitoring
- Integration with Grafana dashboards

## Backup and Disaster Recovery

### What to Backup
- **Model Files**: Large but replaceable from official sources
- **Configuration**: Service configuration and environment
- **Custom Models**: Any fine-tuned or custom models
- **Application Data**: Conversation history if stored

### Backup Strategy
- **Model Files**: Generally don't backup (re-downloadable)
- **Configuration**: Include in NixOS configuration management
- **Custom Content**: Regular backups to NFS storage
- **Documentation**: Model inventory and configuration notes

## Cost-Benefit Analysis

### Benefits
- **Zero Ongoing Costs**: No per-token charges
- **Privacy**: Complete data control
- **Availability**: No external dependencies
- **Customization**: Full control over models and configuration
- **Learning**: Hands-on experience with AI infrastructure

### Costs
- **Hardware**: Additional RAM/storage requirements
- **Power**: Increased energy consumption
- **Maintenance**: Model updates and service management
- **Performance**: May be slower than cloud APIs for large models

## Integration Scenarios

### Development Workflow
```bash
# Code review assistance
echo "Review this function for security issues:" | \
  ollama run codellama:13b

# Documentation generation
echo "Generate documentation for this API:" | \
  ollama run llama3.3:8b
```

### Infrastructure Automation
```bash
# Configuration analysis
echo "Analyze this NixOS configuration for best practices:" | \
  ollama run mistral:7b

# Troubleshooting assistance
echo "Help debug this systemd service issue:" | \
  ollama run llama3.3:8b
```

### Personal Assistant
```bash
# Technical research
echo "Explain the differences between Podman and Docker:" | \
  ollama run llama3.3:8b

# Learning assistance
echo "Teach me about NixOS modules:" | \
  ollama run mistral:7b
```

## Getting Started Recommendations

### Phase 1: Basic Setup
1. Deploy Ollama service on grey-area
2. Install a small general-purpose model (llama3.3:8b)
3. Test basic API functionality
4. Integrate with development workflow

### Phase 2: Expansion
1. Add specialized models (code, reasoning)
2. Set up web interface (if desired)
3. Create automation scripts
4. Monitor resource usage

### Phase 3: Advanced Integration
1. Custom model fine-tuning (if needed)
2. Multi-model workflows
3. Integration with other services
4. External access via reverse proxy

## Conclusion

Ollama provides an excellent opportunity to add AI capabilities to your home lab infrastructure. With NixOS's declarative configuration management, you can easily deploy, configure, and maintain a local AI service that enhances your development workflow while maintaining complete privacy and control.

The integration with your existing grey-area server makes sense for initial deployment, with the flexibility to scale or relocate the service as your AI usage grows.