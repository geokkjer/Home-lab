# Ollama Deployment Guide

## Overview

This guide covers the deployment and management of Ollama on the grey-area server in your home lab. Ollama provides local Large Language Model (LLM) hosting with an OpenAI-compatible API.

## Quick Start

### 1. Deploy the Service

The Ollama service is already configured in your NixOS configuration. To deploy:

```bash
# Navigate to your home lab directory
cd /home/geir/Home-lab

# Build and switch to the new configuration
sudo nixos-rebuild switch --flake .#grey-area
```

### 2. Verify Installation

After deployment, verify the service is running:

```bash
# Check service status
systemctl status ollama

# Check if API is responding
curl http://localhost:11434/api/tags

# Run the test script
sudo /etc/ollama-test.sh
```

### 3. Monitor Model Downloads

The service will automatically download the configured models on first start:

```bash
# Monitor the model download process
journalctl -u ollama-model-download -f

# Check downloaded models
ollama list
```

## Configuration Details

### Current Configuration

- **Host**: `127.0.0.1` (localhost only for security)
- **Port**: `11434` (standard Ollama port)
- **Models**: llama3.3:8b, codellama:7b, mistral:7b
- **Memory Limit**: 12GB
- **CPU Limit**: 75%
- **Data Directory**: `/var/lib/ollama`

### Included Models

1. **llama3.3:8b** (~4.7GB)
   - General purpose model
   - Excellent reasoning capabilities
   - Good for general questions and tasks

2. **codellama:7b** (~3.8GB)
   - Code-focused model
   - Great for code review, generation, and explanation
   - Supports multiple programming languages

3. **mistral:7b** (~4.1GB)
   - Fast inference
   - Good balance of speed and quality
   - Efficient for quick queries

## Usage Examples

### Basic API Usage

```bash
# Generate text
curl -X POST http://localhost:11434/api/generate \
  -H "Content-Type: application/json" \
  -d '{
    "model": "llama3.3:8b",
    "prompt": "Explain the benefits of NixOS",
    "stream": false
  }'

# Chat completion (OpenAI compatible)
curl http://localhost:11434/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "llama3.3:8b",
    "messages": [
      {"role": "user", "content": "Help me debug this NixOS configuration"}
    ]
  }'
```

### Interactive Usage

```bash
# Start interactive chat with a model
ollama run llama3.3:8b

# Code assistance
ollama run codellama:7b "Review this function for security issues: $(cat myfile.py)"

# Quick questions
ollama run mistral:7b "What's the difference between systemd services and timers?"
```

### Development Integration

```bash
# Code review in git hooks
echo "#!/bin/bash
git diff HEAD~1 | ollama run codellama:7b 'Review this code diff for issues:'" > .git/hooks/post-commit

# Documentation generation
ollama run llama3.3:8b "Generate documentation for this NixOS module: $(cat module.nix)"
```

## Management Commands

### Service Management

```bash
# Start/stop/restart service
sudo systemctl start ollama
sudo systemctl stop ollama
sudo systemctl restart ollama

# View logs
journalctl -u ollama -f

# Check health
systemctl status ollama-health-check
```

### Model Management

```bash
# List installed models
ollama list

# Download additional models
ollama pull qwen2.5:7b

# Remove models
ollama rm model-name

# Show model information
ollama show llama3.3:8b
```

### Monitoring

```bash
# Check resource usage
systemctl show ollama --property=MemoryCurrent,CPUUsageNSec

# View health check logs
journalctl -u ollama-health-check

# Monitor API requests
tail -f /var/log/ollama.log
```

## Troubleshooting

### Common Issues

#### Service Won't Start
```bash
# Check for configuration errors
journalctl -u ollama --no-pager

# Verify disk space (models are large)
df -h /var/lib/ollama

# Check memory availability
free -h
```

#### Models Not Downloading
```bash
# Check model download service
systemctl status ollama-model-download
journalctl -u ollama-model-download

# Manually download models
sudo -u ollama ollama pull llama3.3:8b
```

#### API Not Responding
```bash
# Check if service is listening
ss -tlnp | grep 11434

# Test API manually
curl -v http://localhost:11434/api/tags

# Check firewall (if accessing externally)
sudo iptables -L | grep 11434
```

#### Out of Memory Errors
```bash
# Check current memory usage
cat /sys/fs/cgroup/system.slice/ollama.service/memory.current

# Reduce resource limits in configuration
# Edit grey-area/services/ollama.nix and reduce maxMemory
```

### Performance Optimization

#### For Better Performance
1. **Add more RAM**: Models perform better with more available memory
2. **Use SSD storage**: Faster model loading from NVMe/SSD
3. **Enable GPU acceleration**: If you have compatible GPU hardware
4. **Adjust context length**: Reduce OLLAMA_CONTEXT_LENGTH for faster responses

#### For Lower Resource Usage
1. **Use smaller models**: Consider 2B or 3B parameter models
2. **Reduce parallel requests**: Set OLLAMA_NUM_PARALLEL to 1
3. **Limit memory**: Reduce maxMemory setting
4. **Use quantized models**: Many models have Q4_0, Q5_0 variants

## Security Considerations

### Current Security Posture
- Service runs as dedicated `ollama` user
- Bound to localhost only (no external access)
- Systemd security hardening enabled
- No authentication (intended for local use)

### Enabling External Access

If you need external access, use a reverse proxy instead of opening the port directly:

```nix
# Add to grey-area configuration
services.nginx = {
  enable = true;
  virtualHosts."ollama.grey-area.lan" = {
    listen = [{ addr = "0.0.0.0"; port = 8080; }];
    locations."/" = {
      proxyPass = "http://127.0.0.1:11434";
      extraConfig = ''
        # Add authentication here if needed
        # auth_basic "Ollama API";
        # auth_basic_user_file /etc/nginx/ollama.htpasswd;
      '';
    };
  };
};
```

## Integration Examples

### With Forgejo
Create a webhook or git hook to review code:

```bash
#!/bin/bash
# .git/hooks/pre-commit
git diff --cached | ollama run codellama:7b "Review this code for issues:"
```

### With Development Workflow
```bash
# Add to shell aliases
alias code-review='git diff | ollama run codellama:7b "Review this code:"'
alias explain-code='ollama run codellama:7b "Explain this code:"'
alias write-docs='ollama run llama3.3:8b "Write documentation for:"'
```

### With Other Services
```bash
# Generate descriptions for Jellyfin media
find /media -name "*.mkv" | while read file; do
  echo "Generating description for $(basename "$file")"
  echo "$(basename "$file" .mkv)" | ollama run llama3.3:8b "Create a brief description for this movie/show:"
done
```

## Backup and Maintenance

### Automatic Backups
- Configuration backup: Included in NixOS configuration
- Model manifests: Backed up weekly to `/var/backup/ollama`
- Model files: Not backed up (re-downloadable)

### Manual Backup
```bash
# Backup custom models or fine-tuned models
sudo tar -czf ollama-custom-$(date +%Y%m%d).tar.gz /var/lib/ollama/

# Backup to remote location
sudo rsync -av /var/lib/ollama/ backup-server:/backups/ollama/
```

### Updates
```bash
# Update Ollama package
sudo nixos-rebuild switch --flake .#grey-area

# Update models (if new versions available)
ollama pull llama3.3:8b
ollama pull codellama:7b
ollama pull mistral:7b
```

## Future Enhancements

### Potential Additions
1. **Web UI**: Deploy Open WebUI for browser-based interaction
2. **Model Management**: Automated model updates and cleanup
3. **Multi-GPU**: Support for multiple GPU acceleration
4. **Custom Models**: Fine-tuning setup for domain-specific models
5. **Metrics**: Prometheus metrics export for monitoring
6. **Load Balancing**: Multiple Ollama instances for high availability

### Scaling Considerations
- **Dedicated Hardware**: Move to dedicated AI server if resource constrained
- **Model Optimization**: Implement model quantization and optimization
- **Caching**: Add Redis caching for frequently requested responses
- **Rate Limiting**: Implement rate limiting for external access

## Support and Resources

### Documentation
- [Ollama Documentation](https://github.com/ollama/ollama)
- [Model Library](https://ollama.ai/library)
- [API Reference](https://github.com/ollama/ollama/blob/main/docs/api.md)

### Community
- [Ollama Discord](https://discord.gg/ollama)
- [GitHub Discussions](https://github.com/ollama/ollama/discussions)

### Local Resources
- Research document: `/home/geir/Home-lab/research/ollama.md`
- Configuration: `/home/geir/Home-lab/machines/grey-area/services/ollama.nix`
- Module: `/home/geir/Home-lab/modules/services/ollama.nix`
