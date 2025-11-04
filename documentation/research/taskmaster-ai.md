# Task Master + Ollama Research & Integration Plan

## Project Overview

**Task Master** (https://www.task-master.dev/) is an AI-powered task management system that can be enhanced with local AI capabilities through Ollama integration for intelligent task breakdown, prioritization, and execution tracking while maintaining complete data privacy.

### Key Features Analysis

#### Core Capabilities
- **Intelligent Task Breakdown**: Automatically decomposes complex projects into manageable subtasks
- **Context-Aware Planning**: Uses AI to understand project requirements and dependencies
- **Progress Tracking**: Monitors task completion and adjusts plans dynamically
- **Natural Language Interface**: Allows task management through conversational commands
- **Integration Ready**: Designed to work with existing development workflows

#### Technical Architecture
- **Backend**: Node.js/Python-based task orchestration
- **AI Integration**: Ollama API for local task analysis and planning
- **Storage**: JSON/Database for task persistence
- **API**: RESTful endpoints for external integrations

## Workflow Compatibility Assessment

### Current Home-lab Methodology Alignment

#### ✅ Strong Fits
1. **Infrastructure-as-Code Philosophy**
   - Task Master's structured approach aligns with your NixOS configuration management
   - Can track infrastructure changes as tasks with dependencies

2. **Service-Oriented Architecture**
   - Fits well with your microservices approach (Transmission, monitoring, etc.)
   - Can manage service deployment and configuration tasks

3. **Documentation-Driven Development**
   - Integrates with your markdown-based documentation workflow
   - Can auto-generate task documentation and progress reports

#### ⚠️ Considerations
1. **Resource Overhead**
   - Additional service to manage in your infrastructure
   - Local AI processing requirements on grey-area

2. **Hardware Requirements**
   - Need sufficient RAM/CPU for Ollama models
   - Storage requirements for model files

## Integration Strategy

### Phase 1: Core Installation & Setup

#### Prerequisites
```bash
# Dependencies for Home-lab integration
- Node.js runtime environment
- Ollama service on grey-area
- Docker/Podman for containerization
- NixOS service configuration
```

#### Installation Plan
1. **Clone and Setup**
   ```bash
   cd /home/geir/Home-lab/services
   git clone https://github.com/task-master-dev/task-master.git taskmaster
   cd taskmaster
   ```

2. **NixOS Service Configuration**
   - Create `taskmaster.nix` service definition
   - Configure Ollama endpoint and environment variables
   - Set up reverse proxy through existing nginx setup

3. **Environment Configuration**
   ```env
   OLLAMA_API_URL=http://grey-area:11434/api
   OLLAMA_MODEL=llama3.2:8b
   TASKMASTER_PORT=3001
   DATABASE_URL=sqlite:///mnt/storage/taskmaster.db
   ```

### Phase 2: GitHub Copilot Integration

#### Integration Points
1. **Code Task Generation**
   - Use Copilot to generate coding tasks from repository analysis
   - Automatic task creation from GitHub issues and PRs

2. **Development Workflow Enhancement**
   ```typescript
   // Example integration hook
   interface CopilotTaskBridge {
     generateTasksFromCode(filePath: string): Task[];
     updateTaskProgress(taskId: string, codeChanges: CodeDiff[]): void;
     suggestNextSteps(currentTask: Task): Suggestion[];
   }
   ```

3. **VS Code Extension Development**
   - Custom extension to bridge Copilot suggestions with Task Master
   - Real-time task updates based on code changes

### Phase 3: Context7 MCP Integration

#### Model Context Protocol Benefits
1. **Unified Context Management**
   - Task Master tasks as context for Ollama conversations
   - Project state awareness across all AI interactions

2. **Cross-Service Communication**
   ```json
   {
     "mcp_config": {
       "services": {
         "taskmaster": {
           "endpoint": "http://sleeper-service:3001/api",
           "capabilities": ["task_management", "progress_tracking"]
         },
         "github_copilot": {
           "integration": "vscode_extension",
           "context_sharing": true
         }
       }
     }
   }
   ```

3. **Context Flow Architecture**
   ```
   GitHub Copilot → Context7 MCP → Task Master → Ollama API
        ↑                                           ↓
   VS Code Editor ←─────── Task Updates ←─────── AI Insights
   ```

## Implementation Roadmap

### Week 1: Foundation
- [ ] Set up Task Master on sleeper-service
- [ ] Configure basic NixOS service
- [ ] Set up Ollama on grey-area
- [ ] Create initial task templates for Home-lab projects

### Week 2: GitHub Integration
- [ ] Develop Copilot bridge extension
- [ ] Set up GitHub webhook integration
- [ ] Create automated task generation from repository events
- [ ] Test code-to-task mapping

### Week 3: MCP Integration
- [ ] Implement Context7 MCP protocol support
- [ ] Create unified context sharing system
- [ ] Develop cross-service communication layer
- [ ] Test end-to-end workflow

### Week 4: Optimization & Documentation
- [ ] Performance tuning and monitoring
- [ ] Complete integration documentation
- [ ] Create user workflow guides
- [ ] Set up backup and recovery procedures

## NixOS Service Configuration Preview

```nix
# /home/geir/Home-lab/machines/sleeper-service/services/taskmaster.nix
{ config, pkgs, ... }:

{
  services.taskmaster = {
    enable = true;
    port = 3001;
    user = "sma";
    group = "users";
    environmentFile = "/etc/taskmaster/env";
    dataDir = "/mnt/storage/taskmaster";
  };

  # Nginx reverse proxy configuration
  services.nginx.virtualHosts."taskmaster.home-lab" = {
    locations."/" = {
      proxyPass = "http://localhost:3001";
      proxyWebsockets = true;
    };
  };

  # Firewall configuration
  networking.firewall.allowedTCPPorts = [ 3001 ];
}
```

## Benefits for Home-lab Workflow

### Immediate Improvements
1. **Project Visibility**: Clear overview of all infrastructure tasks and their status
2. **Dependency Management**: Automatic tracking of service dependencies and update sequences
3. **Documentation Automation**: AI-generated task documentation and progress reports
4. **Workflow Optimization**: Intelligent task prioritization based on system state

### Long-term Value
1. **Knowledge Retention**: Comprehensive history of infrastructure decisions and changes
2. **Onboarding**: New team members can quickly understand project structure through task history
3. **Compliance**: Automated tracking for security updates and maintenance tasks
4. **Scalability**: Framework for managing larger infrastructure deployments

## Risk Assessment & Mitigation

### Technical Risks
- **Hardware Limitations**: Grey-area may not have sufficient resources for larger models
- **Data Loss**: Regular backups to /mnt/storage/backups
- **Performance Impact**: Resource monitoring and limits

### Security Considerations
- **Network Isolation**: Local Ollama API access only from sleeper-service
- **Firewall Rules**: Restrict Ollama API access to authorized services
- **Data Encryption**: Encrypt sensitive task data at rest

## Ollama Self-Hosted AI Integration

### Ollama on Grey-Area Research

**Ollama** provides a lightweight, self-hosted alternative to external AI APIs that can run on your grey-area server, offering complete data privacy and control while eliminating external API dependencies.

#### Grey-Area Hardware Assessment

##### Current Specifications
- **CPU**: Check grey-area specs for AI workload capability
- **RAM**: Minimum 8GB for smaller models, 16GB+ recommended for larger models
- **Storage**: SSD recommended for model storage and fast inference
- **GPU**: NVIDIA GPU would significantly improve performance (if available)

##### Model Size Considerations
```bash
# Recommended models for task management:
# Lightweight options (4-8GB RAM):
- llama3.2:3b (2GB model size)
- gemma2:2b (1.5GB model size)
- phi3:3.8b (2.3GB model size)

# Medium performance (8-16GB RAM):
- llama3.2:8b (4.7GB model size)
- gemma2:9b (5.4GB model size)
- mistral:7b (4.1GB model size)

# High performance (16GB+ RAM):
- llama3.1:70b (40GB model size) - if grey-area has sufficient resources
```

#### Context7 MCP + Ollama Architecture

##### Integration Benefits
1. **Complete Data Privacy**: All AI processing stays within your infrastructure
2. **No API Rate Limits**: Unlimited local inference capacity
3. **Cost Efficiency**: No per-token costs after initial setup
4. **Low Latency**: Local processing eliminates network round-trips
5. **Customization**: Fine-tune models for specific task management patterns

##### Technical Stack
```
VS Code + GitHub Copilot
         ↓
Context7 MCP Server (sleeper-service)
         ↓
Task Master (sleeper-service) ←→ Ollama API (grey-area:11434)
         ↓
Local Model Inference (grey-area)
```

#### NixOS Ollama Configuration for Grey-Area

##### Service Definition
```nix
# /home/geir/Home-lab/machines/grey-area/services/ollama.nix
{ config, pkgs, ... }:

{
  services.ollama = {
    enable = true;
    acceleration = "cuda"; # or "rocm" if AMD GPU, omit if CPU-only
    environmentVariables = {
      OLLAMA_HOST = "0.0.0.0:11434";
      OLLAMA_MODELS = "/mnt/storage/ollama/models";
    };
    openFirewall = true;
  };

  # Ensure sufficient resources
  systemd.services.ollama = {
    serviceConfig = {
      MemoryMax = "16G"; # Adjust based on available RAM
      CPUQuota = "400%"; # Use 4 CPU cores max
    };
  };

  # Storage configuration
  fileSystems."/mnt/storage/ollama" = {
    device = "/dev/disk/by-label/storage";
    fsType = "ext4";
    options = [ "defaults" "noatime" ];
  };
}
```

##### Model Management Script
```bash
#!/usr/bin/env bash
# /home/geir/Home-lab/scripts/ollama-setup.sh

# Pull recommended models for task management
ollama pull llama3.2:8b
ollama pull gemma2:9b
ollama pull mistral:7b

# Create custom modelfile for task management optimization
cat > /tmp/taskmaster-model << EOF
FROM llama3.2:8b
PARAMETER temperature 0.7
PARAMETER top_p 0.9
SYSTEM """You are an AI assistant specialized in project task management and software development workflows. You excel at breaking down complex projects into manageable tasks, understanding dependencies, and providing clear, actionable guidance. Always respond with structured, practical information."""
EOF

ollama create taskmaster-optimized -f /tmp/taskmaster-model
```

#### Modified Task Master Integration

##### Environment Configuration for Ollama
```env
# Replace Claude API with Ollama endpoint
AI_PROVIDER=ollama
OLLAMA_API_URL=http://grey-area:11434/api
OLLAMA_MODEL=taskmaster-optimized
# Fallback models
OLLAMA_FALLBACK_MODELS=llama3.2:8b,gemma2:9b,mistral:7b
```

##### API Adapter Layer
```typescript
// services/ai-provider.ts
interface AIProvider {
  generateResponse(prompt: string, context?: any): Promise<string>;
  analyzeTask(taskDescription: string): Promise<TaskAnalysis>;
  suggestSubtasks(project: Project): Promise<Subtask[]>;
}

class OllamaProvider implements AIProvider {
  private baseUrl = process.env.OLLAMA_API_URL;
  private model = process.env.OLLAMA_MODEL || 'llama3.2:8b';

  async generateResponse(prompt: string, context?: any): Promise<string> {
    const response = await fetch(`${this.baseUrl}/generate`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        model: this.model,
        prompt,
        context,
        stream: false
      })
    });
    return response.json().then(data => data.response);
  }

  // ...existing code...
}
```

#### Context7 MCP Integration for Ollama

##### MCP Server Configuration
```json
{
  "mcp_config": {
    "version": "1.0",
    "servers": {
      "taskmaster": {
        "endpoint": "http://sleeper-service:3001/mcp",
        "capabilities": ["task_management", "project_context"]
      },
      "ollama": {
        "endpoint": "http://grey-area:11434/v1",
        "type": "openai_compatible",
        "model": "taskmaster-optimized",
        "capabilities": ["text_generation", "task_analysis"]
      }
    },
    "context_routing": {
      "task_operations": "taskmaster",
      "ai_inference": "ollama",
      "code_analysis": "github_copilot"
    }
  }
}
```

##### Context Flow with Ollama
```
GitHub Copilot (VS Code)
         ↓
Context7 MCP Server (sleeper-service)
         ↓ (project context + task history)
Task Master API (sleeper-service)
         ↓ (AI inference requests)
Ollama API (grey-area:11434)
         ↓
Local Model Processing (grey-area)
```

#### Performance Optimization Strategies

##### Model Selection Matrix
| Use Case | Model | RAM Req | Response Time | Quality |
|----------|-------|---------|---------------|---------|
| Quick tasks | phi3:3.8b | 4GB | <2s | Good |
| Standard workflow | llama3.2:8b | 8GB | <5s | Excellent |
| Complex analysis | gemma2:9b | 12GB | <10s | Excellent |
| Heavy workloads | mistral:7b | 8GB | <7s | Very Good |

##### Caching Strategy
```typescript
// Implement response caching for common queries
class OllamaCache {
  private cache = new Map<string, CachedResponse>();
  
  async getCachedResponse(prompt: string): Promise<string | null> {
    const hash = this.hashPrompt(prompt);
    const cached = this.cache.get(hash);
    
    if (cached && !this.isExpired(cached)) {
      return cached.response;
    }
    return null;
  }
  
  setCachedResponse(prompt: string, response: string): void {
    const hash = this.hashPrompt(prompt);
    this.cache.set(hash, {
      response,
      timestamp: Date.now(),
      ttl: 300000 // 5 minutes
    });
  }
}
```

#### Deployment Strategy Comparison

##### Ollama vs External APIs Comparison
| Aspect | Ollama (Grey-Area) | External APIs (Claude/OpenAI) |
|--------|-------------------|------------|
| **Privacy** | ✅ Complete local control | ❌ External processing |
| **Cost** | ✅ One-time setup cost | ❌ Per-token pricing |
| **Performance** | ⚠️ Hardware dependent | ✅ Consistent cloud performance |
| **Availability** | ⚠️ Depends on grey-area uptime | ✅ High availability SLA |
| **Model Updates** | ⚠️ Manual model management | ✅ Automatic improvements |
| **Setup Complexity** | ❌ Requires local infrastructure | ✅ API key only |

#### Implementation Phases for Ollama Integration

##### Phase 1: Ollama Infrastructure (Week 1-2)
- [ ] Install Ollama on grey-area via NixOS
- [ ] Benchmark grey-area hardware with different models
- [ ] Set up model management and automatic updates
- [ ] Configure networking and firewall rules
- [ ] Test basic inference performance

##### Phase 2: Task Master Adaptation (Week 2-3)
- [ ] Modify Task Master to support Ollama API
- [ ] Implement AI provider abstraction layer
- [ ] Add response caching and optimization
- [ ] Create model selection logic based on task complexity
- [ ] Test task breakdown and analysis quality

##### Phase 3: Context7 Integration (Week 3-4)
- [ ] Configure MCP server for Ollama endpoint
- [ ] Implement context sharing between services
- [ ] Set up GitHub Copilot → Context7 → Ollama flow
- [ ] Test end-to-end workflow with local inference
- [ ] Performance tuning and monitoring

##### Phase 4: Hybrid Deployment (Week 4-5)
- [ ] Implement fallback to external APIs for complex queries
- [ ] Create intelligent routing based on task complexity
- [ ] Set up monitoring and alerting for Ollama service
- [ ] Document operational procedures
- [ ] Create backup and disaster recovery plans

#### Hardware Recommendations for Grey-Area

##### Minimum Requirements
- **RAM**: 16GB (8GB for OS + 8GB for model)
- **CPU**: 4+ cores with good single-thread performance
- **Storage**: 100GB SSD for models and cache
- **Network**: Gigabit connection to sleeper-service

##### Optimal Configuration
- **RAM**: 32GB (enables larger models and concurrent requests)
- **CPU**: 8+ cores or GPU acceleration
- **Storage**: NVMe SSD for maximum I/O performance
- **Monitoring**: Resource usage tracking for optimization

#### Risk Assessment for Ollama Deployment

##### Technical Risks
- **Hardware Limitations**: Grey-area may not have sufficient resources for larger models
- **Single Point of Failure**: Ollama downtime affects entire AI workflow
- **Model Quality**: Local models may not match external APIs' performance for complex tasks

##### Mitigation Strategies
- **Hybrid Approach**: Use Ollama for standard tasks, external APIs for complex analysis
- **High Availability**: Set up Ollama clustering if multiple servers available
- **Progressive Deployment**: Start with smaller models and scale up based on performance

#### Success Metrics

##### Performance Benchmarks
- **Response Time**: <10 seconds for standard task analysis
- **Accuracy**: Task breakdown quality comparable to external APIs
- **Availability**: >99% uptime for Ollama service
- **Resource Usage**: <80% RAM utilization during peak usage

##### Cost Analysis
```bash
# Estimated savings over 1 year
External APIs (estimated): $500-1000/year
Ollama Infrastructure: $0 (using existing hardware)
Power Consumption: ~$50/year additional
Net Savings: $450-950/year
```

**Recommendation**: Proceed with Ollama integration as primary research track, with external APIs as fallback option. This approach provides maximum privacy, cost efficiency, and aligns with your self-hosted infrastructure philosophy.