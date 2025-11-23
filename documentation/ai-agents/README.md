# AI-agents

I want to explore the fesiablility to post-train/fine-tune and optimise an opensource model like ibms granite to live in ram and do inferance locally.
We want the model to be tiny, and highly tuned to the task.
Treat LLMs as computer programs, which they are.

## OS-level assistant

- train a model on linux knowledge and computer science
- use it only for sys-admin/devops.
- Bash expert this is the tools too use

## Composer assistant

- train model on musical theory and domain specific code composition software

---

## Implementation Roadmap

### Phase 1: Research & Setup

- [ ] **Task 1**: Research fine-tuning frameworks and PEFT
  - Investigate QLoRA, LoRA, and prefix tuning implementations
  - Evaluate libraries: Hugging Face PEFT, LiteLLM, Axolotl
  - Compare ease of use and resource requirements
  - *Alternative: explore Unsloth for optimized LoRA training on consumer GPUs*

- [ ] **Task 2**: Evaluate base models for domains
  - Test Granite 3B, Mistral 7B quantized, and Llama 2 7B on CPU
  - Benchmark inference speed, memory usage, and baseline quality on each domain
  - Check if base model performance is sufficient before fine-tuning
  - *Alternative: test TinyLlama or other 1-3B models for extreme resource constraints*

- [ ] **Task 3**: Design system prompts and RAG pipeline
  - Create specialized system prompts for OS-level and Composer assistants
  - Set up RAG infrastructure using existing home-lab docs and resources
  - Evaluate tools: LlamaIndex, Langchain, or custom retrieval
  - Establish baseline performance with prompt engineering alone

### Phase 2: Data Collection

- [ ] **Task 4**: Gather training data for OS-level assistant
  - Collect bash scripts, Linux sysadmin Q&A, DevOps documentation, and Linux internals resources
  - Structure into instruction-response pairs (~1000-5000 examples)
  - Sources: existing scripts, man pages, tutorials, your own troubleshooting logs

- [ ] **Task 5**: Gather training data for Composer assistant
  - Curate music theory resources, DAW documentation, composition examples, and plugin reference materials
  - Create instruction-response pairs for music theory queries and compositional tasks
  - Target 1000-5000 quality examples

### Phase 3: Infrastructure Setup

- [ ] **Task 6**: Set up fine-tuning infrastructure
  - Configure QLoRA training pipeline using Hugging Face Transformers or Axolotl
  - Set up experiment tracking (Weights & Biases or MLflow)
  - Create evaluation framework to measure improvements
  - *Alternative: use LiteLLM for multi-model training orchestration*

- [ ] **Task 7**: Implement data preparation pipeline
  - Write scripts to convert collected data into standard formats (jsonl, chat templates)
  - Build validation checks for data quality
  - Create stratified splits for train/eval/test
  - *Alternative: implement linting/deduplication with sentence transformers for quality control*

### Phase 4: Model Training

- [ ] **Task 8**: Fine-tune OS-level assistant model
  - Run QLoRA fine-tuning on Granite 3B with sysadmin dataset
  - Monitor loss curves and validation metrics
  - Experiment with LoRA rank, alpha, and learning rates
  - *Alternative: use Axolotl for simplified configuration-driven training*

- [ ] **Task 9**: Fine-tune Composer assistant model
  - Run QLoRA fine-tuning on Granite 3B (or separate Mistral 7B) with music theory dataset
  - Validate musical knowledge retention
  - Compare against base model

### Phase 5: Optimization

- [ ] **Task 10**: Optimize models for CPU inference
  - Quantize fine-tuned models to INT4 using llama.cpp or GGUF format
  - Benchmark inference speed and latency on target CPU
  - Profile memory usage under load

### Phase 6: Application Layer

- [ ] **Task 11**: Build inference API with Chainlit or similar
  - Create interactive chat interface for testing both assistants
  - Implement streaming responses for better UX
  - Add RAG document retrieval to context
  - *Alternative: use Gradio for lightweight web UI, or Chainlit for production chat interface*

- [ ] **Task 12**: Implement agent orchestration layer
  - Build routing logic to select appropriate assistant (OS vs Composer)
  - Implement tool calling: OS assistant can execute safe bash commands, Composer can query music databases
  - Use Chainlit/LangChain for agent framework
  - *Alternative: build custom agent with LLM-as-judge architecture using Claude API as fallback*

### Phase 7: Validation & Deployment

- [ ] **Task 13**: Create evaluation benchmark suite
  - Design domain-specific test sets (30-50 questions each)
  - Compare fine-tuned vs base vs RAG-only approaches
  - Measure accuracy, latency, and resource usage
  - Document tradeoffs

- [ ] **Task 14**: Package models for production deployment
  - Create Docker/Nix deployment for both assistants
  - Set up model versioning and A/B testing infrastructure
  - Document resource requirements and performance characteristics for your home-lab machines

- [ ] **Task 15**: Monitor and iterate on performance
  - Collect user interaction logs from deployed assistants
  - Identify failure modes and low-confidence predictions
  - Plan v2 fine-tuning with new data
  - Set up automated retraining pipeline
