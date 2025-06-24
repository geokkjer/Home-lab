# Home Lab Project Conventions

## Code Style Guidelines

### NixOS Configuration

- Use descriptive variable names in Nix expressions
- Comment complex expressions and configurations
- Follow the NixOS manual conventions for module structure
- Use `lib.mkOption` for configurable options
- Prefer `services.` namespace for service configurations

### Guile Scheme

- Use kebab-case for function and variable names
- Prefer pure functions when possible
- Add docstrings for public functions
- Use meaningful parameter names
- Follow GNU Guile coding standards

### Documentation

- Use Markdown for all documentation
- Include examples in code documentation
- Keep README files up to date
- Document configuration options clearly

### Project Structure

- Keep machine-specific configurations in `machines/` directory
- Use `modules/` for reusable NixOS modules
- Store documentation in `documentation/` directory
- Place scripts in `scripts/` directory
- Research and notes go in `research/` directory

### Git Workflow

- Use descriptive commit messages
- Prefix commits with area of change (e.g., "modules:", "docs:", "machines:")
- Create feature branches for significant changes
- Keep commits focused and atomic

### Security

- Never commit secrets or API keys
- Use proper file permissions for sensitive files
- Follow NixOS security best practices
- Document security considerations for services
