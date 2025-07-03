# AI Coding Assistant Customization Guide

This comprehensive guide explains how to personalize and focus your AI coding experience using GitHub Copilot and Claude Code. It covers custom prompts, memory files, instructions, and advanced personalization techniques for both tools.

---

## Table of Contents

- [Overview](#overview)
- [GitHub Copilot Customization](#github-copilot-customization)
  - [Personal Custom Instructions](#personal-custom-instructions)
  - [Repository Custom Instructions](#repository-custom-instructions)
  - [Organization Custom Instructions](#organization-custom-instructions)
  - [Prompt Files (Memory/Reusable Prompts)](#prompt-files-memoryreusable-prompts)
- [Claude Code Customization](#claude-code-customization)
  - [Memory Files (CLAUDE.md)](#memory-files-claudemd)
  - [Custom Commands](#custom-commands)
  - [Settings and Configuration](#settings-and-configuration)
  - [System Prompts](#system-prompts)
- [Configuration Instructions](#configuration-instructions)
- [Advanced Prompt Engineering](#advanced-prompt-engineering)
- [Best Practices](#best-practices)
- [Examples and Templates](#examples-and-templates)
- [References](#references)

---

## Overview

GitHub Copilot can be tailored to your workflow and preferences by providing it with additional context and instructions. This:

- Improves the relevance and quality of suggestions
- Reduces repetitive prompt writing
- Enables team/project/organization-wide standards

---

## Types of Customization

### Personal Custom Instructions

- Apply to all Copilot Chat conversations for your user.
- Set preferences like language, tone, or explanation style.
- Example: `Always respond in Norwegian.`

### Repository Custom Instructions

- Apply to all Copilot Chat conversations in a specific repository.
- Define project coding standards, frameworks, or tools.
- Example: `Use early returns in all functions.`
- File: `.github/copilot-instructions.md`

### Organization Custom Instructions

- (Enterprise only) Apply to all conversations within an organization.
- Set company-wide standards or security guidelines.
- Example: `For security questions, refer to the Security Docs Knowledge Base.`

### Prompt Files (Memory/Reusable Prompts)

- Markdown files (`*.prompt.md`) containing reusable instructions or context.
- Use for common tasks, onboarding, code reviews, or domain knowledge.
- Example: `API security review.prompt.md` with a checklist for secure APIs.

---

## How to Configure Each Type

### 1. Personal Custom Instructions

- In VS Code, open Copilot Chat settings.
- Add your preferences under `Personal custom instructions`.

  ```text
  Always use concise explanations.
  Prefer TypeScript over JavaScript.
  ```

### 2. Repository Custom Instructions

- Create a file: `.github/copilot-instructions.md` in your repository root.
- Add short, self-contained statements. Example:

  ```md
  We use Bazel for Java dependencies, not Maven.
  Always use double quotes and tabs for JavaScript.
  Our team uses Jira for tracking work.
  ```

### 3. Organization Custom Instructions

- (Requires Copilot Enterprise)
- Organization owners can set these in the GitHub web UI.

  ```text
  Always respond in English.
  Do not generate code blocks in responses.
  ```

### 4. Prompt Files (Memory/Reusable Prompts)

- Create a folder (e.g., `.github/prompts/` or `prompts/`).
- Add Markdown files with `.prompt.md` extension.
- Each file should contain a reusable prompt or checklist.
- Example: `API security review.prompt.md`

  ```md
  Secure REST API review:
  - Ensure all endpoints are protected by authentication and authorization
  - Validate all user inputs and sanitize data
  - Implement rate limiting and throttling
  - Implement logging and monitoring for security events
  ```
- Reference prompt files in Copilot Chat by name or use the VS Code Copilot Chat UI to insert them.

#### VS Code Settings Example

Add to your `settings.json`:

```json
{
  "chat.promptFiles": true,
  "chat.promptFilesLocations": {
    "default": ["./.github/prompts"]
  }
}
```

---

## Prompt Engineering Tips

- **Start general, then get specific:** Begin with a broad description, then add requirements.
- **Give examples:** Show input/output or code samples.
- **Break complex tasks into steps:** Ask for one thing at a time.
- **Avoid ambiguity:** Be explicit about what you want.
- **Indicate relevant code:** Open or reference files for context.
- **Iterate:** Refine your prompts if results aren’t ideal.

---

## Examples

### Personal Custom Instructions

```text
Always respond in Norwegian.
Explain a single concept per line. Be clear and concise.
```

### Repository Custom Instructions (`.github/copilot-instructions.md`)

```md
We use Bazel for managing our Java dependencies, not Maven.
We always write JavaScript with double quotes and tabs for indentation.
Our team uses Jira for tracking items of work.
```

### Prompt File Example (`prompts/API security review.prompt.md`)

```md
Secure REST API review:
- Ensure all endpoints are protected by authentication and authorization
- Validate all user inputs and sanitize data
- Implement rate limiting and throttling
- Implement logging and monitoring for security events
```

### VS Code Settings Example

```json
{
  "chat.promptFiles": true,
  "chat.promptFilesLocations": {
    "default": ["./.github/prompts"]
  }
}
```

---

## References

- [GitHub Copilot Docs](https://docs.github.com/en/copilot)
- [Customizing Copilot Chat Responses](https://docs.github.com/en/copilot/concepts/about-customizing-github-copilot-chat-responses)
- [Prompt Engineering for Copilot Chat](https://docs.github.com/en/copilot/concepts/prompt-engineering-for-copilot-chat)
- [Configure Custom Instructions](https://docs.github.com/en/copilot/how-tos/custom-instructions)

---

This guide is based on official documentation and best practices as of July 2025. For updates, check the GitHub Copilot documentation.
