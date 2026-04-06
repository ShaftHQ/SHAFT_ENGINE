# AI Agent Skills for SHAFT_ENGINE

This directory contains skill definitions and instructions to help SHAFT users drive the most commonly used AI agents and models for test automation tasks.

## Overview

Each subdirectory contains skill files tailored to a specific AI agent or model. These skills provide structured prompts, capabilities, and trigger conditions that help users leverage AI tools effectively within the SHAFT test automation ecosystem.

## Supported AI Agents / Models

| Agent / Model | Directory | Description |
|---|---|---|
| [GitHub Copilot](./github-copilot/) | `github-copilot/` | Skills for GitHub Copilot integration with SHAFT |
| [DeepSeek](./deepseek/) | `deepseek/` | Skills for DeepSeek AI model integration |
| [Qwen](./qwen/) | `qwen/` | Skills for Alibaba's Qwen model integration |
| [Claude Code](./claude-code/) | `claude-code/` | Skills for Anthropic's Claude Code integration |
| [Google Gemma](./google-gemma/) | `google-gemma/` | Skills for Google's Gemma model integration |

## Skill File Format

Each skill is defined as a Markdown file within the appropriate agent directory. A skill file follows this structure:

- **Skill Name**: A short, descriptive identifier (e.g., `code-analysis-and-optimization`)
- **Description**: What the skill does and when to use it
- **Required Capabilities**: Detailed breakdown of the AI agent's expected capabilities
- **Trigger Condition**: When/how the skill should be activated
- **Output Generation**: What the skill produces
- **Summary of Action**: A concise overview of the skill's purpose

## How to Use

1. Navigate to the AI agent directory that matches the tool you are using
2. Find the relevant skill file for your task
3. Follow the instructions and provide the required inputs to the AI agent
4. Review the AI-generated output and integrate it into your SHAFT workflow

## Contributing

To add a new skill:

1. Choose or create the appropriate agent subdirectory
2. Create a new Markdown file following the naming convention: `skill-name.md`
3. Follow the skill file format described above
4. Submit a pull request with your new skill

To add support for a new AI agent:

1. Create a new subdirectory under `.github/skills/` named after the agent (lowercase, hyphenated)
2. Add a `README.md` in the new directory describing the agent and its capabilities
3. Add at least one skill file
4. Update this root `README.md` to include the new agent in the table above
