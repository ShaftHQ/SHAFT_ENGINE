# Google Gemma Skills

Skills and instructions for using [Google Gemma](https://ai.google.dev/gemma) AI models with the SHAFT test automation framework.

## About Google Gemma

Google Gemma is a family of lightweight, state-of-the-art open models built from the same research and technology used to create the Gemini models. Gemma models are well-suited for a variety of text generation and code analysis tasks, and can be run locally or via cloud APIs.

## Available Skills

| Skill | File | Description |
|---|---|---|
| Code Analysis and Optimization | [code-analysis-and-optimization.md](./code-analysis-and-optimization.md) | Comprehensive static and dynamic code analysis for Java 21 performance bottleneck identification, with SHAFT-specific patterns |

## Notes

- Gemma models are open-weight and can be run locally (e.g., via Ollama, LM Studio) or through cloud APIs
- When using Gemma with SHAFT, provide relevant source code context for accurate analysis — include build files (`pom.xml`) and test classes alongside application code
- Gemma excels at code understanding, complexity analysis, and generating optimization suggestions
- For SHAFT-specific analysis, point Gemma to the `Usage with SHAFT_ENGINE` section in the skill file for framework-aware recommendations
