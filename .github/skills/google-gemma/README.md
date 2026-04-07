# Google Gemma Skills

Skills and instructions for using [Google Gemma](https://ai.google.dev/gemma) AI models with the SHAFT test automation framework.

## About Google Gemma

Google Gemma is a family of lightweight, state-of-the-art open models built from the same research and technology used to create the Gemini models. Gemma models are well-suited for a variety of text generation and code analysis tasks, and can be run locally (e.g., via Ollama, LM Studio, or the [AI Edge Gallery](https://github.com/google-ai-edge/gallery) on-device app) or through cloud APIs.

## Skill Structure (Google AI Edge Gallery Format)

This directory is a fully self-contained **[AI Edge Gallery skill](https://github.com/google-ai-edge/gallery/tree/main/skills)** that can be loaded directly into the Gallery app.

```
google-gemma/
├── SKILL.md              ← Gallery-format skill definition (frontmatter + LLM instructions)
├── scripts/
│   └── index.html        ← JS helper: fetches source files from GitHub URLs for analysis
├── README.md             ← This file
└── code-analysis-and-optimization.md  ← Detailed reference documentation
```

### Loading This Skill in AI Edge Gallery

1. Open the AI Edge Gallery app with your selected Gemma model.
2. Navigate to **Skill Manager** (tap the "Skills" chip in Agent Skills mode).
3. Tap **(+)** → **Load skill from URL**.
4. Enter the URL pointing to this directory (update the branch name if the default branch changes):
   ```
   https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/master/.github/skills/google-gemma/
   ```
5. The app will load `SKILL.md` and make the skill available.

### Loading via ADB (Local File)

```bash
adb push .github/skills/google-gemma/ /sdcard/Download/google-gemma/
```

Then use **Import local skill** in the Skill Manager.

## Available Skills

| Skill | File | Description |
|---|---|---|
| SHAFT_ENGINE Expert (Gallery format) | [SKILL.md](./SKILL.md) | Google AI Edge Gallery skill: analyzes SHAFT Java code and generates SHAFT-compliant tests. Supports GitHub URL input via JS helper. |
| Code Analysis and Optimization (reference) | [code-analysis-and-optimization.md](./code-analysis-and-optimization.md) | Detailed reference: static/dynamic analysis for Java 21 performance bottleneck identification with SHAFT-specific patterns |

## Skill Capabilities

| Capability | Mode | Trigger |
|---|---|---|
| Analyze pasted Java code | Text-only | User pastes code and asks for review |
| Fetch + analyze from GitHub URL | JS (`scripts/index.html`) | User provides a `github.com` file URL |
| Generate SHAFT-compliant test classes | Text-only | User asks to generate or write tests |
| Identify anti-patterns and fixes | Text-only | User asks for code fixes or best practices |

## Notes

- Gemma models are open-weight and can be run locally or through cloud APIs
- When using Gemma with SHAFT, provide relevant source code context for accurate analysis
- Gemma excels at code understanding, complexity analysis, and generating optimization suggestions
- The `SKILL.md` follows the [Google AI Edge Gallery skill format](https://github.com/google-ai-edge/gallery/tree/main/skills) — frontmatter metadata + LLM instructions
- The `scripts/index.html` JS helper fetches public GitHub files so Gemma can analyze them without the user copying and pasting large files manually
