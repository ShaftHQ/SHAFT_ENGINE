# Archived Google Gemma Skill

This historical integration is retained for reference but is outside active
agent-skill discovery. Codex, Copilot, and Claude use the canonical playbooks
under `.github/skills/`.

## About Google Gemma

Google Gemma is a family of lightweight open models built from the same research used for Gemini. In this repository,
Gemma is configured as a SHAFT-focused expert for code diagnosis, issue fixing guidance, and test generation.

## Skill Structure (Google AI Edge Gallery Format)

This directory is a self-contained [AI Edge Gallery skill](https://github.com/google-ai-edge/gallery/tree/main/skills).

```
google-gemma/
├── SKILL.md                              ← Gallery-format skill definition
├── scripts/
│   └── index.html                        ← JS helper to fetch GitHub source files
├── README.md                             ← This file
└── code-analysis-and-optimization.md     ← Extended analysis reference
```

## Historical Use

This snapshot documents the former AI Edge Gallery integration. It is not a
supported active agent workflow and should not be loaded as repository policy.

## Available Skills

| Skill                                      | File                                                                     | Description                                                                          |
|--------------------------------------------|--------------------------------------------------------------------------|--------------------------------------------------------------------------------------|
| SHAFT_ENGINE Expert                        | [SKILL.md](./SKILL.md)                                                   | High-accuracy SHAFT issue triage, root-cause analysis, and minimal-risk fix guidance |
| Code Analysis and Optimization (reference) | [code-analysis-and-optimization.md](./code-analysis-and-optimization.md) | Broader performance/architecture analysis reference                                  |

## Enhanced Capabilities

| Capability                         | Mode      | Value                                                                             |
|------------------------------------|-----------|-----------------------------------------------------------------------------------|
| Analyze pasted Java code           | Text      | Detects SHAFT anti-patterns with confidence scoring                               |
| Fetch and analyze GitHub file URLs | JS + Text | Converts supported GitHub URLs to raw source with validation and timeout handling |
| Prioritize fixes                   | Text      | Produces risk-based fix order and verification steps                              |
| Generate SHAFT-compliant tests     | Text      | Produces TestNG/JUnit5 skeletons aligned to SHAFT lifecycle patterns              |
| Improve issue-fix accuracy         | Text      | Distinguishes root cause vs symptoms and flags missing context                    |

## Notes

- Best results come from including setup/teardown + test methods + relevant stack trace.
- The helper script only supports public GitHub file URLs.
- The skill emphasizes minimal safe changes and explicit validation guidance.
