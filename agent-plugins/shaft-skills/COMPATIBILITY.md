# Compatibility

Evidence applies to `shaft-skills` 10.4.20261121 as of 2026-11-21. Package versions match the canonical SHAFT engine version. The consumer package ships one self-contained entrypoint, `shaft-developer`. Each column is an independent evidence level; a pass in one column never implies a pass in another.

## Host matrix

Installers, `shaft_project_init_agents`, and the IntelliJ setup wizard share one host registry. They install `shaft-developer` into the host skill root and do not copy contributor packs or CI scorecards.

| Host | Skill root | Instruction file |
| --- | --- | --- |
| Claude Code, Claude Desktop | `.claude/skills` | `CLAUDE.md` for Claude Code |
| Codex, Cursor, VS Code, Grok, Antigravity, Gemini, GitHub Copilot, SHAFT IntelliJ plugin | `.agents/skills` | `AGENTS.md`, or `.github/copilot-instructions.md` for VS Code and Copilot |
| OpenCode | `.opencode/skills` | `AGENTS.md` |

Cursor, OpenCode, VS Code, Grok, Antigravity, and Gemini use that same `shaft-developer` tree.

| Client | Package validation | Marketplace discovery | Install / enable | Real load |
| --- | --- | --- | --- | --- |
| Claude Code 2.1.251 | `claude plugin validate --strict <package>`; required by unauthenticated PR smoke. | `claude plugin marketplace add <package> --scope project`, then `claude plugin list --available --json`; required by unauthenticated PR smoke. | `claude plugin install shaft-skills@shaft-skills --scope project`, then `claude plugin list --json`; required by unauthenticated PR smoke and followed by cleanup. | Scheduled/manual acceptance runs `claude -p ... --output-format json`. A missing `ANTHROPIC_API_KEY` is recorded as `external_blocker`, not a package failure. |
| Codex CLI 0.146.0 | `codex plugin marketplace add <package> --json` validates the native marketplace snapshot; required by unauthenticated PR smoke. | `codex plugin list --available --json`; required by unauthenticated PR smoke. | `codex plugin add shaft-skills@shaft-skills --json`, then `codex plugin list --json`; required by unauthenticated PR smoke and followed by cleanup. | Scheduled/manual acceptance runs `codex exec --json -s read-only --skip-git-repo-check -`. A missing `OPENAI_API_KEY` is recorded as `external_blocker`, not a package failure. |

The smoke and live lanes publish `agent-plugin-client-evidence.json`. Every row records the client version, exactly one evidence level, commands, verdict, detail, and context-budget warnings. Live acceptance also runs the canonical 37-case corpus in `agent-plugins/shaft-skills/evals/cases.json`; each per-case result records the expected, rejected, and observed specialist. Every native call shares a 900-second execution budget; the 600-second routing deadline preserves completed evidence when the deadline or a recognized provider/network blocker stops the remaining cases, while cleanup and artifact time are reserved separately. An unrecognized nonzero client exit is a distinct client failure, not a routing-selection failure. The historical Codex load passed in [#4576](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4576); Claude model access was blocked there. [#4636](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4636) owns the repeatable evidence introduced here.
