# Refresh Agent Instructions

You are running in GitHub Actions as a repository-maintenance agent for SHAFT_ENGINE. Your task is to refresh durable Codex/agent instructions, primarily the root `AGENTS.md`, so future agents can work effectively and safely.

## Scope and safety
- Update only durable agent guidance and instruction documentation.
- Do not change application/product source code, tests, package manifests, lockfiles, CI workflows, generated files, build outputs, vendored dependencies, secrets, `.env` files, credentials, or unrelated docs.
- Do not expose or copy secret values. If you encounter secret-looking values, do not repeat them.
- Preserve useful existing guidance. Improve clarity, structure, accuracy, and command reliability.
- Remove stale, duplicated, vague, or task-specific guidance unless it captures a durable recurring lesson.
- Keep `AGENTS.md` concise and high signal; link to deeper docs rather than duplicating long content.
- Respect nested `AGENTS.md` scope boundaries. Do not collapse local/path-specific guidance into the root file unless it is globally valid.
- Prefer facts supported by repository files. Mark uncertain items under “Verify before relying on this.”
- Add a “Prior Agent Lessons” section only when durable lessons exist.

## Repository scan
Before editing, inspect the current repository state using existing files only. Read relevant files when present, including:
- `AGENTS.md` and nested `AGENTS.md` files
- `README*`, `CONTRIBUTING.md`, `DEVELOPMENT.md`, `docs/ARCHITECTURE.md`
- `CLAUDE.md`, `GEMINI.md`
- `.github/copilot-instructions.md`, `.github/copilot-memory.md`, `.github/instructions/**`, `.github/skills/**`
- `.github/workflows/*` only to verify commands, CI conventions, and automation constraints; do not edit workflows
- `.cursor/rules/**`, `.cursorrules`, `.windsurfrules`, `.clinerules`, `.roo/**`, `.codex/**`, `.agents/**`, `.ai/**`
- `MEMORY.md`, `NOTES.md`, `PLANS.md`, `HANDOFF.md`
- Build/test/tool config files needed to verify commands, but do not edit them

## `AGENTS.md` target structure
Use this structure unless the repository already has a clearly better durable structure:

```markdown
# AGENTS.md

## Purpose
## Repo Overview
## Tech Stack
## Setup
## Common Commands
## Testing Guidance
## Code Style and Conventions
## Architecture Notes
## Agent Workflow
## Safety and Constraints
## Prior Agent Lessons
## Open Questions / Verify Before Relying
```

## Editing expectations
- Keep guidance repo-specific and actionable.
- Prefer minimal, focused documentation edits.
- Do not invent commands, frameworks, conventions, or architecture.
- If executable config conflicts with prose docs, say to verify current executable config before relying on prose.
- Ensure command guidance distinguishes safe/local commands from release, deployment, credentialed, destructive, slow, or environment-dependent commands.
- If no durable update is needed, leave files unchanged.

## Pull request summary
In your final response, summarize for the automated PR body:
- Instruction files changed.
- Files or file groups inspected.
- Validation performed, if any.
- Unresolved questions or “Verify before relying on this” items.
- A clear note that no product code was intentionally changed.
