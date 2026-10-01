# AGENTS.md

## Repository facts

ChaosEngine by Mohab Mohie. SHAFT_ENGINE is a Maven Java automation framework:
core `shaft-engine/`, optional `shaft-*`, IntelliJ plugin `shaft-intellij/`,
and CI tooling under `scripts/ci/`. Configuration wins. Start from the
requested goal and affected files.

## Canonical policy

ChaosEngine, through the marker pointer at the end of this file, is the only router and
working-policy owner. Do not restate its policies here or in host
adapters. `CLAUDE.md` and `GEMINI.md` only import this file (`@AGENTS.md`).
Cleanup scope: [cleanup-scopes](chaos-engine/references/cleanup-scopes.md).
Duty ownership: [agent_ownership.json](scripts/ci/agent_ownership.json).

## Repository safety

- Read live files first, preserve unrelated and pre-existing work, and keep
  changes within the requested repository and task scope.
- Do not launch GUI applications, browsers, editors, installers, servers, or
  watchers without explicit authorization.
- Keep Maven tests scoped and headless with
  `-Dallure.automaticallyOpen=false -DheadlessExecution=true`.
- Never track generated reports, binaries, caches, `target/`, build output,
  Graphify output, MemPalace runtime indexes, secrets, or machine-local state.
- Preserve public APIs; reproduce defects with focused regressions. Functional
  documentation changes remain a separate PR and use the configured docs root.
- Validate harness changes with
  `py -3 scripts/ci/validate_agent_setup.py --skip-external` and the smallest
  directly affected tests. Inspect result artifacts rather than banners alone.
<!-- CHAOSENGINE:START -->
Before every task, follow the canonical [ChaosEngine](.chaos-engine/skills/chaos-engine/SKILL.md). Load project identity from [.chaos-engine/identity.md](.chaos-engine/identity.md). Use `.chaos-engine/tool.py` for the project-local Memory, MemPalace, and Graphify tools. Prefer gh for GitHub when gh exists and is configured. CLI over MCP when both exist. Default MCP catalog never includes GitHub MCP.
<!-- CHAOSENGINE:END -->
