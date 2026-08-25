# AGENTS.md

## Repository facts

ChaosEngine by Mohab Mohie. SHAFT_ENGINE is a Maven Java automation framework:
core `shaft-engine/`, optional `shaft-*`, IntelliJ plugin `shaft-intellij/`,
and CI tooling under `scripts/ci/`. Configuration wins. Start from the
requested goal and affected files.

## Canonical policy

Before every task, read and follow
[ChaosEngine](.agents/skills/chaos-engine/SKILL.md). It is the only router and
working-policy owner. Its selected profile and references own lifecycle hooks,
roles, capability levels, skills, tools, Caveman, Ponytail, TDD, research,
memory, task isolation, cleanup, delegation, review, delivery, and completion.
Do not restate those policies in host adapters.

Cleanup scope is defined only by
[cleanup-scopes](chaos-engine/references/cleanup-scopes.md). Harness duty
ownership is machine-readable in
[agent_ownership.json](scripts/ci/agent_ownership.json). `CLAUDE.md`, host
skills, and provider guidance remain thin pointers to the canonical router.

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
Before every task, follow the canonical [ChaosEngine](.chaos-engine/skills/chaos-engine/SKILL.md). Use `.chaos-engine/tool.py` for the project-local Memory, MemPalace, and Graphify tools.
<!-- CHAOSENGINE:END -->

## graphify

This project has a knowledge graph at graphify-out/ with god nodes, community structure, and cross-file relationships.

When the user types `/graphify`, use the installed graphify skill or instructions before doing anything else.

Rules:
- For codebase questions, first run `graphify query "<question>"` when graphify-out/graph.json exists. Use `graphify path "<A>" "<B>"` for relationships and `graphify explain "<concept>"` for focused concepts. These return a scoped subgraph, usually much smaller than GRAPH_REPORT.md or raw grep output.
- Dirty graphify-out/ files are expected after hooks or incremental updates; dirty graph files are not a reason to skip graphify. Only skip graphify if the task is about stale or incorrect graph output, or the user explicitly says not to use it.
- If graphify-out/wiki/index.md exists, use it for broad navigation instead of raw source browsing.
- Read graphify-out/GRAPH_REPORT.md only for broad architecture review or when query/path/explain do not surface enough context.
- After modifying code, run `graphify update .` to keep the graph current (AST-only, no API cost).
