# AGENTS.md

## Repository facts

ChaosEngine by Mohab Mohie. SHAFT_ENGINE is a Maven Java automation framework:
core `shaft-engine/`, optional `shaft-*`, IntelliJ plugin `shaft-intellij/`,
and CI tooling under `scripts/ci/`. Configuration wins. Start from the
requested goal and affected files.

## Canonical policy

Before every task, read and follow
[ChaosEngine](chaos-engine/skills/chaos-engine/SKILL.md). It is the only router and
working-policy owner. Its selected profile and references own lifecycle hooks,
roles, capability levels, skills, tools, Caveman, Ponytail, TDD, research,
memory, task isolation, cleanup, delegation, review, delivery, and completion.
Do not restate those policies in host adapters.

Cleanup scope is defined only by
[cleanup-scopes](chaos-engine/references/cleanup-scopes.md). Harness duty
ownership is machine-readable in
[agent_ownership.json](scripts/ci/agent_ownership.json). `CLAUDE.md`, host
skills, and provider guidance remain thin pointers to the canonical router.

Harness parity (permanent): lasting behavior and policy — including Learning
Session after every delivery — must live in the portable ChaosEngine overlay
(hooks, skills, installer/doctor, host guidance adapters), never only in one
agent's memory or routines. Unchanged `chaos-engine/` files are not a valid
Learning Session skip. Research/explore isolation follows
[context firewall](chaos-engine/references/context-firewall.md) (all hosts); periodic [meta-optimize](chaos-engine/references/meta-optimize.md); opt-in [draft skill PRs](chaos-engine/references/draft-skill-pr.md) default OFF.

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
