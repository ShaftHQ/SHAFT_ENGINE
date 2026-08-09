# AGENTS.md

## Repository

ChaosEngine by Mohab Mohie. SHAFT_ENGINE is a Maven Java automation
framework: core `shaft-engine/`, optional `shaft-*`, CI tools `scripts/ci/`.
Config wins. Start from requested goal and affected files.

## Mandatory entrypoint

Before every task, read and follow
[act-as-mohab](.agents/skills/act-as-mohab/SKILL.md). It is the single
provider-agnostic router for intent, capability levels, skills, MCPs, Caveman,
Ponytail, TDD, PDCA, Memory, MemPalace, Graphify, delegation, and completion.
Every main thread and delegate loads it; repo playbooks are reached through
that entrypoint, never as competing policy. Its triage runs before every task
and decides whether the change also earns
[consult-first](.agents/skills/act-as-mohab/references/consult-first.md).

Name capability only as most intelligent, default, or mechanical. Never name a
model or product in tracked guidance.

`CLAUDE.md` and `.claude/skills/act-as-mohab/SKILL.md` are thin adapters.
Grok uses `AGENTS.md` plus that compatible adapter; do not add duplicate Grok
guidance. All operational paths in tracked guidance/config stay relative.

## Work lifecycle

Session branch and worktree cleanup, and the fresh `ChaosEngine/*` base, are
owned by the entrypoint's Task isolation section. Reuse one session and make
dependent subtasks commits. Before PR: sync default, resolve conflicts, rerun
affected checks, commit, push, and maintain tracker plus linked subtask issues
per the routed GitHub playbook. `.memory/events.jsonl` alone may report
`CONFLICTING`; its union merge is authoritative locally (#4137).

Role boundaries and capability-level delegation live only in the mandatory
act-as-mohab entrypoint.

## Working rules

- Read first; follow patterns; tight scope; preserve user work.
- Reproduce defects and add focused regressions. Preserve public API;
  deprecate before removal.
- User-facing work starts from rendered intent and finishes only when real
  user flow passes. Function changes update companion docs under
  `../shafthq.github.io` in a separate PR.
- Never expose secrets or run deploy, publish, history rewrite, cleanup, or
  cloud suites unless asked.
- No generated reports, binaries, caches, `target/`, Graphify output, or
  MemPalace runtime indexes in git. Canonical non-secret policy/config stays tracked.
- Fix small blockers in path. Search before filing every non-blocking adjacent
  finding as an issue; do not silently expand scope or leave it only in chat.
- Preserve structured data with structured APIs. Keep prose natural; avoid
  repetitive filler and stock AI wording.

## Windows and GUI safety

Do not launch GUI applications or handlers: `start`, `explorer`, `Invoke-Item`,
`Start-Process`, `rundll32`, `os.startfile`, browsers, editors, or installers.
Use non-interactive `py -3`, `python3`, `node`, PowerShell scripts, Maven,
`npm`, `dotnet`, and git. Ask before servers/watchers, Allure report serving,
browser capture, mobile inspector/emulator, or waits.

Maven tests remain scoped and headless. Always include
`-Dallure.automaticallyOpen=false`; disable GUI Lighthouse behavior. In
PowerShell quote property arguments and tokens containing `{}`, `@`, `;`,
`&`, or `|`.

## Memory and validation

Current files beat every index. Query native `.memory/`, MemPalace, and
Graphify through act-as-mohab before broad manual discovery; verify with
targeted `rg`. Store durable decisions/gotchas once with evidence, reuse IDs,
delete dead entries, and never create diaries.

Use smallest non-redundant check after edits/rebases/dependency changes:

- Guidance/memory: `py -3 scripts/ci/validate_agent_setup.py --skip-external`.
- Local code: affected tests, then one affected compile/package.
- Shared API/build/release: targeted check, then full compile/package.
- UI/report: relevant test plus image/browser evidence; report if blocked.
- External/cloud E2E: only required infrastructure.

Never trust a Maven/Allure banner alone; confirm test reports before verdict.
User harness drift/check deploys through
`py -3 scripts/agents/sync_user_harness.py [--apply]`. Secrets stay only in
user-level host directories and are never synced.

## Completion

Report outcomes, exact checks, and Learning Loop result: native Memory,
MemPalace/Graphify refresh, skill/playbook correction, issue filed, or none.
