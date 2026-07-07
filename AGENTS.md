# AGENTS.md

## Repository

SHAFT_ENGINE is a Maven Java automation framework; config wins. Core `shaft-engine/`; optional `shaft-*`; tools `scripts/ci/`. Start at goal/files; prefer `rg`.

## Routing

Bridge: `framework-source-rules` main Java; `java-test-rules` tests; `ci-failure-investigator` CI; `flaky-test-stabilizer` flaky; `release-dependency-guard` release/deps; `graphify` graph; `shaft-ui-design` UI; `shaft-marketing-ad-producer` ads; `public-behavior-docs-synchronizer` docs; `mcp-transport-contract-auditor` MCP; `modular-boundary-auditor` modules; `allure-extent-report-operator` reports; `agent-guidance-boundary-guard` guidance.

## New Task Flow

For edits: preserve work; at session start fetch/prune, branch/worktree fresh `codex/*` from `origin/main`; reuse for the whole session -- sub-tasks are commits, not new branches. Workflow-internal worktrees fold into the session branch before PR. Before PR sync default, resolve conflicts, rerun checks; commit, push, open one ready PR for the session. Draft only if blocked/incomplete/requested.

## Working Rules

- Read first; follow patterns; tight scope; preserve user work.
- Use structured APIs for structured data.
- Reproduce defects; add focused regressions.
- Preserve public API; deprecate before removal.
- Docs repo `C:\Users\Mohab\IdeaProjects\shafthq.github.io`; targeted `rg`/excerpts. Function changes need guide + docs PR.
- Never expose secrets or run deploy/publish/rewrites/cleanup/cloud suites unless asked.
- No generated reports, binaries, or `target/`; browser tests headless unless headed approved.

## Windows/Codex Safety

No GUI/shell-open: avoid `start`, `explorer`, `Invoke-Item`/`ii`, `Start-Process`, `cmd /c start`, `rundll32`, `os.startfile`, browsers/editors/installers/dialogs. Run via `py -3`, `node`, `powershell -ExecutionPolicy Bypass -File`, `Get-Content`, `mvn`, `npm`, `dotnet`, `git`. Ask before Allure report/serve, servers/watchers, browser capture, mobile inspector/emulator, waits. Maven: add GUI-off Allure/Lighthouse `-D...=false`.

## Memory

Memory: `.memory/`; current files win. `AGENTS.md` canonical; `CLAUDE.md` adapts only. Load `memory load "<task>"`/`memory search`. Save durable decisions/constraints/gotchas/workflows/corrections with evidence; reuse IDs; no duplicates/diaries/end saves.

## Validation

Before forked Maven/Surefire/TestNG, load gotchas. If delete gotcha is active, avoid `mvn test`; use compile/test-compile, static checks, or disposable copy. Use the smallest non-redundant check; rerun passing checks only after edits/rebases/dependency changes.

- Guidance/memory: `python3 scripts/ci/validate_agent_setup.py`; Win `py -3 ...`
- Local code: affected tests, then one compile/package.
- IntelliJ plugin: Gradle 9+ + JDK 21; screenshots via `ShaftPluginScreenshotRendererTest -Dshaft.intellij.screenshotDir=...`.
- Shared API/build/release: targeted, then full compile/package.
- Visual: relevant test plus image/browser evidence.
- UI/report PRs need screenshots; draft/report if blocked.
- External/cloud E2E: required infra only.

PowerShell: quote `'-Dname=value'`, `'stash@{0}'`, args with `{}`, `@`, `;`, `&`, `|`. Confirm Allure before SHAFT verdicts.

## Agentic Skills & MCP Tools

#3292 adopted 8 skills/MCP servers (`.claude/settings.json`, `.mcp.json`). Route by task shape; skip-rules bind:

- Plugin Swing UI: `frontend-design` (net-new surfaces only) -> `design` critique/UX copy -> `jdtls-lsp` -> JetBrains IDE MCP inspections when enabled (optional, per-dev) -> plugin screenshot renderer -> heuristic `accessibility-review` on renders. `webapp-testing` is Playwright; it cannot see Swing.
- Docs/report web UI: `frontend-design` -> `design` critique -> implement -> `webapp-testing` evidence -> `accessibility-review`. `chrome-devtools-mcp` only for perf/network regressions.
- Deps/release: `release-dependency-guard` -> `maven-tools-mcp` for live Maven Central facts (skip when in-tree: `rg` a pom beats a Docker cold start) -> `ci-failure-investigator` on breakage.
- `context7`: only past-training-cutoff APIs (Spring Boot 4.1, Spring AI 2.0, JUnit 6, Jackson 3, Selenium 4.45, Docusaurus 3.10/React 19); else repo exemplars win.
- Skip `jdtls-lsp` for one-liners; value scales with cross-module symbol impact. `mcp-server-dev`: net-new tool naming/schema ergonomics only.
- Follow-up: `a11ymcp` belongs in the docs repo.

## Agent Hierarchy

Fable plans (fallback Opus) -> Opus orchestrates multi-task plans (fallback Sonnet) -> Sonnet owns each task, delegates implementation to Haiku, QAs the diff with real checks, loops <=3 rounds, then finishes directly. Fresh context per spawned agent; handoffs serialize to files/structured output; log which model ran each phase. PDCA personas: Kevin=Fable/Opus, Bob=Haiku, Bruce=Sonnet (`agentic-pdca-loop`). Saved workflows: `.claude/workflows/shaft-bug-fix.js`, `shaft-release-ci-fix.js`, `shaft-feature-dev.js`. Precisely-arranged core code (sync/wait internals, locator resolution, EDT threading): Sonnet implements directly. No repo `ralph-loop`: unbounded Stop-hook looping plus Maven fork gotchas risks Windows runaways; use the capped workflows.

Use the hierarchy workflows for multi-module changes, release/CI incidents, or plans with 3+ independent tasks. For single-file fixes, doc edits, and version bumps, a single Sonnet session implements and verifies directly. Never spawn an orchestrator for a one-task plan.

## Completion

Report changes/checks/outcomes.
