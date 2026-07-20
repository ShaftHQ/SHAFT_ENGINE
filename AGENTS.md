# AGENTS.md

## Repository

ChaosEngine, by Mohab Mohie. SHAFT_ENGINE is a Maven Java automation framework; config wins. Core `shaft-engine/`; optional `shaft-*`; tools `scripts/ci/`. Start at goal/files.

## Routing

Bridge: `framework-source-rules` main Java; `java-test-rules` tests; `ci-failure-investigator` CI; `flaky-test-stabilizer` flaky; `release-dependency-guard` release/deps; `graphify` graph; `shaft-ui-design` UI; `shaft-marketing-ad-producer` ads; `public-behavior-docs-synchronizer` docs; `mcp-transport-contract-auditor` MCP; `modular-boundary-auditor` modules; `allure-extent-report-operator` reports; `agent-guidance-boundary-guard` guidance; `agentic-pdca-loop` PDCA phases.

## New Task Flow

At session start fetch/prune, branch/worktree fresh `ChaosEngine/*` from `origin/main`; reuse session -- sub-tasks are commits, unless file-dependent (merge first, branch+PR/issue). Before PR sync default, resolve conflicts, rerun checks; commit, push, 1 PR/session, `Closes #N`.

## Working Rules

- Read first; follow patterns; tight scope; preserve user work.
- Use structured APIs for structured data.
- Reproduce defects; add focused regressions.
- Preserve public API; deprecate before removal.
- User-facing surface: draft/render the UI against intent before implementing; done means the user-visible flow passes, not just unit tests.
- Docs repo `C:\Users\Mohab\IdeaProjects\shafthq.github.io`; targeted `rg`. Function changes need guide + docs PR.
- Never expose secrets or run deploy/publish/rewrites/cleanup/cloud suites unless asked.
- No generated reports, binaries, or `target/`; browser tests headless unless headed approved.
- Blockers in path: fix inline. Any other out-of-scope finding -- never drop or just chat it: interactive -> ask now-vs-issue; noninteractive -> `gh issue create` same session (search first, consolidate). Don't hunt for extras.

## Windows/Codex Safety

No GUI/shell-open: avoid `start`, `explorer`, `Invoke-Item`, `Start-Process`, `rundll32`, `os.startfile`, browsers/editors/installers. Run via `py -3`, `node`, `powershell -ExecutionPolicy Bypass -File`, `Get-Content`, `mvn`, `npm`, `dotnet`, `git`. Ask before Allure report/serve, servers/watchers, browser capture, mobile inspector/emulator, waits. Maven: always `-Dallure.automaticallyOpen=false` (SHAFT prop, defaults `true` -- auto-opens browser; not Allure-3-CLI `allure.open`) + GUI-off Lighthouse `-D...=false`.

## Memory & Learning Loop

Memory: `.memory/`; current files win. `AGENTS.md` canonical; `CLAUDE.md` adapts only. Retrieve: `memory load "<task>"`/`memory search`. Save durable decisions/gotchas/corrections with evidence; reuse IDs; no duplicates/diaries.

Learning Loop (every session): note learnings as they surface; before Completion route each -- durable fact/gotcha -> `memory remember`; repo structure changed -> refresh or flag graphify; reusable procedure or guidance that misled -> add/fix a skill (`agent-guidance-boundary-guard` flow); enhancement/non-blocking issue -> file it now (Working Rules). Nothing durable is a valid outcome -- say so.

User harness (`~/.claude`, incl. `agents/`) deploys from canonical `.claude/user-harness/` + `.claude/agents/` via `py -3 scripts/agents/sync_user_harness.py` (`--check`/`--apply`). Secrets live only in `~/.claude`, never in the repo.

## Validation

Before forked Maven/Surefire/TestNG runs, load gotchas; if delete gotcha active, avoid `mvn test` -- compile/test-compile, static checks, or disposable copy. Smallest non-redundant check; rerun only after edits/rebases/deps.

- Guidance/memory: `py -3`/`python3 scripts/ci/validate_agent_setup.py`
- Local code: affected tests, then one compile/package.
- IntelliJ plugin: build/verify facts live in the `shaft-mastery` intellij-plugin chapter; load it.
- Shared API/build/release: targeted, then full compile/package.
- Visual: relevant test + image/browser evidence.
- UI/report PRs need screenshots; draft/report if blocked.
- External/cloud E2E: required infra only.

PowerShell: quote `'-Dname=value'`, `'stash@{0}'`, args with `{}`, `@`, `;`, `&`, `|`. Confirm Allure before SHAFT verdicts.

## Skills & MCP

`.claude/settings.json` + `.mcp.json`. Route by task shape; skip-rules bind:

- Plugin Swing UI: `frontend-design` (net-new surfaces only) -> `jdtls-lsp` -> JetBrains MCP inspections (optional) -> plugin screenshot renderer review. Browser MCP tools cannot see Swing.
- Docs/report web UI: `frontend-design` -> implement -> shaft-mcp browser evidence (screenshots + `browser_accessibility_audit`). Perf/network regressions: shaft-mcp `browser_network_requests`.
- Deps/release: `release-dependency-guard` -> `maven-tools-mcp` for live Maven Central facts (in-tree facts: just `rg` the pom; Docker down -- never start it: `curl` search.maven.org) -> `ci-failure-investigator` on breakage.
- `context7`: past-cutoff library APIs only, else repo exemplars.
- Discovery: `memory`/`mempalace`/`graphify` before any manual search -- never grep for what a store knows; `rg` only to verify live code (stores can be stale).
- Skip `jdtls-lsp` for one-liners; value scales with impact. `mcp-server-dev`: net-new tool schema only.
- Repo `.claude/skills/`: `act-as-fable` binds always (every model/subagent; owns routing, tiers, and the always-on `caveman` voice); `ponytail` binds every implementation decision; `shaft-mastery`/`test-driven-development`/`graphify`/`work-github` by trigger.

## Agent Hierarchy & Model Routing

Chaos Engine (`.claude/agents/chaos-engine.md`) is the main-thread orchestrator of every chat: Fable@high effort, else Sonnet@max. Never implements: breaks down/assigns/reviews, decides architecture on consult, checks tasks >20 min, accepts owner realignment. Delegates to `coder`/`reviewer`/`tester` (Sonnet L1; they load act-as-fable + TDD first); L1 may sub-delegate mechanical/bulk to L2 Haiku. All HIGH effort. Synthesis + final verification on main thread; act-as-fable owns tiers/covenant/second pass. Workflow tool/saved workflows only on explicit owner ask (`.claude/workflows/` stays deleted). PDCA personas are phases of one session, not agents (`agentic-pdca-loop`). No `ralph-loop` (Stop-hook loops + Maven forks -> Windows runaways).

## Completion

Report changes/checks/outcomes + Learning Loop results: memory/graphify/skill/issue updates, or none.
