# AGENTS.md

## Repository

ChaosEngine, by Mohab Mohie. SHAFT_ENGINE is a Maven Java automation framework; config wins. Core `shaft-engine/`; optional `shaft-*`; tools `scripts/ci/`. Start at goal/files; prefer `rg`.

## Routing

Bridge: `framework-source-rules` main Java; `java-test-rules` tests; `ci-failure-investigator` CI; `flaky-test-stabilizer` flaky; `release-dependency-guard` release/deps; `graphify` graph; `shaft-ui-design` UI; `shaft-marketing-ad-producer` ads; `public-behavior-docs-synchronizer` docs; `mcp-transport-contract-auditor` MCP; `modular-boundary-auditor` modules; `allure-extent-report-operator` reports; `agent-guidance-boundary-guard` guidance; `agentic-pdca-loop` PDCA phases.

## New Task Flow

At session start fetch/prune, branch/worktree fresh `ChaosEngine/*` from `origin/main`; reuse session -- sub-tasks are commits, unless file-dependent (merge first, branch+PR/issue). Before PR sync default, resolve conflicts, rerun checks; commit, push, 1 PR/session, `Closes #N`.

## Working Rules

- Read first; follow patterns; tight scope; preserve user work.
- Use structured APIs for structured data.
- Reproduce defects; add focused regressions.
- Preserve public API; deprecate before removal.
- Docs repo `C:\Users\Mohab\IdeaProjects\shafthq.github.io`; targeted `rg`. Function changes need guide + docs PR.
- Never expose secrets or run deploy/publish/rewrites/cleanup/cloud suites unless asked.
- No generated reports, binaries, or `target/`; browser tests headless unless headed approved.
- Blockers/small issues in path: fix inline. Anything else out-of-scope (enhancement, missing feature, degraded health metric, followup) -- never silently drop or just chat it: interactive -> ask now-vs-issue; noninteractive -> always `gh issue create` same session (search first, consolidate). Don't hunt for extras.

## Windows/Codex Safety

No GUI/shell-open: avoid `start`, `explorer`, `Invoke-Item`, `Start-Process`, `rundll32`, `os.startfile`, browsers/editors/installers. Run via `py -3`, `node`, `powershell -ExecutionPolicy Bypass -File`, `Get-Content`, `mvn`, `npm`, `dotnet`, `git`. Ask before Allure report/serve, servers/watchers, browser capture, mobile inspector/emulator, waits. Maven: always `-Dallure.automaticallyOpen=false` (SHAFT.Properties.allure; defaults `true` -- opens the report in a browser post-run; not `allure.open`, the Allure-3-CLI flag, already `false`) + GUI-off Lighthouse `-D...=false`.

## Memory & Learning Loop

Memory: `.memory/`; current files win. `AGENTS.md` canonical; `CLAUDE.md` adapts only. Load `memory load "<task>"`/`memory search` for retrieval. Save durable decisions/constraints/gotchas/workflows/corrections with evidence; reuse IDs; no duplicates/diaries.

Learning Loop (every session): note learnings as they surface; before Completion route each -- durable fact/gotcha -> `memory remember`; repo structure changed -> refresh or flag graphify; reusable procedure or guidance that misled -> add/fix a skill (`agent-guidance-boundary-guard` flow); enhancement/non-blocking issue -> file it now (Working Rules). Nothing durable is a valid outcome -- say so.

User harness (`~/.claude`) deploys from canonical `.claude/user-harness/` via `py -3 scripts/agents/sync_user_harness.py` (`--check`/`--apply`). Secrets live only in `~/.claude`, never in the repo.

## Validation

Before forked Maven/Surefire/TestNG, load gotchas. If delete gotcha is active, avoid `mvn test`; use compile/test-compile, static checks, or disposable copy. Use the smallest non-redundant check; rerun only after edits/rebases/dependency changes.

- Guidance/memory: `py -3`/`python3 scripts/ci/validate_agent_setup.py`
- Local code: affected tests, then one compile/package.
- IntelliJ plugin: Gradle 9+ + JDK 21; screenshots via `ShaftPluginScreenshotRendererTest -Dshaft.intellij.screenshotDir=...`.
- Shared API/build/release: targeted, then full compile/package.
- Visual: relevant test + image/browser evidence.
- UI/report PRs need screenshots; draft/report if blocked.
- External/cloud E2E: required infra only.

PowerShell: quote `'-Dname=value'`, `'stash@{0}'`, args with `{}`, `@`, `;`, `&`, `|`. Confirm Allure before SHAFT verdicts.

## Skills & MCP

`.claude/settings.json` + `.mcp.json`. Route by task shape; skip-rules bind:

- Plugin Swing UI: `frontend-design` (net-new surfaces only) -> `jdtls-lsp` -> JetBrains MCP inspections (optional) -> plugin screenshot renderer review. Browser MCP tools cannot see Swing.
- Docs/report web UI: `frontend-design` -> implement -> shaft-mcp browser evidence (screenshots + `browser_accessibility_audit`). Perf/network regressions: shaft-mcp `browser_network_requests`.
- Deps/release: `release-dependency-guard` -> `maven-tools-mcp` for live Maven Central facts (in-tree facts: just `rg` the pom) -> `ci-failure-investigator` on breakage.
- `context7`: past-cutoff library APIs only, else repo exemplars.
- Context/history/relationships: `mempalace`/`graphify` first, not grep; `rg` still for live code verification (mempalace/graphify can be stale).
- Skip `jdtls-lsp` for one-liners; value scales with impact. `mcp-server-dev`: net-new tool schema only.
- Repo `.claude/skills/`: `act-as-fable` methodology (binding: always, every model, every subagent; owns skill-routing triggers + delegation tiers); `shaft-mastery`/`ponytail`/`caveman`/`test-driven-development`/`graphify`/`work-github`.

## Agent Hierarchy & Model Routing

Fable plans/delegates/reviews/verifies on the main thread; it does not implement. Route down: Haiku (mechanical/spec-exact/bulk), Sonnet (one bounded component per written spec), Explore/Plan for research; synthesis + final verification stay on the main thread. Delegation tiers, the subagent covenant, and the architectural-decision second pass are owned by act-as-fable. No Workflow tool or saved workflows (`.claude/workflows/` stays deleted). PDCA personas are sequential phases of one session, not agents (`agentic-pdca-loop`). No `ralph-loop` (Stop-hook looping + Maven fork gotchas -> Windows runaways).

## Completion

Report changes/checks/outcomes + Learning Loop results: memory/graphify/skill/issue updates, or none.
