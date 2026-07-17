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
- Blockers/small issues in path: fix inline. Enhancements/non-blocking issues: never silently drop -- route via the Learning Loop. Don't hunt for extras.

## Windows/Codex Safety

No GUI/shell-open: avoid `start`, `explorer`, `Invoke-Item`/`ii`, `Start-Process`, `rundll32`, `os.startfile`, browsers/editors/installers/dialogs. Run via `py -3`, `node`, `powershell -ExecutionPolicy Bypass -File`, `Get-Content`, `mvn`, `npm`, `dotnet`, `git`. Ask before Allure report/serve, servers/watchers, browser capture, mobile inspector/emulator, waits. Maven: always `-Dallure.automaticallyOpen=false` (SHAFT.Properties.allure; defaults `true`, opens the HTML report in a browser post-run -- distinct from `allure.open`, the Allure-3-CLI-native flag, already default `false`) + GUI-off Lighthouse `-D...=false`.

## Memory & Learning Loop

Memory: `.memory/`; current files win. `AGENTS.md` canonical; `CLAUDE.md` adapts only. Load `memory load "<task>"`/`memory search`; `gbrain query`/`code-def` for semantic retrieval -- supplements, never replaces (`skills/retrieval-reflex/`; auto-synced). Save durable decisions/constraints/gotchas/workflows/corrections with evidence; reuse IDs; no duplicates/diaries.

Learning Loop (every session): note learnings as they surface; before Completion route each -- durable fact/gotcha -> `memory remember`; repo structure changed -> refresh or flag graphify; reusable procedure or guidance that misled -> add/fix a skill (`agent-guidance-boundary-guard` flow); enhancement/non-blocking issue -> followup GitHub issue (search first; consolidate). Nothing durable is a valid outcome -- say so.

User harness (`~/.claude`) deploys from canonical `.claude/user-harness/` via `py -3 scripts/agents/sync_user_harness.py` (`--check`/`--apply`). Secrets live only in `~/.claude`, never in the repo.

## Validation

Before forked Maven/Surefire/TestNG, load gotchas. If delete gotcha is active, avoid `mvn test`; use compile/test-compile, static checks, or disposable copy. Use the smallest non-redundant check; rerun passing checks only after edits/rebases/dependency changes.

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
- Skip `jdtls-lsp` for one-liners; value scales with cross-module impact. `mcp-server-dev`: net-new tool naming/schema ergonomics only.
- Repo `.claude/skills/`: `act-as-fable` methodology (binding: always, every model, every subagent; owns skill-routing triggers + delegation tiers); `shaft-mastery`/`ponytail`/`test-driven-development`/`graphify`/`work-github`.
- Local infra: gbrain + `gbrain-ollama` Docker back retrieval.

## Agent Hierarchy & Model Routing

When Fable holds the main thread it plans, delegates, reviews, and verifies with real checks -- it does not implement. Route implementation down: Haiku first (mechanical/spec-exact/bulk work), Sonnet (one bounded component per written spec), Explore/Plan for research. Synthesis, integration, and final verification stay in the main thread, sequentially. Every delegated prompt embeds the act-as-fable covenant; subagents return conclusions, not file dumps; verify their claims against real files before building on them; check the graphify cache before broad exploration. No Workflow tool, saved workflows (`.claude/workflows/` stays deleted), or orchestrators. PDCA personas are sequential phases of one session, not agents (`agentic-pdca-loop`). No `ralph-loop` (Stop-hook looping + Maven fork gotchas -> Windows runaways).

## Completion

Report changes/checks/outcomes + Learning Loop results: memory/graphify/skill/issue updates, or none.

<!-- gbrain:retrieval-reflex:resolver-rows -->
- retrieval-reflex | salient class/module/incident; brain-page pointer; asserting non-trivial repo detail
<!-- /gbrain:retrieval-reflex:resolver-rows -->
