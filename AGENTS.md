# AGENTS.md

## Repository

ChaosEngine, by Mohab Mohie. SHAFT_ENGINE is a Maven Java automation framework; config wins. Core `shaft-engine/`; optional `shaft-*`; tools `scripts/ci/`. Start at goal/files.

## Routing

Bridges (`.agents/skills/<name>/SKILL.md`, not Skill-invocable) enumerated in [routing bridges](.agents/routing-bridges.txt); consult before touching an unfamiliar area.

## New Task Flow

At session start fetch/prune, branch/worktree fresh `ChaosEngine/*` from `origin/main`; reuse session -- sub-tasks are commits, unless file-dependent (merge first, branch+PR/issue). Before PR, sync default, resolve conflicts (`.memory/events.jsonl` CONFLICTING alone is a false positive -- `merge=union` resolves it locally; rebase+push, #4137), rerun checks; commit, push, tracker+subtasks (work-github 3b).

## Working Rules

- Read first; follow patterns; tight scope; preserve user work.
- Use structured APIs for structured data.
- Reproduce defects; add focused regressions.
- Preserve public API; deprecate before removal.
- User-facing surface: draft/render UI vs intent first; done means user-visible flow passes, not just unit tests.
- Docs repo `C:\Users\Mohab\IdeaProjects\shafthq.github.io`; targeted `rg`. Function changes need guide + docs PR. User-facing prose: AI-tell strip pass -- vary sentence length, avoid delve/seamless/robust.
- Never expose secrets or run deploy/publish/rewrites/cleanup/cloud suites unless asked.
- No generated reports, binaries, or `target/`; browser tests headless unless headed approved.
- Blockers in path: fix inline. Other out-of-scope finds -- never drop/chat it: interactive -> ask now-vs-issue; noninteractive -> `gh issue create` same session (search first, consolidate). Don't hunt for extras.
- PR review: scan for deferred/out-of-scope/adjacent-finding language (own or delegate's) -- track before closing (rule above), cross-link the tracker.

## Windows/Codex Safety

No GUI/shell-open: avoid `start`, `explorer`, `Invoke-Item`, `Start-Process`, `rundll32`, `os.startfile`, browsers/editors/installers. Run via `py -3`, `node`, `powershell -ExecutionPolicy Bypass -File`, `Get-Content`, `mvn`, `npm`, `dotnet`, `git`. Ask before Allure report/serve, servers/watchers, browser capture, mobile inspector/emulator, waits. Maven: always `-Dallure.automaticallyOpen=false` (SHAFT prop, not Allure-3-CLI `allure.open`) + GUI-off Lighthouse `-D...=false`.

## Memory & Learning Loop

Memory: `.memory/`; current files win. `AGENTS.md` canonical; `CLAUDE.md` adapts only. Retrieve: `memory load "<task>"`/`memory search`. Save durable decisions/gotchas/corrections with evidence; reuse IDs; no duplicates/diaries; title by decision, not ship event; short ids; caveman-compress entries, still unambiguous. Dead entries: delete, never stale-mark (#4287).

Learning Loop (every session): note learnings live; before Completion route each -- durable fact/gotcha -> `memory remember`; repo structure changed -> refresh or flag graphify; reusable procedure or guidance that misled -> add/fix a skill (`agent-guidance-boundary-guard` flow); enhancement/non-blocking issue -> file it now (Working Rules). Nothing durable is fine -- say so.

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

- Plugin Swing UI: `frontend-design` (net-new surfaces only) -> `jdtls-lsp` -> JetBrains MCP inspections (optional) -> plugin screenshot renderer review; no Swing via browser MCP.
- Docs/report web UI: `frontend-design` -> implement -> shaft-mcp browser evidence (screenshots + `browser_accessibility_audit`). Perf/network regressions: shaft-mcp `browser_network_requests`.
- Deps/release: `release-dependency-guard` -> `maven-tools-mcp` for live Maven Central facts (in-tree facts: just `rg` the pom; Docker down -- never start it: `curl` search.maven.org) -> `ci-failure-investigator` on breakage; date-window ecosystem research to 30-60 days, prefer live community sources over stale posts.
- `context7`: past-cutoff library APIs only, else repo exemplars.
- Discovery: `memory`/`mempalace`/`graphify` before manual search -- never grep what a store knows; `rg` to verify live code (stores can be stale).
- Skip `jdtls-lsp` for one-liners; value scales with impact. `mcp-server-dev`: net-new tool schema only.
- Repo `.claude/skills/`: `act-as-mohab` binds always (all models; owns routing/tiers/voice); `ponytail` binds every implementation decision; `shaft-mastery`/`test-driven-development`/`graphify`/`work-github` by trigger.

## Agent Hierarchy & Model Routing

Chaos Engine is the main-thread orchestrator of every chat: Fable@high effort, else Sonnet@max. Never implements: breaks down/assigns/reviews, decides architecture on-consult, checks >20min tasks, accepts realignment. Charter lives only in `act-as-mohab` (`.claude/agents/chaos-engine.md` is a pointer, never a second copy), owning tiers/covenant/second pass/bootstrap. Delegates to `coder`/`reviewer`/`tester` (Sonnet L1, load act-as-mohab + TDD first); L1 may sub-delegate mechanical/bulk to L2 Haiku, all HIGH effort. Synthesis/verification stay on main thread. Workflow tool/saved workflows only on explicit owner ask (`.claude/workflows/` stays deleted). PDCA personas are phases of one session, not agents (`agentic-pdca-loop`); no `ralph-loop` (Stop-hook loops + Maven forks -> Windows runaways).

## Completion

Report changes/checks/outcomes + Learning Loop results: memory/graphify/skill/issue updates, or none.
