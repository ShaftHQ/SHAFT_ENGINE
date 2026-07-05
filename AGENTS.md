# AGENTS.md

## Repository

SHAFT_ENGINE is a Maven Java automation framework; config wins. Core `shaft-engine/`; optional `shaft-*`; tools `scripts/ci/`. Start at goal/files; prefer `rg`.

## Routing

Bridge: `framework-source-rules` main Java; `java-test-rules` tests; `ci-failure-investigator` CI; `flaky-test-stabilizer` flaky; `release-dependency-guard` release/deps; `graphify` graph; `shaft-ui-design` UI; `shaft-marketing-ad-producer` ads; `public-behavior-docs-synchronizer` docs; `mcp-transport-contract-auditor` MCP; `modular-boundary-auditor` modules; `allure-extent-report-operator` reports; `agent-guidance-boundary-guard` guidance.

## New Task Flow

For edits: preserve work; fetch/prune; branch fresh `codex/*` from `origin/main`; before PR sync default, resolve conflicts, rerun checks; commit, push, open ready PR. Draft only if blocked/incomplete/requested.

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

Issue #3292: 8 of 10 skills/MCP servers adopted (configured in `.claude/settings.json`, `.mcp.json`).

- `jdtls-lsp`: Java language server; precise symbol navigation across Maven reactor and shaft-intellij.
- `design` plugin: WCAG audits, design-critique, UX copywriting for Swing UI and docs DESIGN_LANGUAGE.md.
- `frontend-design`: Distinctive visual design guidance for docs React components.
- `mcp-server-dev`: Best practices for MCP server design (shaft-mcp has 80+ tools, published to registry).
- `chrome-devtools-mcp`: Performance traces, network analysis, console debugging for docs UI and AutoBot.
- `context7` (MCP): Version-pinned library docs on demand (Spring Boot 4.1, Spring AI 2.0, JUnit 6, Selenium 4.45, etc.).
- `maven-tools-mcp` (MCP): Maven Central tools (version lookups, upgrade recommendations, license/CVE) for release automation.
- `webapp-testing`: Playwright verification workflow for screenshot/browser-log evidence on UI/report PRs.

**Not configurable here (separate follow-up):**
- JetBrains IntelliJ IDEA MCP server (#5): Manual enable in IDEA 2025.2+ via Settings | Tools | MCP Server.
- `a11ymcp` (#7): For shafthq.github.io docs repo; requires separate PR with axe-core accessibility testing.

## Completion

Report changes/checks/outcomes.
