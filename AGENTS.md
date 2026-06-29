# AGENTS.md

## Repository

SHAFT_ENGINE is a Maven Java automation framework; config wins.
Core `shaft-engine/`; optional `shaft-*`; tools `scripts/ci/`. Start at
goal/files; prefer `rg`, excerpts, parsers.

## Routing

Load one bridge: `framework-source-rules` main Java; `java-test-rules` tests;
`ci-failure-investigator` CI; `flaky-test-stabilizer` flaky;
`release-dependency-guard` release/deps; `graphify` graph; `shaft-ui-design` UI;
`shaft-marketing-ad-producer` ads; `public-behavior-docs-synchronizer` docs;
`mcp-transport-contract-auditor` MCP; `modular-boundary-auditor` modules;
`allure-extent-report-operator` reports; `agent-guidance-boundary-guard` guidance.

## New Task Flow

For edits: preserve work; fetch/prune; branch fresh `codex/*` from `origin/main`;
before PR sync default, resolve conflicts, rerun checks; commit, push, open ready
PR. Draft only when blocked/incomplete/requested.

## Working Rules

- Read first; follow patterns; tight scope; preserve user work.
- Use structured APIs for structured data.
- Reproduce defects; add focused regression coverage.
- Preserve public API; deprecate before removal.
- Docs repo `C:\Users\Mohab\IdeaProjects\shafthq.github.io`; targeted
  `rg`/excerpts. Function changes need guide + docs PR.
- Never expose secrets or run deploy/publish/rewrites/cleanup/cloud suites unless asked.
- No generated reports, binaries, or `target/`; browser tests headless unless
  headed approved.

## Windows/Codex Safety

No GUI/shell-open: avoid `start`, `explorer`, `Invoke-Item`/`ii`,
`Start-Process`, `cmd /c start`, `rundll32`, `os.startfile`, browsers, editors,
installers, OS dialogs. Run files via `py -3`, `node`, `powershell -ExecutionPolicy
Bypass -File`, `Get-Content`, `mvn`, `npm`, `dotnet`, `git`. Ask before
`generate_allure_report.bat`, `allure serve`, servers/watchers, browser capture,
mobile inspector/emulator, or waits. Maven in Codex: add GUI-off Allure and
Lighthouse `-D...=false` flags.

## Memory

Memory: `.memory/`; current files win. `AGENTS.md` canonical; `CLAUDE.md`
adapts only. Load with `memory load "<task>"`/`memory search`. Save durable
decisions/constraints/gotchas/workflows/corrections with evidence; reuse IDs; no
duplicates/diaries/end saves.

## Validation

Before forked Maven/Surefire/TestNG, load gotchas. If delete gotcha is active,
avoid `mvn test`; use compile/test-compile, static checks, or disposable copy.

Use the smallest non-redundant check. Rerun passing checks only after edits/rebases/dependency changes.

- Guidance/memory: `python3 scripts/ci/validate_agent_setup.py`; Win `py -3 ...`
- Local code: affected tests, then one compile/package pass.
- Shared API/build/release: targeted, then full compile/package.
- Visual: relevant test plus image/browser evidence.
- UI/report PRs need screenshots; draft/report if blocked.
- External/cloud E2E: required infra only.

PowerShell: quote `'-Dname=value'`, `'stash@{0}'`, args with `{}`, `@`, `;`,
`&`, `|`. Confirm Allure before trusting SHAFT test verdicts.

## Completion

Report changes/checks/outcomes.
