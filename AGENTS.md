# AGENTS.md

## Repository

SHAFT_ENGINE is a Maven-published Java 25 automation framework; config wins.
Core: `shaft-engine/`; optional/publication: `shaft-*`; tools: `scripts/ci/`.
Start from goal/files; prefer `rg`, excerpts, parsers.

## Routing

Load only matching `.agents/skills/`: src/main Java
`framework-source-rules`; src/test Java `java-test-rules`; CI/Allure
`ci-failure-investigator`; flaky `flaky-test-stabilizer`; release/deps
`release-dependency-guard`; mapping `graphify`; UI
`shaft-ui-design`; ads `shaft-marketing-ad-producer`.
Bridge skills point to `.github/` or docs; do not preload.

## New Task Flow

For file edits:

1. Delete only clean unused merged/closed Codex worktrees/branches; preserve user work.
2. Fetch/prune; fast-forward `main` from `origin/main`; branch fresh `codex/*`.
3. Before PR: fetch remote default; merge/rebase, resolve conflicts, rerun checks.
4. Commit, push, open ready PR; draft only when blocked/incomplete/requested; share link.

## Working Rules

- Read first; follow patterns; tight scope; preserve user work.
- Delegate Spark when useful; no permission needed; own review.
- Use structured APIs for structured data.
- Reproduce defects; add focused regression coverage.
- Preserve public API; deprecate before removal/rename.
- Public docs: `C:\Users\Mohab\IdeaProjects\shafthq.github.io`; targeted
  `rg`/excerpts. Functionality changes need guide update + docs PR.
- Never expose secrets or run deploy, publish, history rewrites, cleanup, cloud suites unless explicit.
- No generated reports, binaries, or `target/`; browser tests headless unless
  headed approved.

## Memory

Memory: `.memory/`; current files win. `AGENTS.md` is canonical; `CLAUDE.md`
adapts only. Load history, pitfalls, durable instructions, or continuity with
`memory load "<task>"` or `memory search`. Save only durable decisions,
constraints, gotchas, workflows, or corrections with evidence; reuse IDs; no duplicates/diaries/end saves.

## Validation

Before forked Maven/Surefire/TestNG, load gotchas. If the delete gotcha is
active, avoid `mvn test`; use compile/test-compile, static checks, or disposable copy.

Use the smallest non-redundant check. Rerun passing checks only after edits/rebases/dependency changes.

- Guidance/memory: `python3 scripts/ci/validate_agent_setup.py`; Win: `py -3 ...`
- Localized code: affected tests, then one compile/package pass.
- Shared API/concurrency/build/release: targeted, then full compile/package.
- Visual: relevant test plus image/browser evidence.
- UI/reporting PRs must include real screenshot evidence in the PR body or a PR
  comment: before/after for revamps, after-only for new UI. If capture is
  blocked, keep the PR draft or explicitly report the blocker.
- External/cloud E2E: required infra only.

PowerShell: quote `'-Dname=value'`, `'stash@{0}'`, and args with `{}`, `@`,
`;`, `&`, `|`. Confirm Allure before trusting SHAFT test verdicts.

## Completion

Report changes/checks/outcomes/risk; verify remote.
