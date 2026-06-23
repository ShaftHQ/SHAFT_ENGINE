# AGENTS.md

## Repository

SHAFT_ENGINE is a Maven-published Java 25 automation framework; config wins.
Core: `shaft-engine/`; optional/publication: `shaft-*`; tools: `scripts/ci/`; docs: `https://shaftengine.netlify.app`.

Start from goal/files. Prefer `rg`, excerpts, parsers, scripts. Expand only for uncertainty.

## Routing

Load matching `.agents/skills/`: production Java `framework-source-rules`; test
Java `java-test-rules`; CI/Allure `ci-failure-investigator`; intermittent
`flaky-test-stabilizer`; release/dependency `release-dependency-guard`.

Bridge skills point to `.github/`; do not preload.

## New Task Flow

For file edits unless told otherwise:

1. Clean stale Codex worktrees/branches: delete only clean worktrees and unused merged/closed branches; preserve user work.
2. Fetch/prune; fast-forward `main` from `origin/main`.
3. Branch fresh `codex/*` from `origin/main`.
4. Implement, validate, commit, push, and open a ready PR when checks pass;
   draft only when blocked, incomplete, or requested.
5. Share the PR link.

## Working Rules

- Read first; follow patterns; tight scope; preserve user work.
- Delegate Codex Spark when useful; no permission needed. Own review.
- Structured APIs for structured data.
- Reproduce defects and add focused regression coverage.
- Preserve public API; deprecate before removal/rename.
- Public docs: `C:\Users\Mohab\IdeaProjects\shafthq.github.io`; targeted `rg`/excerpts.
  Functionality changes need guide update plus docs PR.
- Before PRs, refresh graphify for core/docs changes; report blockers.
- Never expose secrets or run deploy, publish, history rewrites, cleanup, or cloud suites unless explicit.
- No generated reports, binaries, or `target/`.
- Browser tests headless unless headed approved.

## Memory

Memory: `.memory/`; current files win. `AGENTS.md` is canonical; `CLAUDE.md`
imports/adapts only. Load for history, pitfalls, durable instructions, or
continuity: `memory load "<task>"` or `memory search`. Save only durable
decisions, constraints, gotchas, workflows, or corrections with evidence. Reuse IDs; no duplicates, diaries, or end saves.

## Validation

Before forked Maven/Surefire/TestNG, load gotchas. If the delete gotcha is active, avoid `mvn test`; use compile/test-compile, static checks, or a disposable copy.

Use the smallest non-redundant check. Do not rerun passing checks unless edits,
rebases, or dependencies invalidate them.

- Guidance/memory: `python3 scripts/ci/validate_agent_setup.py`; Win: `py -3 ...`
- Localized code: affected tests only, then one compile/package pass.
- Shared API/concurrency/build/release: targeted, then full compile/package.
- Visual: relevant test plus image/browser evidence.
- External/cloud E2E: only with required infra.

PowerShell: quote `'-Dname=value'`, `'stash@{0}'`, and args with `{}`, `@`,
`;`, `&`, `|`. Confirm Allure before trusting SHAFT test verdicts.

## Completion

Report changes/checks/outcomes/risk; verify remote claims.
