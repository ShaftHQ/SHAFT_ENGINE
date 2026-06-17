# AGENTS.md

## Repository

SHAFT_ENGINE is a Maven-published Java 25 automation framework.
Config overrides prose.

`shaft-engine/` is the core module; sibling `shaft-*` directories contain
optional or publication modules. Deterministic tools live in `scripts/ci/`.
Canonical docs are at `https://shaftengine.netlify.app`.

Start from goal and files. Prefer `rg`, excerpts, parsers, scripts. Expand only
for concrete uncertainty.

## Routing

Load only the matching `.agents/skills/` workflow:

- Production Java: `framework-source-rules`
- Test Java: `java-test-rules`
- CI or Allure failure: `ci-failure-investigator`
- Intermittent test: `flaky-test-stabilizer`
- Release or dependency work: `release-dependency-guard`

Bridge skills point to canonical `.github/` rules; do not preload them.

## Working Rules

- Read first, follow patterns, keep scope tight, and preserve unrelated work.
- Use structured APIs for structured data.
- Reproduce defects and add focused regression coverage.
- Preserve public API compatibility; deprecate before removal/rename.
- Public docs: `C:\Users\Mohab\IdeaProjects\shafthq.github.io`; use targeted
  `rg`/excerpts only.
- Functionality changes require user-guide updates plus docs PR.
- Before PRs, refresh graphify for core/docs after file/relationship changes;
  report blockers.
- Never expose secrets or run deployment, publication, history rewrites,
  cleanup, or credentialed cloud suites unless explicitly required.
- Do not commit generated reports, binaries, or `target/`.
- Run browser tests headlessly unless headed validation is explicitly approved.
- Start new edit/PR tasks from a fresh branch off `origin/main`; use a clean
  worktree if dirty, then implement, validate, and PR.

## Memory

Memory: `.memory/`; current files win.

- Load memory proactively only when history, pitfalls, durable instructions, or
  continuity matter; use one hinted call, budget <= 600.
- Save only durable decisions, constraints, gotchas, workflows, or user
  corrections with evidence. No diaries or end-session saves.

## Validation

Before forked Maven/Surefire/TestNG validation, load memory gotchas. If
`gotcha.surefire-testng-deletes-module` is active, avoid `mvn test` in a SHAFT
worktree; use compile/test-compile, static checks, or a disposable copy.

Use the smallest non-redundant check that proves the change; do not rerun
passing checks unless later edits, rebases, or dependency changes invalidate
them.

- Guidance or memory: `python3 scripts/ci/validate_agent_setup.py`
- Localized code: affected tests only, then one compile/package pass.
- Shared API, concurrency, build, or release: targeted checks, then full
  compile/package.
- Visual behavior: relevant test plus image/browser evidence.
- External/cloud E2E: only when infrastructure is available and required.

PowerShell: quote `'-Dname=value'`, `'stash@{0}'`, and args with `{}`, `@`,
`;`, `&`, or `|`. Confirm Allure results before trusting SHAFT test verdicts.

## Completion

Report changes, checks, outcomes, and unverified risk. Verify remote claims.
