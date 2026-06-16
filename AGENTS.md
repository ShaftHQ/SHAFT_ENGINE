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
  `rg`/excerpts, no broad loads or local public guides.
- Functionality changes require user-guide updates and a separate docs PR.
- Never expose secrets or run deployment, publication, history rewrites,
  cleanup, or credentialed cloud suites unless explicitly required.
- Do not commit generated reports, binaries, or `target/`.
- Run browser tests headlessly unless headed validation is explicitly approved.
- Start new edit/PR tasks from a fresh branch off `origin/main`; use a clean
  worktree if dirty, then implement, validate, and PR.

## Memory

Memory lives in `.memory/`; current code, tests, config, and the request win.

- Use memory proactively when material; do not wait for a user request.
- Do not load memory for routine, self-contained work.
- Use at most one `load_memory` call when history, pitfalls, durable
  instructions, or continuity matter and files do not answer it.
  Add file/subsystem hints; budget <= 600.
- Save only novel durable decisions, constraints, gotchas, workflows, or user
  corrections with evidence. Do not save diaries, transient status, or facts
  clear from code or active guidance.
- Do not save at session end; use `remember_memory` when the preceding rule
  applies.

## Validation

Use the smallest check that proves the change:

- Guidance or memory: `python3 scripts/ci/validate_agent_setup.py`
- Localized code: affected tests, then one compile/package pass.
- Shared API, concurrency, build, or release: targeted checks, then full
  compile/package.
- Visual behavior: relevant test plus image/browser evidence.
- External/cloud E2E: only when infrastructure is available and required.

On PowerShell, single-quote Maven `-D` arguments. Confirm Allure result files
are populated before trusting SHAFT test verdicts.

## Completion

Report changes, checks, outcomes, and unverified risk. Verify remote claims.
