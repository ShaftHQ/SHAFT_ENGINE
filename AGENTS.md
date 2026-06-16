# AGENTS.md

## Repository

SHAFT_ENGINE is a Maven-published Java 25 automation framework.
Executable configuration overrides prose.

`shaft-engine/` is the core module; sibling `shaft-*` directories contain
optional or publication modules. Deterministic tools live in `scripts/ci/`.
Canonical docs are at `https://shaftengine.netlify.app`.

Start from the goal and relevant files. Prefer `rg`, targeted excerpts,
structured parsers and scripts. Expand context only for a concrete uncertainty.

## Routing

Load only the matching `.agents/skills/` workflow:

- Production Java: `framework-source-rules`
- Test Java: `java-test-rules`
- CI or Allure failure: `ci-failure-investigator`
- Intermittent test: `flaky-test-stabilizer`
- Release or dependency work: `release-dependency-guard`

Bridge skills point to canonical `.github/` rules; do not preload them.

## Working Rules

- Read before editing, follow existing patterns, keep changes focused, and
  preserve unrelated user work.
- Use structured APIs for XML, JSON, YAML, and other structured data.
- Reproduce defects and add focused regression coverage when practical.
- Preserve public API compatibility; deprecate before removing or renaming.
- Public docs live in `ShaftHQ/shafthq.github.io`; do not add local public
  guides or non-root READMEs.
- When editing, adding, or removing SHAFT functionality, update the official
  user guide repository in the same work and publish a separate docs PR when
  opening implementation PRs.
- Never expose secrets or run deployment, publication, history rewrites,
  destructive cleanup, or credentialed cloud suites unless explicitly required.
- Do not commit generated reports, binaries, or `target/`.
- Run browser tests headlessly unless headed validation is explicitly approved.
- For assigned GitHub work, sync the default branch, use a dedicated branch,
  implement and validate, then open a pull request.

## Memory

Memory lives in `.memory/`; current code, tests, config, and the request win.

- Do not load memory automatically or for routine, self-contained work.
- Use at most one `load_memory` call when history or a known pitfall is material
  and current files do not answer it. Add file/subsystem hints; budget <= 600.
- Save only novel durable decisions, constraints, gotchas, workflows, or user
  corrections with reviewable evidence. Do not save diaries, transient status,
  or facts clear from code or active guidance.
- Do not save automatically at session end; use `remember_memory` only when the
  preceding rule applies.

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
