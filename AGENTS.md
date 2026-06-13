# AGENTS.md

## Repository

SHAFT_ENGINE is a Maven-published Java 25 test-automation framework.
Executable configuration (`pom.xml`, `.java-version`, `.sdkmanrc`,
`.mvn/jvm.config`, and workflows) overrides prose.

Key locations:

- `shaft-engine/`: core framework, tests, resources, and compatibility artifact.
- `shaft-browserstack/`, `shaft-video/`, `shaft-visual/`: optional modules.
- `shaft-bom/`, `legacy-shaft-engine/`, `report-aggregate/`: publication modules.
- `scripts/ci/`: deterministic validators and report tools.
- `https://shaftengine.netlify.app`: canonical product and maintainer docs,
  sourced from `ShaftHQ/shafthq.github.io`.

Start with the user goal and directly relevant files. Prefer `rg`, targeted
excerpts, structured parsers, and existing local scripts. Expand context only
to resolve a concrete uncertainty, and reuse valid command evidence.

## Routing

Codex discovers task guidance in `.agents/skills/`. Load only the matching
skill:

- Production Java: `framework-source-rules`
- Test Java: `java-test-rules`
- CI failures: `ci-failure-investigator`
- Flaky tests: `flaky-test-stabilizer`
- Releases or dependency currency: `release-dependency-guard`

The bridge skills point to canonical rules under `.github/instructions/` and
`.github/skills/`; do not preload or duplicate those bodies.

## Working Rules

- Read before editing, follow existing patterns, and keep changes focused.
- Preserve user changes in a dirty worktree. Never revert unrelated work.
- Use structured APIs for XML, JSON, YAML, and other structured data.
- Reproduce bugs and add focused regression coverage when practical.
- For public behavior changes, update the Docusaurus repository and link its PR.
- Do not add local public guides or non-root README files.
- Preserve public API compatibility; removals and renames require deprecation.
- Never expose secrets or credentials.
- Do not run deployment, publication, history rewrites, destructive cleanup,
  or credentialed cloud suites unless the user explicitly requires them.
- Do not commit `target/`, reports, downloaded binaries, or generated assets
  unless explicitly requested.
- Run browser tests headlessly; headed-only validation requires user approval.
- For a user-assigned GitHub ticket, sync the default branch, create a dedicated
  branch, implement and validate the work, then open a pull request.

## Validation

Use the smallest check that proves the change:

- Guidance/docs: relevant validator, documentation-boundary check, and
  `git diff --check`.
- Localized code: affected tests, then one compile/package pass.
- Shared API, concurrency, build, or release: targeted tests, module checks,
  then full compile/package.
- Visual behavior: relevant test plus image or browser evidence.
- External/cloud E2E: only when infrastructure is available and required.

Common commands:

```bash
mvn -pl shaft-engine -am test -Dtest=TestClassName
mvn clean install -DskipTests -Dgpg.skip
mvn -pl shaft-engine javadoc:javadoc
python3 scripts/ci/validate_agent_guidance.py
python3 scripts/ci/validate_documentation_boundaries.py
```

On PowerShell, single-quote every Maven `-D` argument. For SHAFT tests, confirm
Allure result files are populated before trusting the verdict; Surefire may be
supporting diagnostics because failures can be ignored for report generation.

## Completion

Report changed behavior, files, checks and outcomes, plus unverified risk or
environment limits. Claim branch, issue, pull-request, or remote results only
after verification.
