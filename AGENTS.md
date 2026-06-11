# AGENTS.md

## Purpose

This is the canonical repository policy for coding agents. Keep it short: load
task-specific details only when the task needs them.

SHAFT_ENGINE is a Maven-published Java 25 test-automation framework. Executable
configuration (`pom.xml`, `.java-version`, `.sdkmanrc`, `.mvn/jvm.config`, and
workflows) overrides prose when they disagree.

## Repository Map

- `shaft-engine/`: main framework source, tests, resources, and compatibility artifact.
- `shaft-browserstack/`, `shaft-video/`, `shaft-visual/`: optional modules.
- `shaft-bom/`, `legacy-shaft-engine/`, `report-aggregate/`: BOM, relocation, and reporting modules.
- `docs/`: architecture and operational runbooks.
- `scripts/ci/`: deterministic CI validators and report tools.
- `.github/instructions/`: path-scoped Java rules.
- `.github/skills/`: task playbooks loaded only when relevant.

Use the [framework source rules](.github/instructions/framework-source.instructions.md)
for production Java and the [Java test rules](.github/instructions/java-tests.instructions.md)
for test Java.

## Cost And Context

- Start with the user goal and directly relevant files. Expand context only to
  resolve a concrete uncertainty.
- Prefer `rg`, targeted excerpts, structured parsers, and batched independent
  reads before broad scans.
- Reuse existing command output and repository facts. Do not reread unchanged
  files or repeat checks without a reason.
- Prefer deterministic local scripts before web, MCP, GitHub, browser, cloud,
  subagent, image, or additional-model calls.
- Load one matching skill when needed; do not preload every playbook.
- Use host-reported usage data when available. Never invent token counts,
  prices, remaining context, or tool billing.
- For long tasks, check usage only at phase boundaries or before optional
  expensive work. Cost never overrides correctness, security, or required
  verification.
- Stop when the acceptance criteria are proved.

## Working Policy

- Read before editing and follow established patterns. Keep changes focused.
- Preserve user changes in a dirty worktree. Never revert unrelated work.
- Use structured APIs for XML, JSON, YAML, and other structured data.
- Add dependencies or abstractions only when the task requires them.
- For bugs, reproduce the failure and add focused regression coverage when
  practical before changing behavior.
- Update documentation when public behavior changes.
- For each new GitHub ticket assigned by the user, sync the latest default
  branch, create a dedicated branch from it, implement and validate the work,
  then open a pull request.
- Perform GitHub, cloud, deployment, release, or destructive operations only
  when the task explicitly requires them.

## Safety

- Never expose secrets from environment variables, credentials, Maven/GPG
  configuration, provider accounts, or local files.
- Do not run Maven Central deployment, JavaDoc publication, history rewrites,
  destructive cleanup, or credentialed cloud suites during ordinary work.
- Treat `target/`, reports, downloaded binaries, and generated assets as
  disposable outputs; do not commit them unless explicitly requested.
- Clean up Docker or local services started for a task.

## Engineering Invariants

- Preserve public API compatibility; use a deprecation cycle for removals or
  renames.
- Public framework APIs require JavaDoc.
- Route configurable values through SHAFT properties; do not hardcode versions,
  URLs, timeouts, or behavior switches.
- Use SHAFT logging/reporting utilities, not `System.out` or silent catches.
- Keep thread and test state isolated; clear `ThreadLocal` state at lifecycle
  boundaries.
- Keep WebDriver capabilities W3C-compliant and vendor options namespaced.

## Validation By Risk

Use the smallest check that proves the change, then broaden only when blast
radius warrants it.

| Change                                     | Required evidence                                                                 |
|--------------------------------------------|-----------------------------------------------------------------------------------|
| Agent/docs only                            | Relevant deterministic validator, reference check, `git diff --check`             |
| Localized code                             | Affected test(s), then one compile/package pass                                   |
| Shared API, concurrency, build, or release | Targeted tests, relevant module checks, full compile/package                      |
| Visual behavior                            | Relevant test plus image or browser evidence                                      |
| External/cloud E2E                         | Run only when infrastructure is available and the result proves required behavior |

Common commands:

```bash
mvn -pl shaft-engine -am test -Dtest=TestClassName
mvn clean install -DskipTests -Dgpg.skip
mvn -pl shaft-engine javadoc:javadoc
python3 scripts/ci/validate_agent_guidance.py
```

For SHAFT tests, verify Allure result files are populated before trusting the
verdict. Surefire is supporting diagnostics because test failures may be
ignored to allow report generation.

## On-Demand Playbooks

- [CI failure investigation](.github/skills/ci-failure-investigator/SKILL.md)
- [Flaky test stabilization](.github/skills/flaky-test-stabilizer/SKILL.md)
- [Release and dependency guard](.github/skills/release-dependency-guard/SKILL.md)
- [Agent guidance maintenance](docs/AGENT_GUIDANCE_MAINTENANCE.md)

## Completion

Report changed behavior, files, checks and outcomes, plus any unverified risk or
environment limitation. Do not claim a visible branch, issue, PR, or remote
result unless it was verified.
