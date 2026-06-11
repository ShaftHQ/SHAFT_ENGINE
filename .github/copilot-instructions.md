# SHAFT_ENGINE Copilot Instructions

Follow `AGENTS.md` as the canonical repository policy. Never expose secrets,
overwrite unrelated changes, run deployment/release commands, or claim remote
GitHub results that were not verified.

## Efficient Execution

- Read only files needed for the current task; expand context for a concrete
  uncertainty.
- Prefer `rg`, targeted excerpts, batched reads, and deterministic local scripts.
- Reuse existing evidence. Avoid repeated builds, test runs, screenshots,
  network calls, tools, skills, or model turns without a new validation need.
- Use host-reported usage only; do not invent token counts, context, prices, or
  billing.
- Load one relevant playbook from `.github/skills/` rather than copying its
  instructions into the conversation.

## Engineering

- Preserve public API compatibility and document public classes and methods.
- Keep changes focused and follow existing SHAFT patterns.
- Put configurable values in the SHAFT properties layer; do not hardcode them
  or call `System.getProperty()` directly in framework code.
- Use SHAFT reporting/logging utilities, specific exceptions, and no silent
  catches or `System.out`.
- Isolate parallel state and clear `ThreadLocal` values at lifecycle boundaries.
- Use namespaced, W3C-compliant WebDriver capabilities.

Path-specific Java rules are in:

- `.github/instructions/framework-source.instructions.md`
- `.github/instructions/java-tests.instructions.md`

## Validation

- Docs/agent guidance: run its deterministic validator and `git diff --check`.
- Local code: run affected tests once, then compile/package once.
- Shared APIs, concurrency, build, or release logic: broaden to relevant module
  and full build checks.
- Capture visual evidence only for visual behavior. Use external/cloud E2E only
  when available and necessary.
- Confirm Allure results are populated before using them as the SHAFT verdict.

Commands:

```bash
mvn -pl shaft-engine -am test -Dtest=TestClassName
mvn clean install -DskipTests -Dgpg.skip
python3 scripts/ci/validate_agent_guidance.py
```

Task playbooks:

- `.github/skills/ci-failure-investigator/SKILL.md`
- `.github/skills/flaky-test-stabilizer/SKILL.md`
- `.github/skills/release-dependency-guard/SKILL.md`
