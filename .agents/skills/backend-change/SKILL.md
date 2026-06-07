---
name: backend-change
description: Use this skill only for changes to SHAFT Java framework modules, public APIs, properties, integrations, listeners, CLI, API, database, or reporting logic.
---

# Backend / Framework Change

## Purpose
Safely change the Java library while preserving public API compatibility, fluent behavior, execution isolation, reporting, and downstream consumer expectations. This repository has no hosted application backend; “backend” means framework code under `src/main/java/com/shaft/`.

## When to Use
Use for feature or behavior changes in Java framework source. For a defect, use `bug-fix` instead. Do not use for the embedded dashboard, review-only work, or release-only validation.

## Required Docs to Read
1. Read only `docs/ai/context.md` first.
2. Read `docs/ai/coding-standards.md` and `docs/ai/testing-policy.md`.
3. Read `docs/ai/architecture.md` only for module/data-flow/deployment impact.
4. Read `docs/ai/security-policy.md` only for credentials, API/DB/CLI input, reporting, encryption, external integrations, or execution isolation.
5. The former source/test path-scoped rules are consolidated in the two policies above; do not look for `.github/instructions/`.

Read only the required docs for this task. Inspect only impacted modules and nearby related files. Do not scan the full repository unless explicitly required for the task or safety. Stop gathering context once the acceptance criteria and validation path are clear. Keep changes minimal. Report if more context is needed.

## Workflow
1. Define the requested contract, compatibility constraints, and affected facade namespace.
2. Inspect the target class, direct caller/facade, nearby internal helpers, matching properties, lifecycle/report hooks, and focused tests.
3. Confirm whether the change needs a typed property rather than a hardcoded value.
4. Design the smallest additive implementation; prefer an overload or internal helper over a breaking signature change.
5. Add/update JavaDoc for public API and focused tests for success, failure, cleanup, and concurrency as applicable.
6. Implement using established logging/reporting and exception patterns.
7. Run the narrowest affected test, inspect populated Allure results, then run the mandatory Maven compile/package check.
8. Run broader browser/API/DB/Cucumber/cloud validation only when the risk warrants it and the environment supports it.
9. Review the diff for unrelated changes, generated artifacts, secrets, and backward compatibility.

## Validation Checklist
- [ ] Public API and fluent return types remain compatible.
- [ ] Public JavaDoc is complete.
- [ ] Configuration uses SHAFT Properties, not direct system-property access.
- [ ] Resources and thread-local state are cleaned up.
- [ ] Sensitive values are excluded/redacted from reports.
- [ ] Focused tests pass and Allure results are populated.
- [ ] `mvn clean install -DskipTests -Dgpg.skip` passes.
- [ ] `mvn javadoc:javadoc` passes if public API docs changed.
- [ ] Unavailable E2E paths are explicitly listed.

## Forbidden Actions
- Broad refactors, dependency/version changes, schema changes, or release publishing without explicit scope.
- Removing/renaming public API without an approved deprecation cycle.
- Hardcoded endpoints, credentials, customer behavior, timeouts, or environment assumptions.
- Shared mutable state across parallel tests without an established isolation mechanism.
- Silent exceptions, `System.out.println`, secret logging, or generated artifact commits.

## Final Response Format
- **Changed files:** paths changed.
- **What changed:** contract and implementation summary.
- **Tests run:** exact commands and results.
- **Tests not run:** exact missing scenarios and why.
- **Risks:** compatibility, concurrency, security, integration, environment.
- **Rollback notes:** files/commit or property toggle needed to restore prior behavior.
