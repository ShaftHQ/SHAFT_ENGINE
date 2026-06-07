---
name: bug-fix
description: Use this skill only for reproducing and fixing an observed SHAFT defect with focused regression coverage.
---

# Bug Fix

## Purpose
Reproduce an actual defect, lock it down with a failing regression test when practical, apply the smallest root-cause fix, and prove that existing behavior remains intact.

## When to Use
Use for incorrect, flaky, crashing, leaking, insecure, or regressed framework/dashboard behavior. Do not use for new features disguised as fixes.

## Required Docs to Read
1. Read only `docs/ai/context.md` first.
2. Read `docs/ai/testing-policy.md` and `docs/ai/coding-standards.md`.
3. Read `docs/ai/security-policy.md` if the defect involves credentials, reports, input handling, encryption, isolation, API/DB/CLI, or external integrations.
4. Read `docs/ai/architecture.md` only if root cause crosses modules/lifecycle/data flow.
5. Source/test-specific rules are consolidated in the two policies above; do not look for `.github/instructions/`.

Read only the required docs for this task. Inspect only impacted modules and nearby related files. Do not scan the full repository unless explicitly required for the task or safety. Stop gathering context once the acceptance criteria and validation path are clear. Keep changes minimal. Report if more context is needed.

## Workflow
1. Restate expected versus actual behavior and collect a minimal reproduction.
2. Inspect the failing entry point, direct execution path, lifecycle/reporting hooks, and existing related tests.
3. Add a focused regression test and run it against the unfixed code to confirm failure. If impractical, record why and capture another reproducible check.
4. Identify root cause; distinguish it from symptoms and unrelated cleanup.
5. Apply the smallest compatible fix without broad refactoring.
6. Run the regression test; verify populated Allure results for Maven test runs.
7. Run neighboring regression tests and mandatory compile/package validation.
8. Check parallel execution, resource cleanup, sensitive logging, and public API compatibility.
9. Document before/after evidence and any environment-dependent validation still needed.

## Validation Checklist
- [ ] Reproduction is documented.
- [ ] Regression test failed before and passes after, or limitation is explicit.
- [ ] Fix addresses root cause with minimal scope.
- [ ] Existing public behavior/API is preserved outside the defect.
- [ ] Failure, cleanup, and concurrency paths are considered.
- [ ] Allure results are populated and inspected.
- [ ] Focused and neighboring tests pass.
- [ ] `mvn clean install -DskipTests -Dgpg.skip` passes for Java changes.
- [ ] No secrets/generated artifacts/unrelated edits are present.

## Forbidden Actions
- Fixing by disabling assertions, retries, security checks, reporting, cleanup, or failing tests.
- Expanding into unrelated refactors or dependency/version upgrades.
- Hardcoding one environment's browser, endpoint, credential, timing, or data.
- Claiming a fix without reproduction/validation evidence.
- Deleting a flaky test rather than addressing isolation or root cause.

## Final Response Format
- **Changed files:** source, regression tests, fixtures/docs.
- **What changed:** reproduction, root cause, and minimal fix.
- **Tests run:** before/after and regression commands/results.
- **Tests not run:** environment-dependent paths and reason.
- **Risks:** remaining edge cases, compatibility, concurrency/security.
- **Rollback notes:** revert path and expected return of original defect.
