---
applyTo: "**/src/test/java/**/*.java"
---

# Java Test Rules

- Follow the existing TestNG, JUnit, or Cucumber style in the affected module.
  Use descriptive scenario-based test names.
- Browser tests create a fresh driver per test and clean it in an always-run
  teardown. For `ThreadLocal` drivers, call both `quit()` and `remove()`.
- Browser tests must run headlessly. Do not start a headed browser during
  ordinary agent validation; inherently headed-only behavior requires explicit
  user approval before execution.
- Under TestNG method parallelism, ordinary instance fields are shared by
  concurrent methods. Keep per-method paths, files, mocks, drivers, and mutable
  setup in locals or `ThreadLocal`; cleanup must read and clear the same
  thread-owned state.
- `@Test(singleThreaded = true)` serializes one class only. Static services or
  SHAFT global properties shared across classes need an explicit cross-class
  lock or prerequisites set inside each test, followed by always-run
  restoration.
- Use `Properties.clearForCurrentThread()` when tests set per-thread SHAFT
  properties.
- Preserve the live `allure-results` directory and delete only its contents.
  Replacing the root can race with Allure writers on Windows.
- Confirm result JSON files are populated before interpreting Allure status.
  When retries occur, inspect non-passed attempts as well as the final summary.
- Prefer SHAFT fluent assertions and existing test-data conventions. Store
  reusable data in module test resources instead of embedding large fixtures.
- Coverage-only unit tests must not call facade/default constructors that create
  real browser sessions. Use explicit mock-backed constructors; keep session
  creation in integration tests.
- Avoid mocking or proxying `org.testng.ISuite` on Java 25 when list/value helper
  logic can be tested directly.
- Mock external services when the contract is not under test. Disable status
  validation only when response status is intentionally outside the assertion.
- For a bug fix, first prove the regression when practical, then run the focused
  test after the fix. Repeat or parallelize runs only when needed to evaluate a
  flaky or concurrency failure.
- Reuse passing test evidence unless later edits affect the tested behavior or
  its dependencies.
- Capture screenshots only when the tested behavior is visual.
- For flaky failures, use
  [the stabilization playbook](../skills/flaky-test-stabilizer/SKILL.md).
