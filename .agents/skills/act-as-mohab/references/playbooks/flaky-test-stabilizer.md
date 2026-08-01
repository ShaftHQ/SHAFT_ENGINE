---
name: flaky-test-stabilizer
description: Diagnose and stabilize intermittent SHAFT tests, especially TestNG parallelism, shared state, files, drivers, timing, and external dependencies.
---

# Flaky Test Stabilizer

Use this playbook for intermittent, retry-only, parallel-only, or
environment-sensitive failures.

## Workflow

1. Gather the exact failure, runner mode, retry history, affected test, and
   setup/teardown code. Do not begin with broad refactoring.
2. Check high-probability causes in this order:
    - per-method state stored in shared instance or static fields;
    - `ThreadLocal` values not removed;
    - one method deleting another method's files;
    - global SHAFT properties or static services shared across classes;
    - fixed sleeps or missing readiness polling;
    - unordered/random assertions or external-service dependencies;
    - browser sessions created accidentally by unit-test constructors.
3. Remember that TestNG `singleThreaded` isolates methods in one class, not
   static state shared with other classes. Use a cross-class lock where a
   static service genuinely cannot run concurrently.
4. Preserve the `allure-results` root during cleanup. Inspect retried/skipped
   result JSON when the final report is green.
5. Fix the ownership or synchronization defect without changing what the test
   validates. Prefer local variables, `ThreadLocal`, explicit locks, readiness
   polling, deterministic fixtures, and mocked external dependencies.
6. Run the focused test once. Repeat it or enable method parallelism only when
   repetition directly tests the suspected flake mechanism; stop after enough
   evidence establishes stability.

## Output

State the failure signature, root cause and confidence, minimal fix, focused
commands run, repetition/parallel evidence if applicable, and residual risk.
