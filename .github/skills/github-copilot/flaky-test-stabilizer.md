# Skill: Flaky Test Stabilizer

## Skill Name

`flaky-test-stabilizer`

## Description

Detects intermittent test anti-patterns (timing assumptions, shared state, ThreadLocal cleanup gaps, non-deterministic assertions), proposes a stabilization plan with minimal behavior changes, and generates focused regression checks for flake-prone code paths.

## Trigger Examples

- "This test passes locally but fails intermittently in CI — help me stabilize it."
- "Analyze `TestClassName` for flakiness anti-patterns."
- "Why does this test fail only when run in parallel?"

---

## Step 1 — Gather Evidence

1. **Collect the test source**: Read the test class and any shared base class or setup/teardown methods.
2. **Collect recent run history**: If available, use `list_workflow_runs` and `get_job_logs` to find intermittent pass/fail patterns for the same test.
3. **Collect the stack trace**: Get the exact failure message and stack trace from the failing run(s).
4. **Identify the test runner**: TestNG (`@Test`, `@BeforeMethod`) or JUnit5 (`@Test`, `@BeforeEach`) — patterns differ.

---

## Step 2 — Detect Flakiness Anti-Patterns

Scan the test code for the following categories:

### A) Timing Assumptions
| Pattern | Risk |
|---|---|
| `Thread.sleep()` or fixed-delay waits | Environment speed varies; CI is often slower |
| Hardcoded timeout values without SHAFT property backing | Breaks on slow runners |
| Assertions on time-sensitive values (timestamps, durations) | Non-deterministic by nature |
| Race conditions between async operations and assertions | Action may not complete before check |

### B) Shared / Leaked State
| Pattern | Risk |
|---|---|
| `static` mutable fields without synchronization | Cross-test pollution in parallel runs |
| `ThreadLocal` set in `@BeforeMethod` but never `.remove()`d in `@AfterMethod` | Leaks across thread-pool reuse |
| `SHAFT.Properties.flags` mutations without restore in teardown | Affects all threads globally |
| Shared test data files written and read by multiple tests | File contention / stale reads |

### C) Non-Deterministic Assertions
| Pattern | Risk |
|---|---|
| Assertions on collection order without sorting | Order varies across JVM runs |
| Assertions on generated IDs, UUIDs, or random values | Never stable |
| Assertions on external service responses without mocking | Service availability varies |
| Floating-point equality without tolerance | Platform-dependent rounding |

### D) WebDriver / Browser Lifecycle
| Pattern | Risk |
|---|---|
| Missing `@AfterMethod(alwaysRun = true)` | Driver leaks on failure |
| `ThreadLocal<SHAFT.GUI.WebDriver>` not calling `.remove()` after `.get().quit()` in teardown | Stale ThreadLocal reference when thread pool reuses threads |
| Browser interactions without explicit waits | Element not ready in CI |
| Shared driver instance across test methods | State bleeds between tests |

### E) Test Isolation
| Pattern | Risk |
|---|---|
| Test method order dependency (`dependsOnMethods`) | Fragile execution graph |
| Database or API state not reset between tests | Previous test's side effects |
| Assertions on absolute file paths | Differs across OS/CI environments |
| Environment variable assumptions without defaults | Missing in CI containers |

---

## Step 3 — Classify Each Finding

For each detected anti-pattern:

| Field | Value |
|---|---|
| Category | Timing / SharedState / NonDeterministic / WebDriverLifecycle / TestIsolation |
| Severity | Critical / High / Medium / Low |
| Confidence | High (direct code match) / Medium (inferred) / Low (needs reproduction) |
| Flake Probability | How likely this pattern causes intermittent failure (High / Medium / Low) |

---

## Step 4 — Propose Stabilization Plan

For each finding, propose a fix following these principles:

1. **Minimal behavior change**: Fix the flakiness source without altering what the test validates.
2. **Prefer SHAFT built-in waits** over `Thread.sleep()` or custom polling.
3. **Prefer SHAFT assertions** over raw TestNG/JUnit assertions for better reporting.
4. **Always clean up state** in `@AfterMethod(alwaysRun = true)`.
5. **Isolate global state mutations** with `@Test(singleThreaded = true)` when modifying `SHAFT.Properties.flags`.
6. **Use `ThreadLocal` correctly**: `.set()` in setup → use in test → `.get().quit()` + `.remove()` in teardown.

Provide a concrete code diff or snippet for each proposed fix.

---

## Step 5 — Generate Regression Checks

For each stabilization fix, define:

1. **What to run**: Exact `mvn test -Dtest=TestClassName#methodName` command.
2. **How to verify stability**: Run the test N times (recommend 5–10) to confirm no intermittent failures.
3. **Parallel safety check**: Run with `-DthreadCount=4 -Dparallel=methods` to verify no parallel-execution regressions.
4. **What to watch for**: Specific symptoms that would indicate the fix is incomplete.

---

## Step 6 — Output Format

Use this exact structure:

```
## Flaky Test Stabilization Report

### Summary
| Field | Value |
|---|---|
| Test Class | `path/TestClass.java` |
| Anti-Patterns Found | [count] |
| Critical | [n] | High | [n] | Medium | [n] | Low | [n] |
| Estimated Flake Probability | High / Medium / Low |

### Anti-Pattern Findings

#### Finding #N
| Field | Value |
|---|---|
| Category | Timing / SharedState / NonDeterministic / WebDriverLifecycle / TestIsolation |
| Severity | Critical / High / Medium / Low |
| Confidence | High / Medium / Low |
| Flake Probability | High / Medium / Low |
| Location | `TestClass.java:line` |
| Pattern | [description of the anti-pattern] |
| Impact | [what breaks intermittently and why] |
| Fix | [minimal code change — snippet or diff] |

### Stabilization Plan (Ordered by Priority)
1. [Highest flake-probability fix first]
2. [Next fix]
3. ...

### Regression Checklist
- [ ] Run `mvn test -Dtest=TestClass` — all methods pass
- [ ] Run test 5–10 times consecutively — no intermittent failures
- [ ] Run with `-DthreadCount=4 -Dparallel=methods` — no parallel regressions
- [ ] Verify teardown runs on failure path (intentionally fail a test, confirm cleanup)
```

---

## Guardrails

- Do not change what a test validates — only fix how it validates or sets up state.
- Do not remove tests that are flaky — stabilize them.
- Do not introduce new external dependencies for stabilization.
- If a test is flaky due to an external service, recommend mocking rather than removing the test.
- Flag any proposed change that could mask a real bug instead of fixing flakiness.
- Prefer actionable fixes over generic "add retries" advice.
