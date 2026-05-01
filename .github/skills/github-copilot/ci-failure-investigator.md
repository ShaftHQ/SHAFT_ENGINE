# Skill: CI Failure Investigator

## Skill Name

`ci-failure-investigator`

## Description

Ingests GitHub Actions workflow run/job logs, summarizes actionable root causes, maps failures to likely source files and classes, and proposes a minimal fix strategy with a rerun checklist and risk assessment.

## Trigger Examples

- "Investigate why the latest CI run failed on this PR."
- "Summarize the root cause of workflow run #12345."
- "What caused the test failures in the `e2e_tests` workflow?"

---

## Step 1 — Gather CI Context

1. **Identify the workflow run**: Use `list_workflow_runs` to find the failing run(s) for the target branch or PR.
2. **Retrieve job logs**: Use `get_job_logs` (with `failed_only=true` and `return_content=true`) to fetch logs for all failed jobs.
3. **Collect supplementary data**: If applicable, use `list_workflow_jobs` to understand the full job matrix and identify which jobs passed vs. failed.

---

## Step 2 — Classify Failure Type

Categorize each failure into one of these buckets:

| Category | Signals |
|---|---|
| **Compilation** | `BUILD FAILURE`, `cannot find symbol`, `incompatible types` |
| **Test Failure** | `Tests run:.*Failures:`, assertion errors, `expected:.*but was:` |
| **Infrastructure** | `timeout`, `connection refused`, `OOM`, `No space left on device` |
| **Dependency** | `Could not resolve dependencies`, `artifact not found` |
| **Configuration** | `Missing property`, `Profile not found`, bad YAML/XML syntax |
| **Flaky / Intermittent** | Same test passes on rerun, timing-sensitive stack traces |

---

## Step 3 — Map Failures to Source

For each failure:

1. Extract the failing class/method name and line number from the stack trace.
2. Identify the source file path (e.g., `src/test/java/testPackage/SomeTest.java`).
3. If the failure is in framework code, trace to the public API entry point the test used.
4. Flag any failures that span multiple files or modules.

---

## Step 4 — Root-Cause Analysis

Follow this order:

1. **Identify the symptom**: What error message or assertion appeared?
2. **Identify the trigger**: What change or condition caused this to fail now? (new commit, dependency update, environment drift)
3. **Identify the root cause**: What is the underlying code or configuration defect?
4. **Classify confidence**:
   - **High**: Stack trace directly points to a clear code defect or missing resource.
   - **Medium**: Failure correlates with recent changes but root cause requires code inspection.
   - **Low**: Insufficient log data; may need local reproduction.

---

## Step 5 — Propose Fix Strategy

For each root cause:

1. Propose the **smallest code change** that resolves the issue.
2. If the fix requires multiple files, list them in dependency order.
3. Flag any risks: backward compatibility, parallel-execution safety, environment-specific behavior.
4. If the failure is infrastructure-related (not a code defect), recommend the operational fix (rerun, resource increase, config change).

---

## Step 6 — Output Format

Use this exact structure:

```
## CI Failure Investigation Report

### Run Summary
| Field | Value |
|---|---|
| Workflow | [workflow name] |
| Run ID | [run ID] |
| Branch | [branch name] |
| Trigger | [push / pull_request / schedule / workflow_dispatch] |
| Failed Jobs | [count] / [total] |

### Failure Classification
| # | Job | Category | Confidence |
|---|---|---|---|
| 1 | [job name] | Compilation / Test / Infrastructure / Dependency / Config / Flaky | High / Medium / Low |

### Root Cause Analysis

#### Failure #N
| Field | Value |
|---|---|
| Job | [job name] |
| Category | [category] |
| Confidence | High / Medium / Low |
| Symptom | [error message summary] |
| Trigger | [what changed to cause this] |
| Root Cause | [underlying defect or condition] |
| Source File(s) | `path/File.java:line` |
| Proposed Fix | [minimal change description] |
| Risk | [backward compat / parallel safety / env-specific notes] |

### Rerun Checklist
- [ ] [Fix description 1] — apply and verify locally
- [ ] [Fix description 2] — apply and verify locally
- [ ] Rerun workflow after fixes are pushed
- [ ] Confirm all previously-failing jobs pass

### Risk Level
| Level | Rationale |
|---|---|
| 🟢 Low / 🟡 Medium / 🔴 High | [explanation of overall risk] |
```

---

## Guardrails

- Do not guess root causes without supporting log evidence.
- Do not recommend broad refactors when a targeted fix resolves the failure.
- Clearly separate infrastructure failures (rerun-worthy) from code defects (fix-required).
- If logs are truncated or insufficient, state what additional data is needed rather than speculating.
- Prefer SHAFT patterns and conventions when proposing code fixes.
