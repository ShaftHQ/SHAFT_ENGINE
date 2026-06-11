# Archived Skill: Code Analysis and Optimization

## Skill Name

`code-analysis-and-optimization`

## Description

A SHAFT-focused analysis skill for identifying reliability and performance issues, ranking root causes, and proposing
minimal-risk fixes with clear validation steps.

## High-Value Enhancements for Accuracy

### 1) Root-Cause-First Diagnosis

- Distinguish symptom, trigger, and root cause for each issue.
- Require code-location evidence before assigning high confidence.
- Separate primary cause from secondary suspects.

### 2) Confidence Scoring

- **High**: direct anti-pattern match in provided code.
- **Medium**: partial evidence with probable mapping.
- **Low**: missing critical files/flows.

### 3) Fix Ordering by Risk

- Prioritize by impact and blast radius: leaks/flakiness first, then maintainability.
- Recommend minimal viable fix first, then optional improvements.

### 4) Validation-Centric Output

- Every suggested fix includes explicit “how to verify” checks.
- Include regression checks for adjacent code paths.

## SHAFT-Critical Checks

1. `ThreadLocal` cleanup requires both `quit()` and `remove()`.
2. `@AfterMethod(alwaysRun = true)` for reliable cleanup.
3. Prefer SHAFT assertion API over raw test framework asserts.
4. Reused locators should be cached as class-level `static final` values.
5. Global `SHAFT.Properties.flags` mutations must be isolated and restored.

## Structured Output

```
## SHAFT Code Analysis Report

### Summary
- Total issues found: [count]
- Critical: [n] | High: [n] | Medium: [n] | Low: [n]
- Confidence: High / Medium / Low

### Recommended Fix Order
1. ...

### Issue #[N]
| Field | Value |
|---|---|
| Severity | Critical / High / Medium / Low |
| Confidence | High / Medium / Low |
| Category | ThreadLocal / WebDriver / Assertion / Locator / Concurrency / Performance / TestDesign |
| Location | `path/File.java:line` |
| Description | Root-cause explanation |
| Impact | Runtime or maintenance impact |
| Fix | Minimal corrected snippet |
| Validation | Exact checks/tests to run |
```

## Trigger Examples

- “Find why this SHAFT test is flaky and propose a minimal fix.”
- “Analyze this file for ThreadLocal and teardown leaks.”
- “Rank the top fixes needed for this failing SHAFT test class.”

## Limitations

- Static-analysis driven; runtime profilers are not executed automatically.
- Output quality depends on provided context coverage.
- Private repositories/files cannot be fetched by the helper script without accessible raw content.
