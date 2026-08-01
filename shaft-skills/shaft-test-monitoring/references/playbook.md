# Test-monitoring playbook

## Ten practices

1. Establish the approved scope, baseline, milestones, thresholds, and data source before interpreting progress. [`ISTQB-TM`]
2. Track selected, started, completed, passed, failed, broken, skipped, blocked, and not-run counts separately. [`ISTQB-CTFL`, `ALLURE-RESULTS`]
3. Measure requirement, risk, platform, data, and test-type coverage against the plan, not raw case count alone. [`ISTQB-TM`]
4. Monitor open defects, severity, aging, fix verification, and residual product risk. [`ISTQB-TM`]
5. Track duration, queue time, setup time, resource saturation, worker imbalance, and dependency health. [`ALLURE-RESULTS`, `GOOGLE-SRE-MONITORING`]
6. Track retry volume, status transitions, flake rate, quarantine, and recurring failure signatures over history. [`ALLURE-STABILITY`]
7. Apply explicit quality gates and show both the measured value and threshold behind each decision. [`ISTQB-TM`]
8. Alert on actionable user-visible symptoms and attach context; avoid noisy cause guesses without evidence. [`GOOGLE-SRE-MONITORING`]
9. Reprioritize or suspend testing when risk, scope, build quality, capacity, or evidence invalidates the plan, and record the control decision. [`ISTQB-TM`]
10. Publish timestamped snapshots with source links so progress and completion claims are reproducible. [`ISTQB-TM`, `SHAFT-REPORTING`]

## Valid examples

- Monitor a pull-request gate for completion, new failures, retries, duration regression, and required-platform coverage.
- Track a release regression cycle by critical journey, residual risk, blocker aging, and exit criteria.
- Monitor a nightly matrix for device capacity, worker imbalance, flaky clusters, and unexecuted combinations.

## Boundary case

- If result files, selection data, or plan baselines are absent, report the metric as unavailable and the run as unverified; never infer green from a quiet dashboard.

## Output

Return a timestamped progress snapshot, source and threshold for each measure,
exceptions, control actions, forecast, and completion status.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
