# Execution-reporting playbook

## Ten practices

1. Name the engineering audience, reporting period, test objective, and decision this report supports. [`ISTQB-TM`]
2. Record provenance: plan, run, build, commit, environment, data, configuration, start, stop, and artifact locations. [`ISTQB-TM`, `ALLURE-RESULTS`]
3. Reconcile selected, started, completed, passed, failed, broken, skipped, blocked, unknown, and not-run totals. [`ALLURE-RESULTS`, `ISTQB-CTFL`]
4. Report requirement, risk, platform, data, and test-type coverage against the approved scope. [`ISTQB-TM`]
5. Group failures by evidence-backed signature and distinguish product, test, data, environment, dependency, and unknown causes. [`ALLURE-RESULTS`, `SHAFT-REPORTING`]
6. Show retry, status-transition, flake, quarantine, duration, and infrastructure trends without hiding unstable passes. [`ALLURE-STABILITY`]
7. Link each confirmed defect, severity, owner, state, affected test, and fix-verification status. [`ISTQB-TM`]
8. Link concise evidence and exact reproduction or focused rerun instructions instead of pasting unbounded logs. [`SHAFT-REPORTING`]
9. State quality-gate values, thresholds, exceptions, blockers, residual risks, and confidence explicitly. [`ISTQB-TM`]
10. End with prioritized technical actions, owners, and next checks; never claim an unobserved execution or resolution. [`ISTQB-TM`, `GOOGLE-SRE-MONITORING`]

## Examples

- Summarize a pull-request run with exact suite counts, one new product failure, two broken tests, retry history, and artifact links.
- Compare a browser matrix by platform, duration, failure signature, unexecuted combinations, and gate outcome.
- Report a regression cycle with risk coverage, defect verification, flake trend, environment incidents, and exit criteria.

## Boundary case

- If raw evidence is empty, stale, or mismatched to the claimed build, report the run as unverified and request the missing provenance; do not calculate a pass rate.

## Output

Return provenance, reconciled results, coverage, clusters, defects, stability,
gate status, risks, evidence links, and owned next actions.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
