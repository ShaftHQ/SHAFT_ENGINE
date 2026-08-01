# Failure-analysis playbook

## Ten practices

1. Verify that the intended test ran and that populated result artifacts identify the build, environment, and attempt. [`ALLURE-RESULTS`, `SHAFT-REPORTING`]
2. Preserve original results, logs, traces, screenshots, network data, and configuration before reproducing or cleaning anything. [`SHAFT-REPORTING`]
3. Read the first decisive error, complete stack trace, failed step, assertion, and status semantics literally. [`ALLURE-RESULTS`]
4. Correlate the failure timeline with environment, data, browser or device state, dependencies, and recent changes. [`SHAFT-REPORTING`, `GOOGLE-SRE-TESTING`]
5. Classify the current evidence as product, test, data, environment, dependency, or unknown without forcing certainty. [`ISTQB-CTFL`, `ALLURE-RESULTS`]
6. List competing hypotheses and choose the next observation that best distinguishes them. [`GOOGLE-SRE-MONITORING`]
7. Reproduce the smallest affected scope under equivalent conditions before changing code or configuration. [`ISTQB-CTFL`, `SELENIUM-PRACTICES`]
8. Compare passing history, failing history, retries, changes, and sibling failures to find the earliest divergence. [`ALLURE-STABILITY`]
9. Fix or recommend action at the owner of the root cause; do not weaken assertions, widen waits, or replace locators merely to force green. [`SELENIUM-PRACTICES`, `ISTQB-CTFL`]
10. Report cause, confidence, supporting and contradictory evidence, impact, and the exact check that will verify resolution. [`ISTQB-TM`, `SHAFT-REPORTING`]

## Examples

- Diagnose a `NoSuchElementException` by checking frame, state, locator uniqueness, timing trace, and recent UI changes.
- Distinguish an API assertion failure caused by changed product behavior from stale expected data or a broken dependency.
- Show that an empty `allure-results` directory means execution is unproven despite a stale generated report saying passed.

## Boundary case

- If decisive artifacts are missing, return “root cause undetermined,” list the minimum evidence or reproduction needed, and do not fabricate a likely fix.

## Output

Return classification, timeline, hypotheses tested, root cause or unknowns,
confidence, affected owner, corrective action, and verification check.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
