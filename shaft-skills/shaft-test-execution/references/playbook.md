# Test-execution playbook

## Ten practices

1. Confirm the approved scope, build, plan version, configuration, environment, data, and execution authority. [`ISTQB-CTFL`, `ISTQB-TM`]
2. Run readiness checks and record blockers before starting tests, not after ambiguous failures appear. [`GOOGLE-SRE-MONITORING`, `SHAFT-REPORTING`]
3. Preserve prior evidence according to retention policy and keep each launch's artifacts attributable. [`ALLURE-RESULTS`, `ALLURE-STABILITY`]
4. Execute the smallest risk-revealing scope first, then expand only when its entry gate passes. [`ISTQB-TM`, `SELENIUM-PRACTICES`]
5. Use controlled, reproducible, headless execution by default; use headed, device, cloud, or external runs only when scope requires and authority permits. [`SHAFT-GUIDE`, `APPIUM`]
6. Record test, build, commit, environment, data, time, duration, worker, and configuration identity. [`ISTQB-CTFL`, `ALLURE-RESULTS`]
7. Capture sufficient logs, steps, assertions, screenshots, traces, and attachments while redacting sensitive values. [`SHAFT-REPORTING`, `ALLURE-RESULTS`]
8. Preserve passed, failed, broken, skipped, unknown, blocked, and not-run distinctions; never collapse them into pass. [`ALLURE-RESULTS`, `ISTQB-CTFL`]
9. Make retries and manual reruns visible, keep every attempt, and never use a later pass to erase an earlier failure. [`ALLURE-STABILITY`]
10. Reconcile selected, started, completed, and status totals against exit criteria before declaring the run complete. [`ISTQB-TM`, `ALLURE-RESULTS`]

## Valid examples

- Run one changed login method headlessly, inspect its populated Allure result, then run the affected class.
- Execute negative API contract cases against a versioned build and preserve request/response evidence with secrets masked.
- Run an approved mobile matrix while recording device, OS, orientation, app build, capability, and attempt identity.

## Boundary case

- Refuse destructive production tests, unapproved external suites, or hidden reruns; return the required authority, safety control, or missing precondition.

## Output

Return the exact command or tool operation, scope, identities, reconciled
status counts, artifacts, retries, blockers, and exit-criterion result.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
