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

## Examples

- Run one changed login method headlessly, inspect its populated Allure result, then run the affected class.
- Execute negative API contract cases against a versioned build and preserve request/response evidence with secrets masked.
- Run an approved mobile matrix while recording device, OS, orientation, app build, capability, and attempt identity.

## Boundary case

- Refuse destructive production tests, unapproved external suites, or hidden reruns; return the required authority, safety control, or missing precondition.

## Output

BrowserStack: `browserStack.buildName`/`projectName` group sessions; use one
build name per app build or CI run. The generated `browserstack.yml` never holds
credentials (SDK reads `BROWSERSTACK_USERNAME`/`BROWSERSTACK_ACCESS_KEY`); still
check `git status` after a cloud run. Cloud runs use the provider video, not
Appium `videoParams_recordVideo`.

Return the exact command or tool operation, scope, identities, reconciled
status counts, artifacts, retries, blockers, and exit-criterion result.

## Execution class

- The default run is the test being fixed: that method, or that class when a method cannot be selected. A full language and platform matrix runs only when the consumer's written delivery contract names those cohorts. Call that matrix a consumer contract, not the SHAFT default.
- Do not restart a cohort whose code and device are unchanged and whose result is already classified. An unclassified skip or a partial session is not packaged.
- The verdict is the Surefire or TestNG report. Maven exit 0 is not success when `testFailureIgnore` is set.
- Select tests with `-Dtest` (`Class#method`, or a comma-separated class list) or a `suiteXmlFiles` value the project already owns. Do not add a TestNG XML file. Do not separate classes with `+`.
- Do not poll a long suite with short waits. One watcher reports a cohort copy or device loss.
- A non-blank `withCustomReportMessage` is on the thrown `AssertionError`. A raw driver exception still needs the defect id on that throwable.
- Cold-boot a wedged emulator with `-no-snapshot-load`. `-no-snapshot-save` is not a cold boot. Recycle cadence is pressure-or-wedge: only when the memory cap killed qemu or a snapshot or renderer wedge is detected. Do not recycle per method or between long suites. No host qemu time-versus-memory measurement was available when this cadence was chosen.
- One device has one Appium session and one Maven run. Stop the Appium this agent started when the suite ends, including after a failure. The managed lease serializes processes it owns. Do not start a second Appium beside a process the lease did not start.
- A later run deletes `allure-results` when `allure.cleanResultsDirectory` is true (the default). Keep a previous cohort by setting that property false for the next run, or by copying the directory into the project workspace first (for example `target/allure-results-kept`). A copy outside the project is not the supported keep path.
- Decode base64 report blobs before a secret scan. A hit blocks publish and must not echo the secret. A plaintext grep is not success. Checking `browserstack.yml` is not that scan (#6826 stays: that file must not hold credentials).
- A delivery set is finished when each result passed, failed with a named id, or was blocked with a named id. An all-green filter must not build that report. Scenario status Done in the journey coverage map still means a passing run. That sentence is unchanged. An unclassified skip is not finished.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
