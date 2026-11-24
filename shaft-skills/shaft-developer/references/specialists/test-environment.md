# Test environment

Use when defining or troubleshooting SHAFT test environment readiness, parity, versions, configuration, dependencies, isolation, reset, or drift.

Emulator cold-boot, memory pressure, and the one-device Appium rule: [mobile session policy](../mobile-session-policy.md).

## Ten practices

1. Inventory the system under test, services, networks, browsers, devices, drivers, databases, accounts, and external dependencies. [`ISTQB-CTFL`, `APPIUM`]
2. Define required production parity and document every intentional difference and its coverage risk. [`ISTQB-TM`, `GOOGLE-SRE-TESTING`]
3. Pin and record application, schema, runtime, browser, driver, device, operating-system, and test-framework versions. [`APPIUM`, `SHAFT-GUIDE`]
4. Keep environment-specific behavior in reviewed configuration, not hardcoded test logic. [`SHAFT-GUIDE`, `GOOGLE-SRE-TESTING`]
5. Automate bounded readiness checks for reachability, authentication, capacity, clock, certificates, dependencies, and test data. [`GOOGLE-SRE-MONITORING`]
6. Isolate concurrent runs and allocate enough CPU, memory, storage, ports, sessions, and device capacity. [`ISTQB-TM`, `GOOGLE-SRE-TESTING`]
7. Inject secrets through approved stores and redact credentials and personal data from diagnostics. [`OWASP-WSTG`, `SHAFT-REPORTING`]
8. Make provisioning, reset, teardown, and recovery repeatable and safe after interrupted runs. [`GOOGLE-SRE-TESTING`]
9. Capture logs, traces, screenshots, network evidence, and environment metadata needed to distinguish product, test, and infrastructure failures. [`SHAFT-REPORTING`, `ALLURE-RESULTS`]
10. Detect drift, record changes, and revalidate the baseline before comparing results across runs. [`GOOGLE-SRE-TESTING`, `ISTQB-TM`]

## Examples

- Define a Chrome, Firefox, and Edge matrix with pinned versions and known browser-specific exclusions.
- Prepare Android emulator and real-device capabilities with app build, automation driver, locale, orientation, and cleanup rules.
- Build readiness checks for an API, database migration, message broker, seeded tenant, and observability endpoints.

## Boundary case

- Do not install toolchains, start paid cloud capacity, or mutate shared infrastructure without explicit approval; report the exact missing readiness condition and safe next action.

## Output

Return an environment manifest, parity risks, readiness checks, isolation and
reset rules, evidence paths, drift status, and blockers.

## Execution class

- A journey installs the supplied binary even when `versionCode` matches. Do not compile the app from the automation repository.
- On a real-device cloud session, refuse a simulator binary (`iphonesimulator`) and an emulator-only Android ABI (`x86` or `x86_64` with no device ABI) before upload or session start. Pin the immutable uploaded build id. A latest-upload alias is not the capability. A simulator session may use a simulator binary. `browserStack.buildName` grouping is not a substitute for that id.
- Isolation between healthy tests clears app data in the same Appium session (`terminateApp`, `mobile: clearApp`, `activateApp`), never a new session, server, or emulator; device control is Appium-only, never `adb`, for cloud parity. Details: [mobile actions](mobile-actions.md) (#7071).
- A mock proxy (for example mitmdump) is owned by the suite: start it in `@BeforeSuite`, stop it in `@AfterSuite(alwaysRun = true)` and a shutdown hook. Reset the device proxy in suite setup too, because a killed run leaves it pointing at a dead port. Name mocked tests `*Mocked` and keep at least one real-data assertion per feature (#7086).
- Setup enables the tracked secret guard once per clone from the SHAFT template `shaft-engine/src/main/resources/examples/tools/git-hooks` (generated projects copy that directory to their hooksPath). Their `.gitignore` covers `*allure*.html`, `allure-results/` and `allure-report/`. Older projects copy both from the same template. Secret values stay in the gitignored files that `SECRET_GUARD_FILES` names (#7177).
