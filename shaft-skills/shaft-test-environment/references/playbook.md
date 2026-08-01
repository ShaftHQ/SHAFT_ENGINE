# Test-environment playbook

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

## Valid examples

- Define a Chrome, Firefox, and Edge matrix with pinned versions and known browser-specific exclusions.
- Prepare Android emulator and real-device capabilities with app build, automation driver, locale, orientation, and cleanup rules.
- Build readiness checks for an API, database migration, message broker, seeded tenant, and observability endpoints.

## Boundary case

- Do not install toolchains, start paid cloud capacity, or mutate shared infrastructure without explicit approval; report the exact missing readiness condition and safe next action.

## Output

Return an environment manifest, parity risks, readiness checks, isolation and
reset rules, evidence paths, drift status, and blockers.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
