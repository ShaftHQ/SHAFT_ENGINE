# Copilot Memory Ledger

> Historical/non-normative context only. Current rules live exclusively in `/AGENTS.md` and `/docs/ai/`; when an entry conflicts, the current authoritative instructions and executable configuration win.

Purpose: retain only verified, high-signal investigation heuristics that are likely to recur and are not already enforced by authoritative policy, tests, or runbooks.

## Admission Rules
Add an entry only when all are true:
- The lesson is supported by a file, test, workflow run, issue, or PR.
- It changes how future work should be investigated or validated.
- It is durable across tasks and cannot be expressed better as an authoritative policy or automated check.

Do not add task summaries, progress notes, speculative conclusions, copied logs, secrets, or duplicate policy. Prefer updating `docs/ai/` or a validator when a lesson is prescriptive. Review entries only when investigating the matching area.

## Entry Template
- Date:
- Area:
- Trigger:
- Investigation heuristic:
- Evidence:

## Entries

### Mock-only GUI coverage tests
- Date: 2026-05-09
- Area: WebDriver lifecycle / coverage tests
- Trigger: A mock-backed coverage test created a real Selenium Grid session through a GUI facade default constructor.
- Investigation heuristic: When a coverage-only unit test unexpectedly reaches Grid or browser startup, inspect constructor references first. GUI facade default constructors may invoke the driver factory; use an explicit mock-driver constructor and leave session creation to integration tests.
- Evidence: `src/test/java/testPackage/unitTests/ElementActionsCoverageUnitTest.java`

### TestNG `ISuite` isolation on Java 25
- Date: 2026-05-10
- Area: TestNG listener unit tests
- Trigger: Mockito/proxy access to `org.testng.ISuite` pulled optional Guice types and broke an otherwise focused Java 25 unit test.
- Investigation heuristic: If a listener test needs only method metadata, avoid mocking or proxying `ISuite`. Extract list/value-level logic and test it directly.
- Evidence: `src/main/java/com/shaft/listeners/internal/TestNGListenerHelper.java`, `src/test/java/testPackage/unitTests/TestNGListenerHelperCoverageUnitTest.java`

### Realtime reporter cross-class lifecycle
- Date: 2026-05-10
- Area: Realtime reporter / static server tests
- Trigger: Realtime reporter tests failed only under method-parallel Grid runs because separate TestNG classes shared one static server lifecycle.
- Investigation heuristic: `@Test(singleThreaded = true)` serializes methods only within one class. Tests that start/stop the static `RealtimeReporter` server need a shared cross-class lock, readiness polling, and property restoration.
- Evidence: `src/test/java/testPackage/unitTests/RealtimeReporterServerUnitTest.java`, `src/test/java/testPackage/unitTests/RealtimeReporterUnitTest.java`, `src/test/java/testPackage/unitTests/RealtimeReporterTestLock.java`
