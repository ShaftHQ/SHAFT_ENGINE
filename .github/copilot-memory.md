# Copilot Memory Ledger

Purpose: keep high-signal, reusable learnings from implementation sessions in one canonical place.

## Entry Template
- Date:
- Area: (e.g., Allure, Properties, CI workflow, WebDriver lifecycle)
- Trigger: (bug/regression/request)
- Lesson:
- Evidence: (file path, test class, workflow, issue/PR)
- Action taken: (instruction update, test added, code guardrail)

## Entries
- Date: 2026-05-07
- Area: Encoding / Unicode
- Trigger: Unicode text rendered incorrectly when tests were run by automated agents on different hosts.
- Lesson: Enforce UTF-8 at all runtime layers (Maven JVM, Surefire forked JVMs, and CI job environment) instead of relying on host defaults.
- Evidence: `pom.xml`, `.mvn/jvm.config`, `.github/actions/setup-test-env/action.yml`, `src/test/java/testPackage/properties/Log4jTests.java`
- Action taken: Added explicit UTF-8 JVM flags and coverage assertions to prevent regressions.

- Date: 2026-05-08
- Area: Agent knowledge migration / instruction hygiene
- Trigger: Request to scan repository skills, tools, instructions, and memory for a smooth AI-agent migration without knowledge loss.
- Lesson: Use `docs/AGENT_KNOWLEDGE_MIGRATION.md` for future migration tasks: create or link a ticket, branch before editing, progressively scan instruction/memory/skill files, favor executable configuration over stale prose, update the memory ledger, commit, and open a PR; if GitHub publication access is missing, explicitly report the limitation and provide a PR handoff.
- Evidence: `docs/AGENT_KNOWLEDGE_MIGRATION.md`, `docs/tickets/2026-05-08-agent-knowledge-migration.md`, `.github/copilot-instructions.md`, `CLAUDE.md`
- Action taken: Added a migration runbook, local ticket, global instruction reference, Claude guidance, and this memory entry.

- Date: 2026-05-09
- Area: WebDriver lifecycle / coverage tests
- Trigger: Grid E2E failures in `ElementActionsCoverageUnitTest.shouldCoverBasicFluentAndActionWrappers`.
- Lesson: Coverage-only unit tests must not call GUI facade default constructors such as `ElementActions::new`; those constructors create real browser sessions through the driver factory and can produce Selenium Grid failures even when the test otherwise uses mocks. Prefer explicit mock-driver constructors for unit coverage and keep all driver-factory/session paths in focused integration tests.
- Evidence: `src/test/java/testPackage/unitTests/ElementActionsCoverageUnitTest.java`
- Action taken: Removed the default-constructor coverage invocation so the test remains fully mock-backed and session-isolated.

- Date: 2026-05-10
- Area: Test validation / Allure reporting
- Trigger: GitHub Actions run `25620020633` grid failure investigation and a local run whose browser-visible Allure report was empty.
- Lesson: For SHAFT Engine, Allure result JSON/report output is the single source of truth for test verdicts. Before analyzing failures, first verify the run is populated by counting executed tests/result JSON files; an empty or unexpectedly small Allure report invalidates any status analysis.
- Evidence: `AGENTS.md`, `.github/instructions/java-tests.instructions.md`, `.github/copilot-instructions.md`, run `25620020633`
- Action taken: Updated durable test-validation instructions and this memory entry.

- Date: 2026-05-10
- Area: Allure / Windows parallel reporting
- Trigger: Local Windows Allure report showed BROKEN results from `AllureResultsWriteException: Could not create Allure results directory` while parallel TestNG methods were writing results.
- Lesson: Preserve the live `allure-results` directory root and clean its contents instead of deleting/recreating the root. On Windows, replacing the root can race with Allure's `FileSystemResultsWriter.ensureInitialized()` and surface as `FileAlreadyExistsException`.
- Evidence: `src/main/java/com/shaft/tools/io/internal/AllureManager.java`, `src/test/java/testPackage/unitTests/AllureManagerUnitTest.java`
- Action taken: Updated Allure cleanup behavior, added regression coverage, and recorded the guidance in `AGENTS.md`.

- Date: 2026-05-10
- Area: TestNG / Java 25 unit-test isolation
- Trigger: `TestNGListenerHelperCoverageUnitTest.setTotalNumberOfTestsShouldSkipCucumberRunScenarioSuites` broke when Mockito/proxy attempts touched `org.testng.ISuite` optional Guice types.
- Lesson: Avoid mocking or proxying `org.testng.ISuite` on Java 25 in this repo. Extract list/value-level helper behavior and test that directly when only `ITestNGMethod` data is needed.
- Evidence: `src/main/java/com/shaft/listeners/internal/TestNGListenerHelper.java`, `src/test/java/testPackage/unitTests/TestNGListenerHelperCoverageUnitTest.java`
- Action taken: Added a list-based helper behind `setTotalNumberOfTests(ISuite)` and updated durable guidance in `AGENTS.md`.

- Date: 2026-05-10
- Area: Realtime reporter / static server tests
- Trigger: Grid run `25620020633` had realtime reporter server failures only under method-parallel Selenium Grid jobs.
- Lesson: `@Test(singleThreaded = true)` does not protect static server lifecycle shared across different TestNG classes. Unit tests that start/stop `RealtimeReporter` need an explicit cross-class lock plus readiness polling and property restoration.
- Evidence: `src/test/java/testPackage/unitTests/RealtimeReporterServerUnitTest.java`, `src/test/java/testPackage/unitTests/RealtimeReporterUnitTest.java`, `src/test/java/testPackage/unitTests/RealtimeReporterTestLock.java`
- Action taken: Serialized realtime reporter unit tests with a package-local lock and added server readiness polling.
- Date: 2026-05-12
- Area: CI E2E failure triage / Allure artifact traversal / PR authorship
- Trigger: Issue #2696 required investigating failing E2E workflow runs from GitHub Actions artifacts, and a follow-up request asked agents to preserve the investigation workflow and make Codex-authored PRs explicit.
- Lesson: Start E2E triage with authenticated `gh run view`, failed job logs, and artifact metadata; self-contained `*_Allure.html` artifacts can be parsed directly by decoding embedded `data/test-results/*.json` payloads, which is much faster than browser traversal. Always count populated Allure results before trusting statuses, distinguish provider/credential failures from code defects, and label Codex-authored branches/PRs so human token owners are not implied as manual authors.
- Evidence: `docs/CI_FAILURE_INVESTIGATION.md`, `AGENTS.md`, issue #2696, PR #2699
- Action taken: Added a CI/Allure investigation runbook, linked it from repository agent guidance, and recorded PR title/body authorship guidance for future Codex-created work.

- Date: 2026-05-13
- Area: Agent start/init workflow / PR traceability
- Trigger: PR follow-up comment asked `@codex init` to make future start/init mentions follow the explicit assignment, linked-branch, draft-PR, checkpoint-commit, and review-notification process.
- Lesson: Treat maintainer mentions with `start` or `init` as a request to initialize the full task workflow: assign to `codex` when possible (otherwise the requester), create a GitHub-linked branch for auto-close traceability when an issue exists, open an early draft PR with a lower-intelligence-agent-ready plan, execute at least three PDCA iterations for non-trivial work, commit at validated checkpoints, then push final status and notify the requester for review.
- Evidence: `AGENTS.md`, `.github/copilot-instructions.md`, PR follow-up trigger `@codex init` on 2026-05-13.
- Action taken: Added explicit start/init protocol guidance to repository agent instructions and recorded this reusable lesson in the memory ledger.

- Date: 2026-05-13
- Area: Release preparation / version metadata
- Trigger: First Codex-generated release PR request for `10.2.20260513`.
- Lesson: Release PRs are metadata-sensitive and must be opened directly once validated. Always inspect recent merged release PRs, compute the version with `{major}.{quarter}.{YYYYMMDD}`, update root `pom.xml`, `Internal.java` `shaftEngineVersion`, verify/update `allure3Version` and `nodeLtsVersion`, and update all seven sample/demo project `<shaft_engine.version>` values before opening the PR. Do not rely on the post-release sample-sync workflow to fix stale sample POMs.
- Evidence: PRs #2502, #2494, #2439, #2428, #2418; `pom.xml`; `src/main/java/com/shaft/properties/internal/Internal.java`; `src/main/resources/examples/**/pom.xml`; `.github/copilot-instructions.md`; `AGENTS.md`.
- Action taken: Added release-preparation notes to agent instructions, framework-source instructions, and this memory entry while preparing the release branch.

- Date: 2026-05-23
- Area: Remote WebDriver capabilities / Selenium Grid 4 compatibility
- Trigger: Follow-up PR iterations for Grid warnings about legacy non-W3C `enableVideo` capability handling.
- Lesson: Never emit top-level `enableVideo`; keep video configuration in namespaced capability payloads only (`selenoid:options`, `moon:options`, `se:recordVideo`). When namespaced maps already exist, merge and preserve existing keys instead of overwriting provider options.
- Evidence: `src/main/java/com/shaft/driver/internal/DriverFactory/OptionsManager.java`, `src/test/java/com/shaft/driver/internal/DriverFactory/OptionsManagerCoverageUnitTest.java`, `AGENTS.md`.
- Action taken: Added helper-based namespaced capability merging in `OptionsManager`, kept tests aligned with no top-level `enableVideo`, and added durable guidance to `AGENTS.md`.

- Date: 2026-06-05
- Area: TestNG parallelism / Selenium Grid E2E triage / image test isolation
- Trigger: E2E run `27004138692` had one `Ubuntu_MicrosoftEdge_Grid` BROKEN result in `ImageProcessingActionsUnitTest.compareImageFoldersShouldPassWhenImagesAreIdentical`.
- Lesson: In TestNG `setParallel=METHODS`, ordinary instance fields are shared by concurrently running methods in the same class. If `@BeforeMethod` writes a per-method temp path to a shared field and `@AfterMethod` deletes from that field, another method can delete files still being read, surfacing as misleading image IO errors such as `javax.imageio.IIOException: Can't create an ImageInputStream!`. Use `ThreadLocal` for per-method temp paths or serialize classes that intentionally mutate shared/static state. Also inspect retried/skipped Allure result JSON and adjacent job logs, because the final summary can pass after retries while exposing the race.
- Evidence: `src/test/java/testPackage/unitTests/ImageProcessingActionsUnitTest.java`, `.github/workflows/e2eTests.yml`, run `27004138692`.
- Action taken: Converted the test temp directory state to `ThreadLocal`, added CI/test instruction guidance, and validated with the Edge Grid workflow properties.
