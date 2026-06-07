# Project Glossary

## SHAFT
Definition: The primary public facade class, `com.shaft.driver.SHAFT`, whose nested namespaces expose framework capabilities.
Where used: Consumer test code and `src/main/java/com/shaft/driver/SHAFT.java`.
Related modules: Driver, GUI, API, CLI, DB, validations, properties, test data, reporting.
Notes: Preserve backward compatibility and fluent API behavior.

## GUI WebDriver
Definition: SHAFT's fluent wrapper around a Selenium or Appium driver session.
Where used: `SHAFT.GUI.WebDriver`, driver factory, browser/element/touch/alert actions, UI tests.
Related modules: `driver`, `gui`, listeners, validations.
Notes: Create per test and quit during always-run teardown.

## DriverFactory
Definition: Framework component that creates or wraps local, remote, web, and mobile driver sessions based on configuration/capabilities.
Where used: `src/main/java/com/shaft/driver/DriverFactory.java` and driver internals.
Related modules: GUI, BrowserStack, LambdaTest, Appium, Selenium Grid.
Notes: Driver lifecycle and capability changes are high risk.

## RestActions
Definition: REST Assured-based API client wrapper that manages request specifications, session headers/cookies, sending, response handling, and reporting.
Where used: `src/main/java/com/shaft/api/RestActions.java` and API tests.
Related modules: `RequestBuilder`, REST validations, Allure filter, properties.
Notes: Target-system authentication data is caller-provided and sensitive.

## RequestBuilder
Definition: Fluent builder for a SHAFT API request, including method, path, parameters, headers, body, content type, and expected status behavior.
Where used: `src/main/java/com/shaft/api/RequestBuilder.java`.
Related modules: `RestActions`, API validations, reporting.
Notes: Keep overloads and fluent return behavior compatible.

## DatabaseActions
Definition: JDBC-based adapter for connecting to and querying databases owned by systems under test.
Where used: `src/main/java/com/shaft/db/DatabaseActions.java` and database E2E tests.
Related modules: DB facade, validations, JDBC drivers.
Notes: SHAFT does not own an application schema or migrations.

## SHAFT Properties
Definition: Typed OWNER-based configuration exposed through `SHAFT.Properties`, backed by internal property interfaces and packaged defaults.
Where used: `src/main/java/com/shaft/properties/internal/` and throughout framework modules.
Related modules: listeners, driver, API, reporting, integrations.
Notes: Executable configuration overrides stale prose; do not read system properties directly in framework code.

## ThreadLocalPropertiesManager
Definition: Per-thread property override storage used to isolate parallel test configuration.
Where used: Property setters, tests, and lifecycle cleanup.
Related modules: `Properties`, listeners, parallel TestNG execution.
Notes: Clear with `Properties.clearForCurrentThread()` to prevent thread-pool leakage.

## Allure Results
Definition: JSON result files and attachments generated during test execution and used as SHAFT's authoritative result/report source.
Where used: `allure-results`, reporting/listeners, CI artifacts and investigation scripts.
Related modules: Allure listeners, ReportManager, REST filter, CI.
Notes: Verify result count before trusting statuses; Surefire may ignore test failures to finish report generation.

## Realtime Reporter
Definition: Framework runtime support that presents live execution status through the packaged dashboard HTML.
Where used: `src/main/resources/realtime/dashboard.html`, reporter/runtime classes, realtime reporter tests.
Related modules: Reporting, listeners, Allure result flow.
Notes: Inferred from codebase: intended as an embedded, dependency-free dashboard rather than a standalone web app.

## Execution Address
Definition: Configuration selecting where automation executes, such as local, Selenium Grid, or cloud providers.
Where used: Driver properties, factory/helper logic, CI E2E workflows.
Related modules: Selenium, Appium, BrowserStack, LambdaTest.
Notes: Global mutation can cause parallel-suite flakes; set prerequisites per test.

## Locator Builder
Definition: Fluent locator construction API for expressive Selenium/Appium element selection.
Where used: `src/main/java/com/shaft/gui/internal/locator/` and UI tests.
Related modules: Element actions, validations, Cucumber steps.
Notes: Prefer it for complex locators; stable Selenium `By` locators remain acceptable.

## Tinkey
Definition: SHAFT configuration/support around Google Tink authenticated encryption for test-data protection and optional KMS integration.
Where used: `src/main/java/com/shaft/tools/internal/security/`, Tinkey properties, lifecycle integration.
Related modules: Test data, properties, listeners, cloud KMS.
Notes: Keys and plaintext data must not enter source control or reports. The spelling reflects existing project naming.

## Allure 3
Definition: Report-generation tooling resolved and managed by SHAFT in addition to Java-side Allure result integrations.
Where used: Report tooling, properties, tests, CI artifacts.
Related modules: `AllureManager`, listeners, report resources.
Notes: Version alignment is release-sensitive.

## Maximum Performance Mode
Definition: A numeric optimization mode applied during property post-processing. Mode `0` makes no override; modes `1` and `2` reduce diagnostic/visual overhead, while mode `2` additionally enables headless execution except for Safari.
Where used: Properties helpers and related tests.
Related modules: Properties, reporting/visual behavior.
Notes: Modes `1`/`2` disable Healenium healing, browser auto-maximization, element highlighting, animated GIFs, video, debug mode, element-name capture, WebDriver log capture, full-log attachment, performance-report generation, and telemetry. They restrict screenshots to validation points and viewport, select AI highlighting configuration, enable watermarking, and default to headed execution before the mode-2 headless override. Do not use this mode for SHAFT framework tests, regression tests, release smoke tests, or any validation that depends on complete evidence/diagnostics.
