# Architecture

## Technology Stack
- Java library built and published as a JAR with Maven (`pom.xml`).
- JDK 25 is enforced by Maven Enforcer; `.java-version` and `.sdkmanrc` select JDK 25.
- UI automation: Selenium WebDriver, Appium, Flutter finder, WebDriverManager, Healenium, Applitools, Shutterbug, OpenCV, and axe-core.
- API automation: REST Assured, Allure REST filter, Swagger request validation, JSONPath/JSONAssert, Jackson/Gson, and GraphQL helper methods.
- Test execution: TestNG (primary), JUnit Jupiter, Cucumber, Mockito, Surefire, and JaCoCo.
- Data/tools: JDBC drivers for MySQL, SQL Server, PostgreSQL, and DB2; Apache POI, Commons CSV, PDFBox, YAML/JSON readers; JSch for remote CLI support.
- Reporting/observability: Allure, Log4j 2, ReportPortal integration, screenshots/video, and an embedded realtime HTML dashboard.
- Security/integrations: Google Tink/KMS support, BrowserStack, LambdaTest, Jira/Xray, Firestore/telemetry-related support.

## Codebase Structure
- `src/main/java/com/shaft/driver/SHAFT.java`: primary consumer-facing namespace and fluent entry point.
- `src/main/java/com/shaft/driver/internal/`: fluent driver helpers and factory internals.
- `src/main/java/com/shaft/gui/`: browser/element/mobile action layers and internal locators, image processing, video, waits, and exceptions.
- `src/main/java/com/shaft/api/`: session-aware HTTP request/response support.
- `src/main/java/com/shaft/cli/` and `src/main/java/com/shaft/db/`: command/file and JDBC test-target adapters.
- `src/main/java/com/shaft/validation/`: hard/soft fluent validation builders.
- `src/main/java/com/shaft/properties/internal/`: typed property interfaces, defaults, file loading, caches, and thread-local overrides.
- `src/main/java/com/shaft/listeners/`: framework lifecycle entry points for TestNG, JUnit, Cucumber, and WebDriver.
- `src/main/java/com/shaft/tools/`: IO, reports, integrations, security, telemetry/support, and runtime utilities.
- `src/main/resources/properties/default/`: packaged configuration defaults.
- `src/main/resources/docker-compose/`: optional Selenium Grid, Selenoid, Healenium, Postgres, and ReportPortal stacks.
- `src/main/resources/k8s/`: Selenium Grid/KEDA examples, not the deployment of a hosted SHAFT service.
- `src/main/resources/examples/`: downstream usage examples.
- `src/test/java/` and `src/test/resources/`: unit, integration, E2E, Cucumber, browser/mobile/cloud, fixtures, suites, and test data.
- `.github/workflows/`: quality, dependency review, E2E matrices, JavaDocs, Maven Central release, links, coverage readiness, and maintenance automation.

## Main Modules and Responsibilities
### Public facade and lifecycle
`SHAFT` exposes nested namespaces such as GUI, API, CLI, DB, TestData, Validations, Properties, and Report. `DriverFactory` creates local, remote, or mobile drivers; listeners manage configuration, reports, retries, and lifecycle cleanup.

### GUI automation
Browser and element actions wrap Selenium/Appium with fluent chaining, waits, screenshots, locators, visual comparison, video, accessibility, and driver event listeners. The framework controls test drivers; it is not the UI under test.

### API automation
`RestActions` and `RequestBuilder` build request specifications, session headers/cookies/configuration, multipart/body data, execute HTTP verbs through REST Assured, capture response details, and expose validations. Authentication values belong to the caller/target system and must not be embedded in framework code.

### CLI and database adapters
CLI code executes commands and manipulates files for tests. `DatabaseActions` opens JDBC connections to external test databases and returns/validates query results. The repository does not own those schemas.

### Configuration and reporting
Typed OWNER interfaces under `properties/internal` define configurable behavior. Packaged defaults live in resources; thread-local overrides isolate parallel tests. Report utilities write Allure steps/attachments and Log4j output; listeners coordinate result generation.

### Embedded frontend asset
`src/main/resources/realtime/dashboard.html` is a self-contained HTML/CSS/JavaScript realtime results dashboard served by framework runtime code. Test HTML files under `src/test/resources/testDataFiles/` are fixtures, not product frontend pages.

## Important Data Flows
1. **Test startup:** TestNG/JUnit/Cucumber listener loads properties and initializes reporting/integrations.
2. **GUI:** consumer creates `SHAFT.GUI.WebDriver` → `DriverFactory` resolves configured execution target/capabilities → browser/element helpers invoke Selenium/Appium → listener/report utilities record evidence → `quit()` releases resources.
3. **API:** consumer creates `RestActions` → request builder combines base URI, service path, headers/cookies/config/body → REST Assured filter sends request and records report data → response is validated and session state may be updated.
4. **Database:** consumer supplies connection details → JDBC connection executes target-system SQL → results are exposed to framework validations → connection must be closed.
5. **Properties:** packaged/default/project properties are materialized as typed interfaces; per-thread overrides take precedence for isolated test configuration and are cleared after execution.
6. **Reports:** actions emit SHAFT log/report events → Allure result JSON and attachments are generated → optional realtime dashboard/report tooling presents execution state.

## Deployment and Runtime
SHAFT is published to Maven Central and runs inside consumer test processes. `.github/workflows/mavenCentral_cd.yml` creates a GitHub release and deploys signed artifacts after pushes to `main`; it skips tests during deployment and relies on prior validation. JavaDocs are published separately. Docker Compose and Kubernetes files provision optional test infrastructure such as Selenium Grid; they are not a persistent SHAFT application deployment.

Not found in current codebase scan: application server framework, inbound HTTP routes/controllers, ORM/repositories, migration framework, business-domain database, message queue, or long-running background worker service.

## Architecture Rules for AI
- Keep facade changes additive and delegate implementation to focused modules.
- Preserve fluent return types, listener lifecycle ordering, and public JavaDocs.
- Add configuration through typed property interfaces/defaults; avoid new static global switches.
- Keep external-system details configurable and never couple framework behavior to one customer or environment.
- Treat parallel execution and cleanup as architectural requirements.
- Update nearby unit/E2E tests and examples when public behavior changes.
- Do not edit deployment/release assets for ordinary feature work.
- Inferred from codebase: the embedded realtime dashboard is intentionally dependency-free and packaged inside the JAR; preserve that packaging model unless maintainers request a frontend toolchain.
- Public APIs must remain source- and binary-compatible with the latest LTS Java version supported by executable configuration; deprecated APIs remain available for at least three calendar months before removal.
- Browser, mobile, API, database, cloud-provider, Cucumber, reporting, and other broad matrices are scheduled/non-blocking release validation; they do not block publication.
