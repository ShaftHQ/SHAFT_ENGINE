# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.
It is a living document — update it at the end of every non-trivial session with new patterns, constraints, and learnings.

## Repository Knowledge Migration

When a session asks to scan, consolidate, migrate, or preserve repository knowledge, use `docs/AGENT_KNOWLEDGE_MIGRATION.md` as the canonical runbook. Create or link a task ticket first, create a dedicated task branch before editing, scan instruction/memory/skill/tool files with progressive disclosure, reconcile conflicts in favor of scoped instructions and executable config, update `.github/copilot-memory.md` with reusable lessons, commit the work, and open a PR for human collaboration; if GitHub publication access is unavailable, report the limitation and provide a PR handoff.

## Commands

```bash
# Build (compile + package, skip tests and GPG signing)
mvn clean install -DskipTests -Dgpg.skip

# Run a specific test class
mvn test -Dtest=TestClassName

# Run all tests
mvn test

# Generate JavaDocs
mvn javadoc:javadoc

# Check for outdated dependencies before a release
mvn versions:display-dependency-updates versions:display-plugin-updates versions:display-property-updates -DgenerateBackupPoms=false

# Side-by-side comparison (before/after a focused change)
BEFORE=<before_commit> AFTER=<after_commit> TESTS='testPackage.ClassA,testPackage.ClassB'
for C in "$BEFORE" "$AFTER"; do
  git checkout -q "$C"
  START=$(date +%s)
  mvn test -Dtest="$TESTS" -Dgpg.skip > "/tmp/compare_${C}.log" 2>&1
  END=$(date +%s)
  echo "${C},$((END-START))s"
  grep -E "Tests run:|BUILD SUCCESS|BUILD FAILURE" "/tmp/compare_${C}.log" | tail -10
done
```

**Mandatory pre-commit sequence** (no exceptions):
1. `mvn clean install -DskipTests -Dgpg.skip` must succeed.
2. Write tests for every new or changed feature.
3. `mvn test -Dtest=<YourTestClass>` must pass.

---

## Environment Requirements

> **Critical — read before attempting any build or test run.**

| Requirement | Version | Notes |
|---|---|---|
| **JDK** | **25** (exact) | Maven enforcer rejects any other JDK. The enforcer rule is `[25,26)`. |
| **Maven** | 3.9+ | Verified with 3.9.11 |
| **Node.js** | 25 (CI default) or `lts/*` (Cucumber jobs) | Used for Allure 3 CLI resolution. Not required for non-Allure local runs. |
| **Docker** | Any recent version | Required for Selenium Grid jobs (`selenium4.yml` compose). Not needed for pure unit tests. |
| **GPG** | Any | Only needed for Maven Central releases. Skip with `-Dgpg.skip` in dev. |

**⚠️ JDK 25 is strictly enforced.** If you only have JDK 21 available (e.g., in an automated agent environment):
- `mvn clean install` will fail at the enforcer step.
- Attempting `-Denforcer.skip=true` bypasses the enforcer but then fails at the compiler (`release version 25 not supported`).
- There is **no workaround** — JDK 25 must be installed to build or run tests.
- As a consequence, **fresh JaCoCo coverage numbers cannot be generated** in a JDK 21 environment; coverage data must come from CI runs.

**⚠️ Stale copilot-instructions.md**: `.github/copilot-instructions.md` says "Language: Java 21" in its overview section — this is **incorrect**. The project has been on Java 25 since at least 2026. Always trust `pom.xml` `<jdk.version>` over prose documentation.

**UTF-8 enforcement**: The CI `setup-test-env` action sets these JVM flags globally:
```
JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF-8 -Dsun.jnu.encoding=UTF-8 -Dstdout.encoding=UTF-8 -Dstderr.encoding=UTF-8
```
They are also declared in `.mvn/jvm.config` for local runs. Do not remove them — Unicode text renders incorrectly without them on non-UTF-8 hosts.

---

## Monorepo Structure (as of 2026-05-08, branch `Monorepo-Shaft-mvp` / PR #2646)

The project is being migrated from a monolithic artifact to a 6-module Maven monorepo.
Dependency chain flows strictly top-down — never add a `shaft-web` dep to `shaft-core` or `shaft-api`.

```
shaft-bom
  └─► shaft-core
        ├─► shaft-api
        ├─► shaft-db
        └─► shaft-web
              └─► shaft-engine   (thin aggregator / backwards-compat umbrella)
```

| Module | Package root | Contents |
|--------|-------------|----------|
| `shaft-bom` | — | BOM POM only — all version pins live here |
| `shaft-core` | `com.shaft.{cli,enums,listeners,performance,properties,tools,validation}` | Core framework: property system, tools, listeners, primitive validation, CLI |
| `shaft-api` | `com.shaft.api` | REST — `RestActions`, `RequestBuilder`, `ApiValidations`, REST validation builders |
| `shaft-db` | `com.shaft.db` | `DatabaseActions` |
| `shaft-web` | `com.shaft.{driver,gui,cucumber,validation(web)}` | WebDriver, Appium, Playwright, browser/element actions, web validation builders, all test suites |
| `shaft-engine` | — | Empty aggregator POM; depends only on `shaft-web`; published for backwards compat |

**Run tests after monorepo migration:**
```powershell
# Windows — must set JAVA_HOME explicitly
$env:JAVA_HOME = "C:\Program Files\Eclipse Adoptium\jdk-25.0.3.9-hotspot"
mvn test -pl shaft-web -am
```

**Where new code belongs:**
- REST API features → `shaft-api`; Database → `shaft-db`; GUI/driver/browser → `shaft-web`; Core properties/tools/enums → `shaft-core`; Version pins → `shaft-bom`; Tests → `shaft-web/src/test/`

---

## Architecture

SHAFT_ENGINE is a unified test automation framework (Java 25, Maven) built as a **batteries-included library** consumed by downstream test projects. The entire public API surfaces through one facade class: `com.shaft.driver.SHAFT`.

### Facade / Entry Point — `SHAFT.java`
`SHAFT.java` exposes namespaced nested static classes so users write `SHAFT.GUI.WebDriver`, `SHAFT.API`, etc. Never add raw public constructors or utilities outside this facade pattern for user-facing code. Internal classes live under `*/internal/` sub-packages and are not part of the public API.

| SHAFT namespace | Production package | What it does |
|---|---|---|
| `SHAFT.GUI.WebDriver` | `com.shaft.driver` + `com.shaft.gui.*` | Selenium/Appium browser + element + touch + alert actions |
| `SHAFT.API` | `com.shaft.api` | REST API testing via REST Assured |
| `SHAFT.CLI` | `com.shaft.cli` | File system (`FileActions`) + terminal (`TerminalActions`) |
| `SHAFT.DB` | `com.shaft.db` | Database operations |
| `SHAFT.Validations` | `com.shaft.validation` | Fluent hard and soft assertions |
| `SHAFT.TestData` | `com.shaft.tools.io` | JSON / Excel / CSV / YAML data readers |
| `SHAFT.Properties` | `com.shaft.properties.internal` | Runtime configuration via OWNER library |
| `SHAFT.Report` | `com.shaft.tools.io` | Custom Allure logging + attachments |

### Properties System
All configurable values are declared in interfaces under `src/main/java/com/shaft/properties/internal/` using `@Key`/`@DefaultValue` annotations (OWNER library). Access via `SHAFT.Properties.<group>.<method>()` — never `System.getProperty()` directly.

**Two-tier scoping:**
- **Engine-wide** (`Flags.java`): uses `System.setProperty` under `synchronized(Properties.class)`; affects all threads. Tests modifying these must use `@Test(singleThreaded = true)` and reset in `@AfterMethod(alwaysRun = true)`.
- **Per-thread** (`ThreadLocalPropertiesManager`): isolated per thread. Call `Properties.clearForCurrentThread()` in teardown to prevent stale state in thread-pool reuse.

Property interfaces by concern: `Flags` (toggles), `Web` (browser), `Mobile` (Appium), `API`, `Timeouts`, `Visuals`, `Reporting`/`Allure`, `Paths`, `Performance`, `Healenium`, `Internal` (version metadata).

### Batteries-Included Binary Resolution
SHAFT never requires pre-installed binaries. New tool dependencies must follow the three-tier pattern: system PATH → `npx`/package-manager → self-download to `~/.m2/repository/`. Version strings live as `@DefaultValue` in `Internal.java`; read them via `SHAFT.Properties.internal.<property>()`, never hardcoded.

### Allure 3 (Current)
Use `AllureManager.resolveAllureCommandPrefix()` — never invoke `allure` directly. `allure generate` has **no `--clean` flag** in v3; summary JSON is at `<report>/summary.json` (not `widgets/`); config file is `allurerc.yaml`.

---

## Code Patterns

### Fluent API with `.and()` chaining
```java
driver.element()
    .type(searchBox, "query")
    .and().browser().captureScreenshot()
    .and().assertThat(result).text().contains("Expected");
```

### Thread-safe parallel driver management
```java
private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

@BeforeMethod
public void init() { driver.set(new SHAFT.GUI.WebDriver()); }

@AfterMethod(alwaysRun = true)
public void tear() { driver.get().quit(); }
```

### Locator builder (prefer over raw XPath)
```java
By field   = Locator.hasTagName("input").hasAttribute("name", "q").build();
By button  = Locator.clickableField("Login");
By shadow  = Locator.hasTagName("button").insideShadowDom(hostLocator).build();
```

### SHAFT assertions (never use TestNG/JUnit Assert directly)

Three entry points — use the right one:

| Entry point | Use for | Returns |
|-------------|---------|---------|
| `Validations` (`shaft-core`) | Primitives, objects, numbers, files, JSON, collections | `ValidationsBuilder` |
| `WebValidations` (`shaft-web`) | WebDriver elements, browser state | `WebValidationsBuilder` |
| `ApiValidations` (`shaft-api`) | REST responses (`Response` objects) | `RestValidationsBuilder` |

```java
// element / browser assertions
WebValidations.assertThat().element(locator).text().isEqualTo("Expected");
WebValidations.assertThat().browser().title().contains("Dashboard");

// REST response assertions
ApiValidations.assertThat(response).extractedJsonValue("$.id").isEqualTo("1").perform();

// primitive / object assertions
Validations.assertThat().object(actual).isEqualTo(expected).perform();
```

**Anti-pattern (eliminated in PR #2646):** Do NOT call `Validations.assertThat().response(...)` —
`response()` now only exists on `ApiValidations`. Do NOT create classes in `shaft-web` at the same
FQCN as `shaft-core` classes (class-shadowing was the root cause of the old pattern).

```java
// ❌ old — no longer compiles after PR #2646
Validations.assertThat().response(response).extractedJsonValue("$.id")...
// ✅ new
ApiValidations.assertThat(response).extractedJsonValue("$.id")...
```

### Test data
```java
SHAFT.TestData.JSON data = new SHAFT.TestData.JSON("filename.json");
String value = data.getTestData("key");  // stored in src/test/resources/testDataFiles/
```

### New properties (no hardcoded values)
```java
// ✅ correct
SHAFT.Properties.timeouts.waitForLazyLoadingTimeout();

// ❌ wrong
int timeout = 30;
System.getProperty("targetBrowserName");
```

### ThreadLocal teardown (always remove to prevent pool-reuse leaks)
```java
@AfterMethod(alwaysRun = true)
public void tear() {
    driver.get().quit();
    driver.remove();              // prevents stale reference when thread pool reuses threads
    Properties.clearForCurrentThread();
}
```

---

## Source Layout

```
src/main/java/com/shaft/
  driver/         SHAFT.java facade + DriverFactory + internal helpers
  gui/            browser/, element/ (Actions, AlertActions, TouchActions, AsyncElementActions)
                  internal/ (locator builder, image utils)
  api/            RestActions, RequestBuilder, ShaftRestAssuredFilter
  cli/            FileActions, TerminalActions
  db/             DatabaseActions
  validation/     ValidationsBuilder + internal builders
  properties/internal/  @Key/@DefaultValue property interfaces
  tools/io/       ReportManager, TestDataReader types, logging helpers
  listeners/      TestNG/JUnit event listeners + WebDriverListener
  cucumber/       Pre-built Cucumber step definitions
  enums/internal/ Shared typed enums (ClipboardAction, NavigationAction, Screenshots)

src/test/java/
  testPackage/           Main integration/feature tests
    unitTests/           Pure unit tests (no browser/device required)
    mockedTests/         Tests using Mockito mocks (no browser/device required)
    properties/          Property accessor tests (no browser/device required)
    coverage/            Race-condition and file-action tests
    legacy/              Full browser integration tests (require Selenium Grid/local browser)
    locator/             Locator builder tests (require browser)
    validationsWizard/   Validation API tests (require browser)
    SHAFTWizard/         Wizard-pattern demo tests (some require browser)
    appium/              Mobile tests (require Appium server + device/emulator)
    LambdaTest/          LambdaTest cloud tests (require LambdaTest credentials)
  com/shaft/             Framework unit tests (mirrors main package structure, no browser needed)
  junitTestPackage/      JUnit5 examples (no browser needed)
  cucumberTestRunner/    Cucumber runner + customCucumberSteps/

src/main/resources/
  properties/            Default .properties files read by OWNER
  examples/              Seven sample pom.xml files (TestNG, JUnit, Cucumber variants)
  docker-compose/        Selenium Grid + Postgres compose files
```

---

## CI Workflow Coverage Map

The four active CI workflows use Maven `-Dtest=` regex patterns to select test classes. Understanding this map prevents "why isn't my test running in CI?" surprises.

### `e2eTests.yml` (triggers on push to `main`, nightly, `workflow_dispatch`)

| Job | Test pattern | Infrastructure needed |
|---|---|---|
| `Ubuntu_Database` | `.*DatabaseActions.*`, `.*DB.*`, `.*Db.*`, `.*db.*` | PostgreSQL container |
| `Ubuntu_APIs` | `.*API.*`, `.*Api.*`, `.*uestBuilder.*`, `.*Rest.*`, `.*Json.*`, `.*JSON.*`, `.*json.*`, `.*YAML.*`, `.*Yaml.*`, `.*yaml.*`, `.*CLIWizard.*`, `.*TerminalActions.*`, `.*properties.Mobile.*`, `.*CSVFileManagerUnitTest.*`, `.*PdfFileManager.*`, `.*ExcelFileManager.*` | None (pure unit/API) |
| `Ubuntu_Firefox_Grid` | **GLOBAL_TESTING_SCOPE** (see below) | Docker Selenium Grid, 10 Firefox nodes |
| `Ubuntu_Chrome_Grid` | **GLOBAL_TESTING_SCOPE** | Docker Selenium Grid, 10 Chrome nodes |
| `Ubuntu_MicrosoftEdge_Grid` | **GLOBAL_TESTING_SCOPE** | Docker Selenium Grid, 10 Edge nodes |
| `Android_Native_BrowserStack` | `.*ndroid.*`, `.*BrowserStackPropertiesUnitTest.*`, `.*BrowserStackHelperUnitTest.*`, `.*BrowserStackSdkHelperCoverageUnitTest.*` | BrowserStack credentials |
| `iOS_Web_SAFARI_BrowserStack` | `.*MobileWebTest.*` | BrowserStack credentials |
| `Android_Web_Chrome_BrowserStack` | `.*MobileWebTest.*` | BrowserStack credentials |
| `MacOSX_Safari_BrowserStack` | `.*BrowserActionsTests.*`, `.*BigPageActionsTest.*`, `.*BrowserStackPropertiesUnitTest.*`, `.*BrowserStackHelperUnitTest.*`, `.*BrowserStackSdkHelperCoverageUnitTest.*` | BrowserStack credentials |
| `Ubuntu_Chrome_Cucumber_Grid` | `.*CucumberTests.*`, `.*CucumberFeatureListenerUnitTest.*`, `.*CucumberTestRunnerListenerUnitTest.*` | Docker Selenium Grid |
| `MacOSX_Safari_Cucumber_BrowserStack` | `.*CucumberTests.*` | BrowserStack credentials |

### `e2eLocalTests.yml` (nightly, `workflow_dispatch`)

Runs **GLOBAL_TESTING_SCOPE** on Windows Edge, Windows Chrome, macOS Safari, macOS Chrome (local browser, no Grid).
Also runs `.*CucumberTests.*` on Windows Edge Cucumber.
MacOS Safari excludes additionally: `.*MobileEmulation.*`

### `e2eLambdaTestTests.yml` (`workflow_dispatch`, nightly)

| Job | Test pattern |
|---|---|
| `LambdaTest_NativeAndroid` | `.*Test_LTMobAPK.*` |
| `LambdaTest_NativeIOS` | `.*Test_LTMobIPA.*` |
| `LambdaTest_WebApp` | `.*Test_LTWebApp.*` |
| `LambdaTest_DesktopWeb` | `.*Test_LTDesktopWeb.*`, `.*LambdaTestHelperUnitTest.*`, `.*LambdaTestSetPropertyCoverageUnitTest.*` |

### GLOBAL_TESTING_SCOPE (exclusion-based)

Everything **except** these patterns:
```
!.*DatabaseActions.*, !.*CLI.*, !.*DB.*, !.*Db.*, !.*db.*,
!.*API.*, !.*Api.*, !.*api.*, !.*uestBuilder.*, !.*Rest.*,
!.*Json.*, !.*JSON.*, !.*json.*, !.*ndroid.*, !.*IOS.*,
!.*MobileWeb.*, !.*appium.Mobile.*, !.*properties.Mobile.*,
!.*CucumberTests.*, !.*LT.*, !.*Flutter.*
```

**Important subtleties:**
- `YAML`/`Yaml`/`yaml` are NOT excluded from GLOBAL_TESTING_SCOPE, so YAML unit tests run in both `Ubuntu_APIs` AND browser grid jobs (double execution — harmless for unit tests).
- `.*LT.*` is matched literally, so "LambdaTest" (no adjacent L+T) passes through. Only `Test_LT*` class names are excluded.
- `.*CucumberTests.*` excludes `testPackage.properties.CucumberTests` from GLOBAL_TESTING_SCOPE; it runs in Cucumber grid jobs instead.
- `CucumberFeatureListenerUnitTest` and `CucumberTestRunnerListenerUnitTest` do NOT contain "CucumberTests" as a substring, so they run in BOTH Cucumber grid jobs AND GLOBAL_TESTING_SCOPE browser grid jobs.

---

## Known Workflow Coverage Gaps

| Test class | Status | Reason | Resolution |
|---|---|---|---|
| `testPackage.appium.IOSBasicInteractionsTest` | ❌ **Not covered by any workflow** | Excluded from GLOBAL_TESTING_SCOPE via `!.*IOS.*`; iOS BrowserStack job only runs `MobileWebTest` | Needs a dedicated native iOS BrowserStack job, or explicit inclusion in the iOS web job |
| `testPackage.appium.FlutterTest` | ⚠️ Intentionally not run | Excluded via `!.*Flutter.*`; all `@Test` methods have `groups = {"flutter"}` which no workflow enables | Intentional — no Flutter CI infrastructure configured |
| `testPackage.appium.FileUploadDownloadTest` | ⚠️ Intentionally not run | All `@Test` methods have `enabled = false` | Intentional — tests are scaffolded but disabled |
| `testPackage.appium.MobileTest` | N/A | Abstract base class | Cannot be instantiated directly |

---

## Logging Rules
- Every SHAFT action emits **exactly one** `INFO` log describing the outcome.
- Every SHAFT action emits **at least one** `DEBUG` log with diagnostic context.
- Route logs through SHAFT reporting helpers — never `System.out.println` or raw logger calls from action classes.
- Default: root level `INFO`, console enabled, file logging disabled (enabled only on retry, then deleted and attached to Allure).
- Do not add duplicate `INFO` logs through separate loggers for the same action.

---

## Release Versioning
Format: `{major}.{quarter}.{YYYYMMDD}` — e.g., `10.2.20260506`.

Three files must always be updated together:
1. `pom.xml` `<version>`
2. `Internal.java` `@DefaultValue` for `shaftEngineVersion` (and check `allure3Version`, `nodeLtsVersion`)
3. All 7 example pom.xml files under `src/main/resources/examples/` (use bulk `sed`)

Also sync these properties across all 7 sample pom.xml files:
`jdk.version`, `aspectjweaver.version`, `maven-compiler-plugin.version`, `maven-resources-plugin.version`, `maven-surefire-plugin.version`, `surefire-testng.version`

Merging to `main` auto-triggers Maven Central CD, JavaDocs publishing, and sample-project sync workflows.

### Required Secrets for Release
| Secret | Purpose |
|---|---|
| `GPG_KEYNAME`, `GPG_PASSPHRASE`, `GPG_PRIVATE_KEY` | Maven Central artifact signing |
| `OSSRH_USERNAME`, `OSSRH_PASSWORD` | Maven Central credentials |
| `BOT_TOKEN` | PAT with `repo` scope on `ShaftHQ/shafthq.github.io` for cross-repo dispatch |
| `CODECOV_TOKEN` | JaCoCo coverage upload in `post-test-report` action |

### Cross-Repo Dispatch Contract
The CD pipeline dispatches to `ShaftHQ/shafthq.github.io` for automated blog post creation.
**Always include `tag_name`** in the dispatch payload — the blog workflow reads `client_payload.tag_name`.
Do NOT use `stefanzweifel/git-auto-commit-action` in the blog workflow; use `peter-evans/create-pull-request` instead (protected branch requires PR).

---

## Key Anti-Patterns
- Do **not** call `System.getProperty()` or `System.setProperty()` in framework or test code — use `SHAFT.Properties.*` / `ThreadLocalPropertiesManager`.
- Do **not** add `--clean` to `allure generate` commands (removed in Allure 3).
- Do **not** leave `@Ignore`-annotated or all-commented-out test classes — delete them.
- Do **not** add intentionally broken/failing tests; Allure `BROKEN` status fails CI.
- Do **not** add new external binary dependencies without the three-tier resolution pattern.
- Do **not** modify unrelated code while fixing a bug.
- Do **not** use `Thread.sleep()` — use SHAFT's built-in wait mechanisms.
- Do **not** skip `ThreadLocal.remove()` in teardown — stale references cause failures on thread-pool reuse.
- Do **not** modify `SHAFT.Properties.flags` in parallel tests without `@Test(singleThreaded = true)` + teardown reset.
- Do **not** read CI workflow job names from copilot-instructions.md for Java version — always check `pom.xml`.
- Do **not** add `## What's Changed` placeholder in `RELEASE_BODY_TEMPLATE.md` — it is appended automatically.

---

## Mobile & Remote Testing Constraints

### BrowserStack
- `TouchActions.pushFile()`/`pullFile()` may return corrupted content on BrowserStack virtual devices — validate thoroughly before writing integration tests.
- `nativeKeyboardKeyPress()` relies on `mobile: isKeyboardShown` which may not return `true` on BrowserStack after typing.
- BrowserStack SDK integration is optional; it is excluded from the default surefire classpath (`classpathDependencyExcludes`) to avoid AspectJ instrumentation noise.

### API Tests with External Services
- Tests hitting external services (e.g., `restful-booker.herokuapp.com`) should use `.setTargetStatusCode(0)` to skip status code validation when the test is about functionality (performance, request building) rather than the API contract.

### iOS Native Tests
- `IOSBasicInteractionsTest` is the only test class with no CI workflow coverage (see Known Gaps above). It requires a BrowserStack native iOS session.

---

## Path-Scoped Instruction Files
More detailed rules are in `.github/instructions/`:
- `java-tests.instructions.md` — applies to `src/test/java/**/*.java`
- `framework-source.instructions.md` — applies to `src/main/java/**/*.java`

Full development methodology (PDCA cycle, bug-fix process, PR checklist, release process) is in `.github/copilot-instructions.md`.

---

## Agent Knowledge Repository

Other AI agents are also configured for this repo. Their skills inform patterns usable here too:

| Agent | Files | Key skills |
|---|---|---|
| GitHub Copilot | `.github/copilot-instructions.md`, `.github/copilot-memory.md`, `.github/instructions/` | PDCA methodology, bug-fix process, release process, self-improvement ledger |
| Copilot skills | `.github/skills/github-copilot/` | CI failure investigator, flaky-test stabilizer, release-dependency guard |
| Google Gemma | `.github/skills/google-gemma/` | SHAFT expert triage, code analysis, confidence-scored diagnosis |
| DeepSeek | `.github/skills/deepseek/README.md` | Code analysis + debugging (no skills yet) |
| Qwen | `.github/skills/qwen/README.md` | Multilingual code generation (no skills yet) |
| Claude Code | `.github/skills/claude-code/README.md` | This file is Claude's primary context |

**Adopt from copilot-instructions.md:**
- PDCA: Plan → Do → Check → Act, minimum 3 iterations for non-trivial work.
- Bug-fix: write a failing test first, fix root cause (not symptoms), run regression tests.
- Validation checklist before every PR (see `.github/pull_request_template.md`).

**Adopt from Gemma code-analysis-and-optimization.md:**
- Root-cause-first diagnosis: distinguish symptom, trigger, and root cause.
- Confidence scoring: High (direct match) / Medium (partial) / Low (insufficient evidence).
- Fix ordering by impact and blast radius.

**Adopt from flaky-test-stabilizer.md:**
- `ThreadLocal` requires both `.quit()` AND `.remove()` in teardown.
- Global `SHAFT.Properties.flags` mutations need `@Test(singleThreaded = true)` + teardown reset.
- Assertions on external services need mocking or `.setTargetStatusCode(0)`.

---

## Self-Improvement Cycle

> Claude Code **must** update this file after every non-trivial session. This is how the agent gets smarter over time.

### What to capture
1. **Environment constraints discovered** — version mismatches, missing tools, JDK requirements.
2. **New patterns or idioms** — if you found a better way, note it in the relevant section.
3. **Anti-patterns encountered** — add to the anti-patterns list to prevent future repetition.
4. **CI/workflow behavior** — if a workflow behaved unexpectedly, document the root cause and fix.
5. **Coverage/test gaps** — any test class not covered by a workflow.
6. **Cross-agent learnings** — if another agent's skill file had useful patterns, migrate them here.

### Format for session learnings
Add entries to the **Session Learnings Log** section below. Keep each entry compact:
```
- Date: YYYY-MM-DD
- Area: (Environment / CI / Coverage / Pattern / Anti-pattern / Release)
- Lesson: one-sentence summary
- Evidence: (file path, class, workflow, issue/PR)
```

### When NOT to update
- Trivial fixes (typo, renaming a variable) — skip.
- Changes already documented in `.github/copilot-instructions.md` — reference that file instead of duplicating.
- Temporary workarounds — document the root cause and the intended fix, not the workaround.

---

## Session Learnings Log

- Date: 2026-05-08
- Area: Environment
- Lesson: JDK 25 is strictly enforced by the Maven enforcer plugin (`[25,26)` range); JDK 21 is the only version available in automated agent environments (Claude Code on Web) and cannot build or run tests — no workaround exists.
- Evidence: `pom.xml` line 39 (`<jdk.version>25</jdk.version>`), `pom.xml` lines 1122-1135 (enforcer rule), `setup-test-env/action.yml` line 6 (`default: '25'`)

- Date: 2026-05-08
- Area: CI / Coverage
- Lesson: `testPackage.appium.IOSBasicInteractionsTest` is the only test class in the repo with zero CI workflow coverage — it is excluded from GLOBAL_TESTING_SCOPE via `!.*IOS.*` and the iOS BrowserStack job only runs `MobileWebTest`.
- Evidence: `.github/workflows/e2eTests.yml` lines 14-15 (GLOBAL_TESTING_SCOPE), issue #2503 comment 2026-05-08

- Date: 2026-05-08
- Area: CI / Coverage
- Lesson: The regex `!%regex[.*LT.*]` in GLOBAL_TESTING_SCOPE only excludes class names containing a literal "LT" substring — "LambdaTest" does not contain adjacent L+T, so `LambdaTestHelperUnitTest` and `LambdaTestSetPropertyCoverageUnitTest` run in GLOBAL_TESTING_SCOPE in addition to the LambdaTest workflow.
- Evidence: `.github/workflows/e2eTests.yml` line 15

- Date: 2026-05-08
- Area: CI / Coverage
- Lesson: `testPackage.properties.CucumberTests` matches `!%regex[.*CucumberTests.*]` and is therefore excluded from GLOBAL_TESTING_SCOPE, running only in Cucumber grid jobs — this is a property test, not a Cucumber runner, but it co-locates correctly.
- Evidence: `.github/workflows/e2eTests.yml` lines 347-348

- Date: 2026-05-08
- Area: Coverage / JaCoCo
- Lesson: All 60 classes in issue #2503 coverage backlog now have dedicated unit tests wired into at least one CI workflow; 34 of those classes have confirmed post-PR coverage percentages from commit messages, the remainder need a full CI run to measure.
- Evidence: git log since 2026-05-07, issue #2503 comment 2026-05-08

- Date: 2026-05-08
- Area: Environment / Agent
- Lesson: `.github/copilot-instructions.md` states "Language: Java 21" — this is outdated and incorrect; always derive the canonical Java version from `pom.xml` `<jdk.version>`.
- Evidence: `.github/copilot-instructions.md` line 8 vs `pom.xml` line 39

- Date: 2026-05-08
- Area: CI / Coverage / Anti-pattern
- Lesson: `<testFailureIgnore>false</testFailureIgnore>` in Surefire causes Maven to exit (code 1) during the `test` phase when tests fail, before JaCoCo's `report` goal (also bound to `test` phase) runs — coverage is never generated. Always keep `testFailureIgnore=true` and delegate pass/fail enforcement to the `post-test-report` composite action, which reads Allure summary.json or Surefire XML and exits 1 on failures.
- Evidence: `pom.xml` surefire config (lines 1484/1549), `.github/actions/post-test-report/action.yml` lines 141-154, issue #2642, PR for `claude/coverage-on-maven-failure-dbdnG`

- Date: 2026-05-08
- Area: Pattern / New PR workflow
- Lesson: When working on a new scope of work: always create a new branch, self-assign the GitHub issue, implement the fix, commit, push, and open a draft PR assigning the user as reviewer.
- Evidence: User instructions in session 2026-05-08

- Date: 2026-05-07
- Area: Encoding / Unicode
- Lesson: Enforce UTF-8 at all runtime layers (Maven JVM, Surefire forked JVMs, and CI environment) — do not rely on host defaults.
- Evidence: `pom.xml`, `.mvn/jvm.config`, `setup-test-env/action.yml`, `testPackage/properties/Log4jTests.java` (from copilot-memory.md)

- Date: 2026-05-08
- Area: Architecture / Monorepo
- Lesson: Project was migrated from a monolith to a 6-module Maven monorepo on branch `Monorepo-Shaft-mvp` (PR #2646). All tests now live in `shaft-web`; run with `mvn test -pl shaft-web -am`. Never add shaft-web as a dependency of shaft-core or shaft-api.
- Evidence: PR #2646, branch `Monorepo-Shaft-mvp`, 11 commits starting at `31b1105`

- Date: 2026-05-08
- Area: Architecture / Validation
- Lesson: `ValidationsBuilder` class-shadowing was eliminated. `shaft-web` no longer has a `ValidationsBuilder` at `com.shaft.validation.internal.ValidationsBuilder`. Use `WebValidations` for element/browser, `ApiValidations` for REST responses, `Validations` for primitives. `Validations.assertThat().response(...)` no longer compiles.
- Evidence: PR #2646 commit `c75cc407cf`, 23 call sites updated, 25 files changed

- Date: 2026-05-08
- Area: Environment / Windows
- Lesson: On Windows, `new File("/tmp/...").isAbsolute()` returns false (JDK only considers paths absolute when they start with a drive letter). Fixed in `JavaHelper.appendTestDataToRelativePath` by adding `|| relativePath.startsWith("/")` guard.
- Evidence: `shaft-core/.../JavaHelper.java`, commit `7781b8b`

- Date: 2026-05-08
- Area: Environment / Windows
- Lesson: `FileInputStream` left open without try-with-resources holds a file lock on Windows, preventing `File.delete()` in test cleanup. Always use try-with-resources for `FileInputStream` in `PropertyFileManager` (and the deferred `ExcelFileManager`, `RecordManager`).
- Evidence: `shaft-core/.../PropertyFileManager.java`, commit `d0e27ca`

- Date: 2026-05-08
- Area: Environment / PowerShell
- Lesson: PowerShell does not support `$(cat <<'EOF'...EOF)` bash heredoc syntax. Use PowerShell `@'...'@` here-strings when passing multiline strings to native executables (`git commit -m`, `gh pr create --body`).
- Evidence: Session 2026-05-08, PR creation for #2646 and #2647

- Date: 2026-05-08
- Area: Pattern / grep / multiline
- Lesson: Single-line `grep` patterns miss Java fluent chains split across lines (e.g., `Validations.assertThat()\n.response(res)`). When doing impact analysis for method-level refactors, also grep with multiline mode or read files directly after the first compile pass.
- Evidence: Session 2026-05-08, 3 rounds of compile-error discovery during ValidationsBuilder fix

- Date: 2026-05-11
- Area: Architecture / Monorepo / Validations
- Lesson: Primitive validation methods (`validateEquals`, `validateNumber`) and their shared utilities (`setCommonParameters`, `updateAllureParameters`, `performValidation`, `formatAssertionErrorWithAutoDetectedPackage`, `reportPrimitiveValidationState`) belong in shaft-core via a new `PrimitiveValidationsHelper` class. `ValidationsHelper2` in shaft-web `extends PrimitiveValidationsHelper` and adds the 7 web-specific methods + a driver-aware `reportWebValidationState`. `ValidationsExecutor`'s switch arms use **direct calls** (`new PrimitiveValidationsHelper(...).validateXxx(...)`) for primitives and **reflective `invokeHelper2(...)`** only for the 7 web methods that legitimately cannot resolve at compile time in lean scope. Mirrors the monolith's design (direct calls everywhere) while respecting the monorepo dep direction.
- Evidence: PR #2646 commits `7b629f7b82`, `3079bb13e4`, `9093226437`; spec `Projects/SHAFT_ENGINE/specs/2026-05-11-validations-primitives-split.md`; issue #2690

- Date: 2026-05-11
- Area: CI / Surefire / SPI
- Lesson: `surefire-testng` provider does NOT honour SPI auto-discovery via `META-INF/services/org.testng.ITestNGListener`. The SHAFT TestNG listener must be registered explicitly in every module's surefire config under `<configuration><properties><property><name>listener</name><value>com.shaft.listeners.TestNGListener</value></property></properties>`. The SPI file shipped in `shaft-core/src/main/resources/META-INF/services/` is necessary-but-not-sufficient on its own. Consumer pom templates already had the explicit listener block; lean module poms (`shaft-core`, `shaft-api`, `shaft-db`) did not until this PR.
- Evidence: PR #2646 commit `ed1515db70`; issue #2690 with A/B run evidence (tests pass with no SHAFT bootstrap vs. listener fires only when explicitly registered)

- Date: 2026-05-11
- Area: Anti-pattern / Test framework
- Lesson: Adding `surefire-testng` as a surefire plugin dependency forces the TestNG provider, which **silently drops JUnit 5 `@Test` methods from discovery**. Modules that mix frameworks need both `surefire-testng` AND `surefire-junit-platform` as plugin dependencies. Per the validation spec convention, `shaft-core`/`shaft-api`/`shaft-db` are TestNG modules; any JUnit 5 test in those modules (e.g., `ApiValidationsEntryPointTest` was originally JUnit 5) needs migration to TestNG to remain discoverable.
- Evidence: PR #2646 commit `ed1515db70` (3 tests migrated); `superpowers:systematic-debugging` investigation in session 2026-05-11

- Date: 2026-05-11
- Area: Debugging / TestNG / Diagnostic anti-pattern
- Lesson: When TestNG reports `Tests run: 0, Failures: 0, Skipped: 0` with no error logs, the test methods were **never discovered** — it is NOT a "kill switch" or `SkipException`. Suspect framework mismatch (JUnit vs TestNG), classpath issues, or annotation discovery failures. Instrument `IMethodInterceptor.intercept(List<IMethodInstance>, ITestContext)` and `ITestListener.onStart(ITestContext)` with `ReportManager.logDiscrete` to see exactly what TestNG considers runnable. `context.getAllTestMethods()` returning an empty array confirms the discovery layer rejected the methods.
- Evidence: Session 2026-05-11, retraction of the "kill switch" claim on issue #2690

- Date: 2026-05-11
- Area: Allure / Test design
- Lesson: `Allure.getLifecycle().updateStep(stepResult -> FailureReporter.fail(message))` is a **no-op when no Allure step is currently active**. Unit tests that bypass `@Step`-annotated wrappers (e.g., call a validation helper method directly) will silently skip the failure path — `FailureReporter.fail` never executes, no `AssertionError` is thrown, the test passes when it should fail. Either wrap the call in `Allure.step("...", () -> ...)` to create a step context, OR test through `ValidationsExecutor.performValidation` / `internalPerform` which carry `@Step` annotations. Do not write unit tests that assume the failure path fires from inside `updateStep`.
- Evidence: Session 2026-05-11, `PrimitiveValidationsHelperTest` initial draft expected `AssertionError` but had to be loosened
