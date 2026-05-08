# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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
```

**Mandatory pre-commit sequence** (no exceptions):
1. `mvn clean install -DskipTests -Dgpg.skip` must succeed.
2. Write tests for every new or changed feature.
3. `mvn test -Dtest=<YourTestClass>` must pass.

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
```java
driver.assertThat().browser().title().contains("Expected");
driver.assertThat().element(locator).domProperty("value").isEqualTo("");
Validations.assertThat().response(response).extractedJsonValue("$.id").isEqualTo("1").perform();
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
  com/shaft/             Framework unit tests (mirrors main package structure)
  junitTestPackage/      JUnit5 examples
  cucumberTestRunner/    Cucumber runner + customCucumberSteps/

src/main/resources/
  properties/            Default .properties files read by OWNER
  examples/              Seven sample pom.xml files (TestNG, JUnit, Cucumber variants)
```

## Logging Rules
- Every SHAFT action emits **exactly one** `INFO` log describing the outcome.
- Every SHAFT action emits **at least one** `DEBUG` log with diagnostic context.
- Route logs through SHAFT reporting helpers — never `System.out.println` or raw logger calls from action classes.
- Default: root level `INFO`, console enabled, file logging disabled (enabled only on retry, then deleted and attached to Allure).

## Release Versioning
Format: `{major}.{quarter}.{YYYYMMDD}` — e.g., `10.2.20260506`.

Three files must always be updated together:
1. `pom.xml` `<version>`
2. `Internal.java` `@DefaultValue` for `shaftEngineVersion` (and check `allure3Version`, `nodeLtsVersion`)
3. All 7 example pom.xml files under `src/main/resources/examples/` (use bulk `sed`)

Merging to `main` auto-triggers Maven Central CD, JavaDocs publishing, and sample-project sync workflows.

## Key Anti-Patterns
- Do **not** call `System.getProperty()` or `System.setProperty()` in framework or test code — use `SHAFT.Properties.*` / `ThreadLocalPropertiesManager`.
- Do **not** add `--clean` to `allure generate` commands (removed in Allure 3).
- Do **not** leave `@Ignore`-annotated or all-commented-out test classes — delete them.
- Do **not** add intentionally broken/failing tests; Allure `BROKEN` status fails CI.
- Do **not** add new external binary dependencies without the three-tier resolution pattern.
- Do **not** modify unrelated code while fixing a bug.

## Path-Scoped Instruction Files
More detailed rules are in `.github/instructions/`:
- `java-tests.instructions.md` — applies to `src/test/java/**/*.java`
- `framework-source.instructions.md` — applies to `src/main/java/**/*.java`

Full development methodology (PDCA cycle, bug-fix process, PR checklist) is in `.github/copilot-instructions.md`.
