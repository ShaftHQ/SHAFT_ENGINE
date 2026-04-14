# GitHub Copilot Instructions for SHAFT Test Automation

## Purpose
Guide Copilot coding agent to generate concise, maintainable, and secure code following SHAFT patterns and best practices for unified test automation across Web, Mobile, API, CLI, Database, and more.

## Repository Overview
SHAFT_ENGINE is a unified test automation framework built with:
- **Language**: Java 21
- **Build Tool**: Maven
- **Testing Frameworks**: TestNG, JUnit5, Cucumber
- **Key Technologies**: Selenium WebDriver, Appium, REST Assured, Allure Reports
- **Project Structure**:
  - `src/main/java/com/shaft/` - Core framework code
  - `src/test/java/` - Test examples and validation tests
  - `src/test/resources/testDataFiles/` - JSON test data files
  - `src/main/resources/` - Framework configuration and Docker compose files
  - `docs/` - Documentation
  - `.github/workflows/` - CI/CD pipelines
  - `.github/instructions/` - Path-specific Copilot instructions (java-tests, framework-source)

## Development Workflow

### Plan-Do-Check-Act (PDCA) Strategy

> **Every implementation — feature, bug fix, or refactor — MUST follow the PDCA cycle. No exceptions.**

PDCA is the mandatory development methodology.  Each cycle consists of four phases that **must all be completed** before a change is considered done:

#### Phase 1 — Plan
- Fully understand the requirements, constraints, and expected outcomes before writing any code.
- Identify which files, classes, and tests are affected.
- Write down an explicit implementation plan (checklist format preferred).
- Anticipate edge cases and failure modes.
- **Do NOT start coding until the plan is clear.**

#### Phase 2 — Do
- Implement the planned change with the smallest possible diff.
- Follow all SHAFT patterns, naming conventions, and architectural rules.
- Write or update tests alongside the implementation (test-first preferred).
- Compile: `mvn clean install -DskipTests -Dgpg.skip` must succeed before proceeding.

#### Phase 3 — Check
- Run all affected tests: `mvn test -Dtest=TestClassName`.
- Verify the implementation against the original requirements.
- Capture screenshots or log output as evidence of passing tests.
- Review your own code for correctness, readability, and SHAFT idioms.
- If anything fails, return to **Plan** with the new information — do not patch blindly.

#### Phase 4 — Act (Refactor & Stabilize)
- Refactor the implementation based on what the Check phase revealed.
- Remove duplication, improve naming, tighten abstractions.
- Re-run tests to confirm refactoring did not break anything.
- Update JavaDocs and inline comments to reflect the final design.

### Minimum 3-Iteration Rule

> **You MUST complete at least three full PDCA cycles for any non-trivial implementation.**

Three iterations are the minimum required to produce an optimized, stable result:

| Iteration | Focus |
|-----------|-------|
| **1 — Make it work** | Get a correct, compiling, tested implementation. No premature optimization. |
| **2 — Make it right** | Refactor for clarity, eliminate duplication, harden edge cases, improve test coverage. |
| **3 — Make it optimal** | Optimize performance/readability, verify no regressions, finalize docs and naming. |

Additional iterations are encouraged whenever tests reveal new edge cases or the design can be meaningfully simplified.

**Anti-patterns to avoid:**
- ❌ Committing after a single "it compiles" pass
- ❌ Skipping the Check phase because "the logic looks correct"
- ❌ Treating the first working implementation as the final one
- ❌ Deferring refactoring to "a follow-up PR" that never happens

---

### ⛔ Mandatory Pre-Commit Rules (No Exceptions)
> **You MUST NEVER commit untested code. There are no exceptions to these rules.**

Before **every** commit, you **must** complete **all** of the following steps in order:

1. **Compile**: Run `mvn clean install -DskipTests -Dgpg.skip` and confirm it succeeds. Do not proceed if compilation fails.
2. **Write Tests**: For any newly developed code, you **must** create corresponding tests **before** committing. Every new feature, bug fix, or code modification requires test coverage.
3. **Run Tests**: Execute tests for all fixed, newly added, or modified code using `mvn test -Dtest=TestClassName` and confirm they pass. Do not commit if any test fails.
4. **Capture Evidence**: Take screenshots of test results (pass/fail output) to provide evidence that tests were executed and passed.
5. **Review Code**: Review your own code changes for correctness, maintainability, and adherence to SHAFT patterns before committing.

**Failure to follow any of these steps is unacceptable. No code may be committed without compilation, tests, and verification.**

### Full Command Reference
- **Build**: `mvn clean install -DskipTests -Dgpg.skip`
- **Run All Tests**: `mvn test`
- **Run Specific Tests**: `mvn test -Dtest=TestClassName`
- **Generate JavaDocs**: `mvn javadoc:javadoc`
- **Code Analysis**: CodeQL runs automatically on PRs

### Validation Checklist
Before submitting a pull request, ensure:
1. ✅ `mvn clean install -DskipTests -Dgpg.skip` completes successfully (compiles and packages)
2. ✅ Tests exist for all new or modified code (no untested code is committed)
3. ✅ All affected tests pass with `mvn test -Dtest=YourTestClassName` (replace with the actual class name)
4. ✅ Screenshots of test results are captured as evidence
5. ✅ Code has been reviewed for correctness and adherence to SHAFT patterns
6. ✅ New or modified `public` methods/classes have JavaDoc comments
7. ✅ No hardcoded credentials or sensitive data in code or test data files

## Bug Fixing Process

> **This process MUST be applied whenever there is a bug or unexpected behavior that needs to be fixed. No exceptions.**

### Steps to Follow (in order)

1. **Understand the Bug**
   - Read the issue/bug report carefully; identify symptoms, expected behavior, and actual behavior
   - Collect all available context: logs, stack traces, screenshots, and reproduction steps
   - Do **not** start coding until you fully understand the problem

2. **Reproduce the Bug**
   - Set up a minimal scenario that consistently triggers the bug
   - Confirm you can reproduce it reliably before proceeding
   - If you cannot reproduce it, request more information from the reporter

3. **Diagnose the Root Cause**
   - Trace the code path that leads to the buggy behavior
   - Identify the exact line(s) or logic that cause the problem
   - Do **not** fix symptoms — always fix the **root cause**

4. **Write a Failing Test First**
   - Before making any code changes, write an automated test that demonstrates the bug
   - The test **must FAIL** against the current (unfixed) code — this proves the test captures the bug
   - Follow existing test patterns in `src/test/java/`
   - Name the test to clearly describe the bug scenario (e.g., `createFileShouldPreserveContentOnConcurrentAccess`)

5. **Fix the Bug (Minimal Change)**
   - Make the **smallest possible code change** that fixes the root cause
   - Do **not** refactor or change unrelated code while fixing a bug
   - Follow all existing code patterns, conventions, and SHAFT architecture
   - Maintain backward compatibility unless the bug itself is a breaking contract

6. **Verify the Fix**
   - Run the newly written failing test — it **must now PASS**
   - Compile: `mvn clean install -DskipTests -Dgpg.skip`
   - Run all affected tests: `mvn test -Dtest=TestClassName`

7. **Run Regression Tests**
   - Run all related test classes to ensure no regressions were introduced
   - Pay special attention to tests that exercise the same code paths as the fix

8. **Document and Commit**
   - Add inline `//` comments to explain non-obvious parts of the fix
   - Capture screenshots of passing test results as evidence
   - Follow all **Mandatory Pre-Commit Rules** before committing

### Bug Fix Anti-Patterns to Avoid
- ❌ Don't start coding before you can reproduce and understand the root cause
- ❌ Don't fix symptoms — always fix the root cause
- ❌ Don't make code changes before writing a failing test first
- ❌ Don't refactor unrelated code while fixing a bug
- ❌ Don't commit without running and passing the newly added test and all related tests

## General Guidelines

### Code Quality
- Use SHAFT's fluent API for browser and element actions
- Prefer descriptive variable and method names following Java conventions
- Always initialize drivers and test data in setup methods
- Use SHAFT assertions for validations
- Structure tests: setup → action → assertion → teardown
- Add JavaDocs for all public methods and classes
- Follow existing code formatting and style patterns in the repository

#### Code Quality Examples

**Example 1 – ThreadLocal for thread-safe parallel driver management:**
```java
// Correct: each thread gets its own driver instance
public class BrowserActionsTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://shafthq.github.io/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
```

**Example 2 – Reusable abstract base class to avoid driver lifecycle duplication:**
```java
// Abstract base: driver init/teardown defined once, reused across test classes
public abstract class Tests {
    protected static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod
    public void init() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void tear() {
        driver.get().quit();
    }
}

// Subclass just extends and focuses on test logic
public class TestClass extends Tests {
    SHAFT.TestData.JSON testData;

    @BeforeClass
    public void beforeClass() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @Test
    public void navigateToDuckDuckGoAndAssertBrowserTitleIsDisplayedCorrectly() {
        driver.get().browser().navigateToURL("https://duckduckgo.com/")
              .and().assertThat().browser().title().contains(testData.getTestData("expectedTitle"));
    }
}
```

**Example 3 – Use Lombok `@Data` and Jackson annotations for clean data objects:**
```java
// Correct: use @Data + @JsonProperty for clean, boilerplate-free response models
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class BodyObject {
    @JsonProperty("key1")
    private String key1 = null;

    @JsonProperty("key2")
    private String key2 = null;

    @JsonProperty("key3")
    private String key3 = null;

    @JsonProperty("key4")
    private int key4;
}
```

**Example 4 – Always use `alwaysRun = true` on teardown methods to guarantee cleanup:**
```java
// Ensures driver.quit() runs even when a test fails
@AfterMethod(alwaysRun = true)
public void afterMethod() {
    driver.get().quit();
}

// Same pattern for class-scoped driver
@AfterClass(alwaysRun = true)
public void tear() {
    driver.get().quit();
}
```

### SHAFT-Specific Patterns
- **For browser title assertions, always use:**
  `driver.assertThat().browser().title().contains("expectedTitle");`
  instead of any other pattern
- **Prioritize using SHAFT's assertion API directly** (e.g., `driver.assertThat().element(locator).domProperty("value").isEqualTo("")`) instead of manually retrieving properties and then asserting them
- **Use method chaining** with `.and()` for fluent, readable test code
- **Use SHAFT.TestData.JSON** for test data management instead of hardcoding values

## Example: TestNG Test Class

```java
// Import required SHAFT and TestNG classes
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.*;

public class TestClass {
    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;

    String targetUrl = "https://duckduckgo.com/";
    By logo = By.xpath("//div[contains(@class,'container_fullWidth__1H_L8')]//img");
    By searchBox = Locator.hasAnyTagName().hasAttribute("name", "q").build();
    By firstSearchResult = Locator.hasTagName("article").isFirst().build();

    @BeforeClass
    public void beforeClass() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test
    public void navigateToDuckDuckGoAndAssertBrowserTitleIsDisplayedCorrectly() {
        driver.browser().navigateToURL(targetUrl)
              .and().assertThat().browser().title().contains(testData.getTestData("expectedTitle"));
    }

    @Test
    public void navigateToDuckDuckGoAndAssertLogoIsDisplayedCorrectly() {
        driver.browser().navigateToURL(targetUrl)
              .and().element().assertThat(logo).matchesReferenceImage();
    }

    @Test
    public void searchForQueryAndAssert() {
        driver.browser().navigateToURL(targetUrl)
              .and().element().type(searchBox, testData.getTestData("searchQuery") + Keys.ENTER)
              .and().assertThat(firstSearchResult).text().doesNotEqual(testData.getTestData("unexpectedInFirstResult"));
    }

    @Test
    public void assertSearchBoxIsEmpty() {
        driver.browser().navigateToURL(targetUrl)
                .and().assertThat().element(searchBox).domProperty("value").isEqualTo("");
    }

    @AfterMethod
    public void afterMethod() {
        driver.quit();
    }
}
```

## Patterns to Follow

### TestNG Patterns (Primary)
- Use `@BeforeClass` for test data setup
- Use `@BeforeMethod/@AfterMethod` for driver lifecycle
- Use `@Test` for test methods with descriptive names

### JUnit5 Patterns
- Use `@BeforeAll` for test data setup
- Use `@BeforeEach/@AfterEach` for driver lifecycle
- Use `@Test` for test methods

### Locator Patterns
- Use SHAFT's `Locator` builder for element selection:
  ```java
  By element = Locator.hasTagName("input").hasAttribute("name", "q").build();
  ```
- Use standard Selenium `By` locators when appropriate:
  ```java
  By element = By.id("searchbox");
  By element = By.xpath("//div[@class='container']");
  ```

### Test Data Management
- Store test data in JSON files under `src/test/resources/testDataFiles/`
- Access via SHAFT.TestData.JSON:
  ```java
  SHAFT.TestData.JSON testData = new SHAFT.TestData.JSON("filename.json");
  String value = testData.getTestData("key");
  ```

### Assertion Patterns
- **Use SHAFT assertions directly for element properties and states**
- **Always use `driver.assertThat().browser().title()` for browser title assertions**
- Chain assertions for readability:
  ```java
  driver.element().click(button)
        .and().assertThat(result).isVisible()
        .and().assertThat(result).text().contains("Success");
  ```

### Coding Patterns
Follow the architectural patterns already established in the SHAFT codebase:

**Example 1 – Fluent API / method chaining with `.and()` for readable multi-step tests:**
```java
// Correct: chain actions across element, browser, and assertions in one expression
driver.get().element()
    .type(By.id("et_pb_contact_name_0"), "TEST_NAME")
    .type(By.id("et_pb_contact_email_0"), "email@email.email")
    .captureScreenshot(By.id("et_pb_contact_message_0"))
    .and().browser().captureScreenshot()
    .and().element().assertThat(By.id("et_pb_contact_email_0"))
        .text().isEqualTo("email@email.email").perform();
```

**Example 2 – Builder pattern via `RequestBuilder` for REST API calls (legacy `RestActions` API):**
```java
// Note: for new code, prefer SHAFT.API (see Example 5). RestActions is shown here for
// compatibility with legacy test code and for static utility methods (getResponseJSONValue, etc.)
Response response = RestActions.buildNewRequest("https://jsonplaceholder.typicode.com/", "posts", RestActions.RequestType.GET)
    .setTargetStatusCode(200)
    .performRequest()
    .getResponse();

// Retrieve a specific JSON value
String title = RestActions.getResponseJSONValue(response, "$.title");

// Validate a nested field using SHAFT Validations
Validations.assertThat()
    .response(response)
    .extractedJsonValue("$.userId")
    .isEqualTo("1")
    .perform();
```

**Example 3 – Page Object Model (POM) for reusable page-level actions and locators:**
```java
// poms/GoogleSearch.java
public class GoogleSearch {
    WebDriver driver;

    @Getter
    static By searchBox_textField = By.xpath("//*[@id='lst-ib' or @class='lst' or @name='q']");
    String url = "https://www.google.com/ncr";

    public GoogleSearch(WebDriver driver) {
        this.driver = driver;
    }

    public void navigateToURL() {
        new BrowserActions(driver).navigateToURL(url);
    }

    public void assertPageIsOpen() {
        Validations.assertThat()
            .element(driver, googleLogo_image)
            .exists()
            .perform();
    }
}
```

**Example 4 – Cucumber step definitions using SHAFT driver:**
```java
// customCucumberSteps/steps.java
public class steps {
    private SHAFT.GUI.WebDriver driver;

    @Given("I open the target browser")
    public void i_open_the_target_browser() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @When("I navigate to {string}")
    public void i_navigate_to(String pageName) {
        if (pageName.equals("Google Home")) {
            driver.browser().navigateToURL("https://www.google.com/ncr", "https://www.google.com");
        }
    }

    @Then("I assert that the {string} attribute of the browser, equals {string}")
    public void iAssertThatTheAttributeOfTheBrowserEquals(String browserAttribute, String expectedValue) {
        driver.assertThat().browser()
            .attribute(browserAttribute)
            .isEqualTo(expectedValue)
            .withCustomReportMessage("Browser [" + browserAttribute + "] should equal [" + expectedValue + "]")
            .perform();
    }
}
```

**Example 5 – Use `SHAFT.API` instance methods for a full REST session:**
```java
// Correct: use SHAFT.API for session-scoped REST interactions
SHAFT.API apiDriver = new SHAFT.API("https://jsonplaceholder.typicode.com");

Response users = apiDriver.get("/users")
    .setTargetStatusCode(200)
    .performRequest()
    .getResponse();

// Extract and validate a value from the response list
String userId = RestActions.getResponseJSONValueFromList(users, "$", "id", "name", "Chelsey Dietrich");
Validations.assertThat().object(userId).isEqualTo("5").perform();
```

### Class Scopes
Follow these access modifier and class structure rules observed throughout the SHAFT framework:

**Example 1 – Nested static classes as user-facing facades (SHAFT.java pattern):**
```java
// SHAFT.java: nested static classes expose a clean, namespaced public API
public class SHAFT {
    public static class GUI {
        public static class WebDriver {           // SHAFT.GUI.WebDriver
            public WebDriver() { /* ... */ }
            public WebDriver(DriverFactory.DriverType driverType) { /* ... */ }
            public Actions element() { return new Actions(helper); }
            public BrowserActions browser() { return new BrowserActions(helper); }
            public WizardHelpers.WebDriverAssertions assertThat() { /* ... */ }
            public void quit() { /* ... */ }
        }
        public static class Locator extends com.shaft.gui.internal.locator.Locator { }
    }
    public static class API { /* ... */ }        // SHAFT.API
    public static class CLI { /* ... */ }        // SHAFT.CLI
    public static class DB extends DatabaseActions { }
    public static class Validations { /* ... */ }
    public static class TestData { /* ... */ }
    public static class Report { /* ... */ }
}
```

**Example 2 – Utility classes with `private` constructor to prevent instantiation:**
```java
// Validations.java: static-only utility; throw IllegalStateException to block instantiation
public class Validations {
    private Validations() {
        throw new IllegalStateException("Utility class");
    }

    public static ValidationsBuilder assertThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT);
    }

    public static ValidationsBuilder verifyThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT);
    }
}
```

**Example 3 – Inner non-static class for contextual sub-actions (Async inner class):**
```java
// Inside SHAFT.GUI.WebDriver: non-static inner class gives access to parent's helper
public class WebDriver {
    DriverFactoryHelper helper;

    public Async async() {
        return new Async();
    }

    // Inner class: scoped to its enclosing WebDriver instance
    public class Async {
        public AsyncElementActions element() {
            return new AsyncElementActions(helper);  // accesses outer helper
        }
    }
}
```

**Example 4 – Enums with constructor parameters for typed constants:**
```java
// DatabaseActions.java
public enum DatabaseType {
    MY_SQL, SQL_SERVER, POSTGRES_SQL, ORACLE, IBM_DB2, H2, MONGO_DB
}

// ValidationEnums.java
public enum ValidationType {
    POSITIVE(true), NEGATIVE(false);
    private final boolean value;
    ValidationType(boolean value) { this.value = value; }
    public boolean getValue() { return value; }
}

// RestActions.java
public enum RequestType { GET, POST, PATCH, DELETE, PUT }
public enum ParametersType { FORM, QUERY }
```

**Example 5 – Factory method (`getInstance`) for controlled instantiation:**
```java
// FileActions.java: factory method controls instance configuration
public class FileActions {
    private boolean internalInstance = false;

    // Default public factory method
    public static FileActions getInstance() {
        return getInstance(false);
    }

    // Overloaded factory method with configuration flag
    public static FileActions getInstance(boolean internalInstance) {
        var instance = new FileActions();
        instance.internalInstance = internalInstance;
        return instance;
    }
}

// SHAFT.API: static factory alternative alongside constructor
public class API {
    public API(String serviceURI) { /* ... */ }
    public static API getInstance(String serviceURI) { return new API(serviceURI); }
}
```

## Batteries-Included Architecture

SHAFT's core promise is that users never need to download, install, or configure **any** binary, driver, or external tool.  Before adding any code that assumes an external tool is present on the host, read this section carefully.

### WebDriver Management
Selenium Manager is fully integrated.  **Never** instruct users to download `chromedriver`, `geckodriver`, or any other browser driver.  SHAFT resolves the correct driver automatically.

### Allure 3 CLI — Three-Tier Resolution
`AllureManager.resolveAllureCommandPrefix()` resolves the Allure CLI automatically, in order:
1. System `allure` binary on `PATH`
2. `npx --yes allure@<allure3Version>` (downloads and caches via npx)
3. Downloads a portable Node.js LTS archive to `~/.m2/repository/nodejs/` (uses `nodeLtsVersion` from `Internal.java`)

### Version Constants
`allure3Version` and `nodeLtsVersion` live in `Internal.java` as `@Key`/`@DefaultValue` properties and are fully user-overridable via `internal.properties`.  **Never** hardcode version strings inline in logic.

```java
// ✅ Read from SHAFT properties — user-overridable
String allureVersion = SHAFT.Properties.internal.allure3Version();
String nodeLts       = SHAFT.Properties.internal.nodeLtsVersion();

// ❌ Hardcoded — breaks when the version changes
private static final String ALLURE_VERSION = "3.4.0";
```

### Rule for New Binary Dependencies
Any new binary or tool dependency **MUST**:
1. Follow the same three-tier resolution pattern (system PATH → npx/package-manager → self-download).
2. Expose its version as a configurable `@Key`/`@DefaultValue` property in `Internal.java` (or the appropriate properties interface).
3. Never assume the tool is pre-installed on the host machine.

**Anti-patterns:**
```
❌ Assuming 'allure', 'node', 'npx', or any browser driver is on PATH
❌ Hardcoding a version number (e.g. "3.4.0") directly in AllureManager or similar classes
✅ Read version from SHAFT.Properties.internal.allure3Version() / nodeLtsVersion()
✅ Fall back gracefully to bundled resolution if the tool is absent
```

---

## No Hardcoded Values Policy

SHAFT must **never** contain magic strings, version strings, URLs, thresholds, or behavioral switches baked into logic without a corresponding user-overridable property.

### Where to Declare Properties
Every configurable value MUST be declared as a `@Key` / `@DefaultValue` entry in the appropriate interface under `src/main/java/com/shaft/properties/internal/`:

| Interface | Purpose |
|-----------|---------|
| `Flags.java` | Behavioral toggles, engine-wide (static) |
| `Web.java` | Browser/WebDriver config |
| `Mobile.java` | Appium/mobile config |
| `API.java` | REST API defaults |
| `Timeouts.java` | Wait/timeout values |
| `Visuals.java` | Screenshot/visual testing |
| `Reporting.java` / `Allure.java` | Reporting behavior |
| `Paths.java` | File system paths |
| `Performance.java` / `Healenium.java` | Integration-specific |
| `Internal.java` | Internal version/build metadata |

New properties must always include a sensible `@DefaultValue` so existing users are unaffected.  Access properties via `SHAFT.Properties.<group>.<methodName>()`, never via `System.getProperty()` directly.

**Anti-patterns:**
```java
❌ private static final String ALLURE_VERSION = "3.4.0";  // hardcoded
✅ SHAFT.Properties.internal.allure3Version()              // user-overridable

❌ int timeout = 30;                                        // magic number
✅ SHAFT.Properties.timeouts.waitForLazyLoadingTimeout()    // configurable

❌ System.getProperty("targetBrowserName")                  // bypasses SHAFT layer
✅ SHAFT.Properties.web.targetBrowserName()                 // correct accessor
```

---

## Properties Scoping: ThreadLocal vs Engine-Wide

SHAFT uses a **two-tier property system**.  Understanding the difference is essential to avoid race conditions and incorrect scoping in parallel test runs.

### Engine-Wide (Static) Properties — `Flags.java` pattern
- Applied via `System.setProperty` under `synchronized(Properties.class)`, then `ConfigFactory.create(Flags.class)` refreshes the singleton.
- Affect **all threads** for the remainder of the execution.
- Use for: retry counts, driver lifecycle flags, engine feature toggles.
- Tests that modify these **must** use `@Test(singleThreaded = true)` to avoid within-class races.
- **Always reset** engine-wide state in `@AfterMethod(alwaysRun = true)` to prevent test pollution.

```java
// Engine-wide: visible to ALL threads
SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);

// Reset in teardown — mandatory for test isolation
@AfterMethod(alwaysRun = true)
public void afterMethod() {
    SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);
}
```

### Per-Thread (ThreadLocal) Properties — `ThreadLocalPropertiesManager` pattern
- Stored in `ThreadLocal<java.util.Properties>`; invisible to other threads.
- `ThreadLocalPropertiesManager.getProperty(key)` checks thread-local overrides first, then falls back to system properties.
- `Properties.clearForCurrentThread()` **must** be called at lifecycle boundaries to prevent stale state when thread pools reuse threads (it clears `ThreadLocalPropertiesManager` plus all cached per-interface overrides).
- Use for: per-test browser type, target URL, credentials — anything that must differ between parallel test threads.

```java
// Thread-local: only affects the calling thread
ThreadLocalPropertiesManager.setProperty("targetBrowserName", "firefox");

// In teardown — mandatory to avoid state leakage
@AfterMethod(alwaysRun = true)
public void afterMethod() {
    Properties.clearForCurrentThread();
}
```

### Decision Guide

| Change affects… | Use |
|---|---|
| All threads / whole run | `SHAFT.Properties.flags.set().*` (engine-wide) |
| Only this test's thread | `ThreadLocalPropertiesManager.setProperty()` |
| Never | `System.setProperty()` directly in test/framework code |

**Anti-pattern:**
```java
❌ System.setProperty("targetBrowserName", "firefox"); // bypasses SHAFT's property layer
✅ ThreadLocalPropertiesManager.setProperty("targetBrowserName", "firefox");
```

---

## Versioning Convention

SHAFT uses the format **`{major}.{quarter}.{YYYYMMDD}`**:
- `major` increments each January (once per year).
- `quarter` is the current calendar quarter (1–4).
- `patch` is the release date in `YYYYMMDD` format.

Example: `10.2.20260414` (major 10, Q2 of 2026, released 2026-04-14).

When generating changelogs, release notes, or updating `Internal.java`, always follow this convention.

---

## Release Process

Follow these steps in order when preparing a new SHAFT release:

### 1. Update Version Numbers (Three Places — Always All)
- **`pom.xml`** (line 6): change `<version>OLD</version>` to `<version>NEW</version>`.  The comment next to it reminds you to also update `Internal.java`.
- **`src/main/java/com/shaft/properties/internal/Internal.java`**: change the `@DefaultValue` of `shaftEngineVersion()` to match the new version.
- **All example `pom.xml` files** under `src/main/resources/examples/` (7 files — TestNG, JUnit, Cucumber variants): change `<shaft_engine.version>OLD</shaft_engine.version>` to `<shaft_engine.version>NEW</shaft_engine.version>`.  You can do this in bulk with:
  ```bash
  find src/main/resources/examples -name "pom.xml" | xargs sed -i 's|<shaft_engine.version>OLD</shaft_engine.version>|<shaft_engine.version>NEW</shaft_engine.version>|g'
  ```

### 2. Verify No Stable Dependency Updates Are Skipped
Run `mvn versions:display-dependency-updates` and update any dependency that has a **stable** (non-beta, non-RC, non-milestone, non-alpha) newer version.  Pre-release updates should be skipped.

### 3. Compile and Test
```bash
mvn clean install -DskipTests -Dgpg.skip    # must succeed
mvn test -Dtest=<AffectedTests>             # must pass
```

### 4. Commit and Merge to `main`
Merging to `main` automatically triggers the **Maven Central Continuous Delivery** workflow (`mavenCentral_cd.yml`), which:
1. Creates a GitHub Release tagged with the version from `pom.xml`.
2. Deploys the artifact to Maven Central.
3. Dispatches a `shaft-engine-release` event to `ShaftHQ/shafthq.github.io` (user guide repo) via `BOT_TOKEN`.

After the release is published, two more workflows fire automatically:
- **JavaDocs Publisher** (`publishJavaDocs.yml`) — regenerates and publishes the JavaDoc site.
- **Sync Sample Projects SHAFT Version** (`sync-sample-projects-version.yml`) — opens a PR in this repo updating all example `pom.xml` files to the new version.

### Required Secrets
| Secret | Purpose |
|---|---|
| `GPG_KEYNAME`, `GPG_PASSPHRASE`, `GPG_PRIVATE_KEY` | Artifact signing for Maven Central |
| `OSSRH_USERNAME`, `OSSRH_PASSWORD` | Maven Central credentials |
| `BOT_TOKEN` | PAT with `repo` scope on `ShaftHQ/shafthq.github.io` for cross-repo dispatch |

### Checklist
- [ ] `pom.xml` version updated
- [ ] `Internal.java` `shaftEngineVersion` `@DefaultValue` updated
- [ ] All 7 example `pom.xml` files under `src/main/resources/examples/` updated (use bulk `sed` command above)
- [ ] No stable dependency updates skipped
- [ ] Compiles: `mvn clean install -DskipTests -Dgpg.skip`
- [ ] PR merged to `main`

---

## Allure Report 3 Migration Notes

Since the migration (PR #2387), **Copilot must not assume Allure 2 behavior**.  Key differences:

| Aspect | Allure 2 (old) | Allure 3 (current) |
|---|---|---|
| CLI package | Java `allure-commandline` | npm `allure` |
| `allure generate` flag | `--clean` supported | **No `--clean` flag** |
| Config file | Java properties | `allurerc.yaml` (auto-detected in CWD) |
| Summary JSON path | `widgets/summary.json` | `<report>/summary.json` |
| History | folder copy | `historyPath` JSONL |
| Version config | hardcoded | `allure3Version` in `Internal.java` |

- `allure3Version` and `nodeLtsVersion` are configurable via `Internal.java` / `internal.properties`.
- Always use `AllureManager.resolveAllureCommandPrefix()` — never invoke `allure` directly.
- CI workflows supply Node.js via `setup-test-env` action; the `post-test-report` action reads version constants from `Internal.java` at runtime.

---

## Security Best Practices
- Never hardcode credentials, API keys, or sensitive data
- Use environment variables or secure configuration files for secrets
- Validate all user inputs in test data
- Follow secure coding practices from the CONTRIBUTING.md file
- All code changes undergo security scanning via CodeQL

## Continuous Improvement Guard Rails
These guard rails apply specifically when modifying SHAFT internals across PDCA iterations 2 and beyond (see **Plan-Do-Check-Act Strategy** above):
- When modifying validation/reporting internals, clear `ThreadLocal` state with `.remove()` at lifecycle boundaries.
- For progress/log UX, prefer readable output in CI/non-interactive terminals and avoid forcing ANSI colors.
- Keep complexity reductions small and behavior-preserving (extract/inline only what is needed).
- Add/adjust focused tests for every non-trivial behavior change.

## Testing Guidelines
- **You MUST create tests for every new feature, bug fix, or code modification**
- **You MUST run all affected tests and confirm they pass before committing**
- **You MUST capture screenshots of test results** as evidence that tests were executed and passed
- Write tests that validate your changes
- Follow existing test patterns in `src/test/java/`
- Ensure tests are independent and can run in parallel
- Clean up resources in teardown methods
- Use meaningful test names that describe what is being tested

### Parallel-safe Tests for Global Engine State
- `SHAFT.Properties.flags` uses **engine-global** state (not per-thread). Tests that modify flags (e.g. `retryMaximumNumberOfAttempts`) **must** use `@Test(singleThreaded = true)` at the class level to prevent within-class race conditions.
- Always reset global flag state in `@AfterMethod(alwaysRun = true)` by calling `SHAFT.Properties.flags.set().xxx(defaultValue)`.
- All CI workflows set `-DretryMaximumNumberOfAttempts=2` globally; tests that need zero retries must explicitly call `SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0)`.

### API Tests with External Services
- Tests hitting external services (e.g. `restful-booker.herokuapp.com`) should use `.setTargetStatusCode(0)` to skip status code validation when the test is about functionality (performance, request building) rather than the API contract.

### BrowserStack Mobile Tests
- `TouchActions.pushFile()`/`pullFile()` may return corrupted content on BrowserStack; validate before writing integration tests for these.
- `nativeKeyboardKeyPress()` relies on `mobile: isKeyboardShown` which may not return `true` on BrowserStack virtual devices after typing.

## Common Anti-Patterns to Avoid
- ❌ **Don't commit untested code — EVER** (always write and run tests first)
- ❌ **Don't commit code that doesn't compile** (always run `mvn clean install -DskipTests -Dgpg.skip` first)
- ❌ **Don't skip test execution** before committing (always run `mvn test -Dtest=TestClassName` and capture screenshots)
- ❌ Don't mix different assertion libraries (use SHAFT assertions)
- ❌ Don't skip cleanup in `@AfterMethod` or equivalent
- ❌ Don't hardcode wait times (SHAFT handles synchronization)
- ❌ Don't create new frameworks or patterns without discussion
- ❌ Don't modify working code unless fixing a bug or security issue
- ❌ **Don't add intentionally-broken/failing tests** — they cause CI to fail on every run (BROKEN status in Allure triggers CI failure)
- ❌ **Don't read `System.getProperty()` directly in retry/property logic** — use `SHAFT.Properties.flags.*` which consolidates all sources safely
- ❌ **Don't leave commented-out test classes** (`@Ignore` or all methods commented) — delete them to avoid confusion

## Documentation Requirements
- Add JavaDoc comments for all public classes and methods
- Include `@param` and `@return` tags where applicable
- Document any non-obvious logic or workarounds
- Update relevant documentation in `docs/` for new features

## Code Review Guidelines
- Keep changes minimal and focused
- Test changes across affected platforms (Web, Mobile, API, etc.)
- Provide evidence before and after fixes for bug fixes
- Ensure all CI checks pass before requesting review
- Follow the patterns in CONTRIBUTING.md

## Framework-Specific Tips

### For Web Testing
- Use `SHAFT.GUI.WebDriver` for browser automation
- Navigate using `driver.browser().navigateToURL(url)`
- Interact with elements using `driver.element().method(locator)`

### For API Testing
- Use `SHAFT.API` for REST API testing
- Build requests using `RequestBuilder`
- Validate responses using SHAFT's API assertions

### For Mobile Testing
- Use `SHAFT.GUI.WebDriver` with Appium driver types
- Specify driver type: `new SHAFT.GUI.WebDriver(DriverFactory.DriverType.MOBILE_ANDROID)`
- Use touch actions via `driver.touch()`

### For Database Testing
- Use `SHAFT.DB.DatabaseActions` for database operations
- Close connections properly in teardown methods

### Smart / Semantic Locators
Prefer SHAFT's semantic locators over raw XPath when possible:
```java
// Smart locators — no XPath needed
By searchBox  = Locator.inputField("username");
By submitBtn  = Locator.clickableField("Login");

// ARIA roles
By dialog     = Locator.hasRole(Role.DIALOG).build();

// XPath axes
By sibling    = Locator.hasTagName("label").byAxis().followingSibling("input").build();

// Shadow DOM
By shadowEl   = Locator.hasTagName("button").insideShadowDom(shadowHostLocator).build();
```

### Element Extras
```java
driver.element().typeSecure(locator, secret);              // masks value in Allure report
driver.element().switchToIframe(iframeLocator);            // iframe navigation
driver.element().switchToDefaultContent();

// Clipboard operations
driver.element().clipboard().copyAll(locator);
driver.element().clipboard().cutAll(locator);
driver.element().clipboard().paste(locator);
driver.element().clipboard().deleteAll(locator);

// Explicit wait
driver.element().waitUntil(ExpectedConditions.visibilityOfElementLocated(locator));

// Alert handling
driver.alert().isAlertPresent();
driver.alert().acceptAlert();
driver.alert().dismissAlert();
driver.alert().getAlertText();
driver.alert().typeIntoPromptAlert("text");
```

### Browser Extras
```java
// Network mocking / interception (Selenium DevTools) — both methods take (Predicate<HttpRequest>, HttpResponse)
driver.browser().mock(predicate, httpResponse);
driver.browser().intercept(predicate, httpResponse);

// Accessibility (axe-core)
driver.browser().accessibility().analyzePage();
driver.browser().accessibility().assertNoCriticalViolations();
driver.browser().accessibility().assertAccessibilityScoreAtLeast(90);
driver.browser().accessibility().analyzeWithIgnoredRules(List.of("color-contrast"));
```

### Async / Virtual Threads (Java 21)
```java
// Fire-and-forget with synchronization — uses Java 21 virtual threads
driver.async().element().click(locator).synchronize();
```

### GraphQL Support
```java
RestActions.sendGraphQlRequest(baseURI, endpoint, query);
RestActions.sendGraphQlRequest(baseURI, endpoint, query, variables);
// Also available with headers and/or fragment overloads (6 total)
```

### Additional Test Data Readers
```java
// YAML
new SHAFT.TestData.YAML("data.yaml").getString("key");

// CSV
new SHAFT.TestData.CSV("data.csv").getCellData(row, col);

// PDF
new PdfFileManager("doc.pdf").readFileContent();
```

### Integrations (all configured via SHAFT properties — no external install needed)
```java
// Healenium self-healing
SHAFT.Properties.healenium.set().healEnabled(true).recoveryTries(3);

// Visual regression
driver.element().assertThat(logo).matchesReferenceImage();

// Lighthouse performance report (batteries-included Node.js)
SHAFT.Properties.performance.set().isEnabled(true);
driver.browser().generateLightHouseReport();

// Video recording
SHAFT.Properties.visuals.set().recordVideo(true);
```

## AI-Assisted Development Tips
- Use Copilot to suggest locators, assertions, and test data access
- Always review generated code for correctness and maintainability
- Validate that generated code follows SHAFT patterns
- **Always compile generated code** with `mvn clean install -DskipTests -Dgpg.skip` before committing
- **Always create tests for generated code** — untested code must never be committed
- **Always run tests** with `mvn test -Dtest=TestClassName` and capture screenshots of results before committing
- Test generated code thoroughly before committing

## Refactoring Guidelines
- Update locators and test data as needed for your application
- Add more test methods following the shown patterns
- For JUnit5 or Cucumber, adapt setup/teardown annotations accordingly
- Maintain backward compatibility unless explicitly changing APIs

## Getting Help
- Review existing tests in `src/test/java/testPackage/` for examples
- Check documentation at https://shafthq.github.io/
- Join the Slack community for questions
- Review README.md and CONTRIBUTING.md for contribution guidelines

---
**Note**: These instructions are designed to help GitHub Copilot coding agent generate high-quality, maintainable code that follows SHAFT framework best practices. **You MUST NEVER commit untested code.** Always compile, create tests, run tests (with screenshots), and review code before every commit — no exceptions.
