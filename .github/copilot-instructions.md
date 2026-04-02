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

## Security Best Practices
- Never hardcode credentials, API keys, or sensitive data
- Use environment variables or secure configuration files for secrets
- Validate all user inputs in test data
- Follow secure coding practices from the CONTRIBUTING.md file
- All code changes undergo security scanning via CodeQL

## Continuous Improvement Guard Rails (PDCA Cycles 2-5)
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

## Common Anti-Patterns to Avoid
- ❌ **Don't commit untested code — EVER** (always write and run tests first)
- ❌ **Don't commit code that doesn't compile** (always run `mvn clean install -DskipTests -Dgpg.skip` first)
- ❌ **Don't skip test execution** before committing (always run `mvn test -Dtest=TestClassName` and capture screenshots)
- ❌ Don't mix different assertion libraries (use SHAFT assertions)
- ❌ Don't skip cleanup in `@AfterMethod` or equivalent
- ❌ Don't hardcode wait times (SHAFT handles synchronization)
- ❌ Don't create new frameworks or patterns without discussion
- ❌ Don't modify working code unless fixing a bug or security issue

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
