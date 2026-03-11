---
applyTo: "src/test/java/**/*.java"
---

## Java Test File Requirements

When writing or editing test files under `src/test/java/`, follow these SHAFT-specific rules.
These examples use **TestNG** (the primary framework). For JUnit5, adapt annotations: use `@BeforeAll`/`@BeforeEach`/`@AfterEach` instead of `@BeforeClass`/`@BeforeMethod`/`@AfterMethod`.

### Test Class Structure
- Use `@BeforeClass` to initialize `SHAFT.TestData.JSON` (load once per class)
- Use `@BeforeMethod` to create a fresh `SHAFT.GUI.WebDriver` instance per test
- Use `@AfterMethod(alwaysRun = true)` to call `driver.quit()` — **always** include `alwaysRun = true` so cleanup runs even when a test fails
- Use `@Test` with descriptive method names that summarize the scenario

### Thread Safety for Parallel Execution
- Wrap driver instances in `ThreadLocal<SHAFT.GUI.WebDriver>` when the class is designed for parallel runs
- Never share a single driver instance across test methods

### Assertions
- Always use SHAFT's fluent assertion API: `driver.assertThat()...perform()`
- For browser title: `driver.assertThat().browser().title().contains("...")`
- For element properties: `driver.assertThat().element(locator).domProperty("value").isEqualTo("")`
- Do **not** use TestNG `Assert.*` or JUnit `Assertions.*` directly — use SHAFT's wrappers

### Locators
- Prefer `Locator` builder: `Locator.hasTagName("input").hasAttribute("name", "q").build()`
- Use `By.id`, `By.xpath`, `By.cssSelector` for simple, stable selectors

### Test Data
- Store all test data in JSON files under `src/test/resources/testDataFiles/`
- Load via `new SHAFT.TestData.JSON("filename.json")` — never hardcode values in tests

### Imports
Always include these standard imports for new test classes:
```java
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.testng.annotations.*;
```

### Naming
- Class names: `PascalCase` ending in `Test` or `Tests` (e.g., `BrowserActionsTests`)
- Test method names: `camelCase`, describe the full scenario (e.g., `navigateToPageAndAssertTitleIsCorrect`)

### ⛔ Mandatory Pre-Commit Rules (No Exceptions)
> **You MUST NEVER commit untested code. There are no exceptions to these rules.**

Before committing **any** test code change, you **must**:

1. **Compile**: Run `mvn clean install -DskipTests -Dgpg.skip` and confirm it succeeds
2. **Run Tests**: Execute `mvn test -Dtest=TestClassName` for all new or modified test classes and confirm they pass
3. **Capture Evidence**: Take screenshots of test execution results to provide proof that tests were executed and passed
4. **Review Code**: Review test code for correctness, completeness, and adherence to SHAFT test patterns

Every new feature, bug fix, or code modification **must** have corresponding tests. No code may be committed without verified, passing tests.
