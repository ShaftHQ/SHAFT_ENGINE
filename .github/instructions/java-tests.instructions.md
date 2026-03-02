---
applyTo: "src/test/java/**/*.java"
---

## Java Test File Requirements

When writing or editing test files under `src/test/java/`, follow these SHAFT-specific rules:

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
