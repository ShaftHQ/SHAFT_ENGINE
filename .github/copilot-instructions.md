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
  - `docs/` - Documentation
  - `.github/workflows/` - CI/CD pipelines

## Build & Test Commands
- **Build**: `mvn clean install -DskipTests -Dgpg.skip`
- **Run Tests**: `mvn test`
- **Run Specific Tests**: `mvn test -Dtest=TestClassName`
- **Generate JavaDocs**: `mvn javadoc:javadoc`
- **Code Analysis**: CodeQL runs automatically on PRs

## General Guidelines

### Code Quality
- Use SHAFT's fluent API for browser and element actions
- Prefer descriptive variable and method names following Java conventions
- Always initialize drivers and test data in setup methods
- Use SHAFT assertions for validations
- Structure tests: setup → action → assertion → teardown
- Add JavaDocs for all public methods and classes
- Follow existing code formatting and style patterns in the repository

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

## Security Best Practices
- Never hardcode credentials, API keys, or sensitive data
- Use environment variables or secure configuration files for secrets
- Validate all user inputs in test data
- Follow secure coding practices from the CONTRIBUTING.md file
- All code changes undergo security scanning via CodeQL

## Testing Guidelines
- Write tests that validate your changes
- Follow existing test patterns in `src/test/java/`
- Ensure tests are independent and can run in parallel
- Clean up resources in teardown methods
- Use meaningful test names that describe what is being tested

## Common Anti-Patterns to Avoid
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
**Note**: These instructions are designed to help GitHub Copilot coding agent generate high-quality, maintainable code that follows SHAFT framework best practices. Always review and test generated code before committing.
