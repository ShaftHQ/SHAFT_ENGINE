# GitHub Copilot Instructions for SHAFT Test Automation

# Purpose
Guide Copilot to generate concise, maintainable test code using SHAFT patterns and best practices.

# General Guidelines
- Use SHAFT's fluent API for browser and element actions.
- Prefer descriptive variable and method names.
- Always initialize drivers and test data in setup methods.
- Use SHAFT assertions for validations.
- Structure tests: setup → action → assertion → teardown.
- **For browser title assertions, always use:**
  `driver.assertThat().browser().title().contains("expectedTitle");`
  instead of any other pattern.
- **Prioritize using SHAFT's assertion API directly** (e.g., `driver.assertThat().element(locator).domProperty("value").isEqualTo("")`) instead of manually retrieving properties and then asserting them.

# Example: TestNG Test Class

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

# Patterns to Follow
- Use @BeforeClass for test data setup.
- Use @BeforeMethod/@AfterMethod for driver lifecycle.
- Use SHAFT's locator builder for element selection.
- Chain actions and assertions for readability.
- Store test data in JSON files and access via SHAFT.TestData.JSON.
- **Use SHAFT assertions directly for element properties and states.**
- **Always use `driver.assertThat().browser().title()` for browser title assertions.**

# Comments for Refactoring
- Update locators and test data as needed for your application.
- Add more test methods following the shown pattern.
- For JUnit5 or Cucumber, adapt setup/teardown annotations accordingly.

# Additional Tips
- Use Copilot to suggest locators, assertions, and test data access.
- Always review generated code for correctness and maintainability.

# End of Instructions
