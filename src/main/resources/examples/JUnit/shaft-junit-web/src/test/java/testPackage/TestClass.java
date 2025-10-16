package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;

public class TestClass {
    static SHAFT.TestData.JSON testData;
    SHAFT.GUI.WebDriver driver;
    String targetUrl = "https://duckduckgo.com/";

    By logo = By.xpath("//div[contains(@class,'container_fullWidth__1H_L8')]//img");
    By searchBox = Locator.hasAnyTagName().hasAttribute("name", "q").build(); // synonym to By.name("q");
    By firstSearchResult = Locator.hasTagName("article").isFirst().build(); // synonym to By.xpath("(//article)[1]");

    @BeforeAll
    static void beforeClass() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @Test
    void navigateToDuckDuckGoAndAssertBrowserTitleIsDisplayedCorrectly() {
        driver.browser().navigateToURL(targetUrl)
                .and().assertThat().title().contains(testData.get("expectedTitle"));
    }

    @Test
    void navigateToDuckDuckGoAndAssertLogoIsDisplayedCorrectly() {
        driver.browser().navigateToURL(targetUrl)
                .and().element().assertThat(logo).matchesReferenceImage();
    }

    @Test
    void searchForQueryAndAssert() {
        driver.browser().navigateToURL(targetUrl)
                .and().element().type(searchBox, testData.get("searchQuery") + Keys.ENTER)
                .and().assertThat(firstSearchResult).text().doesNotEqual(testData.get("unexpectedInFirstResult"));
    }

    @BeforeEach
    void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterEach
    void afterMethod() {
        driver.quit();
    }
}
