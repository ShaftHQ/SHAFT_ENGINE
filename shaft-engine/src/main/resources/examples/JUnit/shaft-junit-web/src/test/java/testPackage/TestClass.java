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
    String targetUrl = "https://en.wikipedia.org/wiki/Main_Page";

    By logo = By.xpath("//img[@class='mw-logo-icon']");
    By searchBox = Locator.hasAnyTagName().hasAttribute("id", "searchInput").build(); // synonym to By.id("searchInput");
    By firstSearchResult = By.xpath("(//div[contains(@class,'mw-search-result-heading')])[1]//a");

    @Test
    void navigateToWikipediaAndAssertBrowserTitleIsDisplayedCorrectly() {
        driver.browser().navigateToURL(targetUrl)
                .and().assertThat().title().contains(testData.get("expectedTitle"));
    }

    @Test
    void navigateToWikipediaAndAssertLogoIsDisplayedCorrectly() {
        driver.browser().navigateToURL(targetUrl)
                .and().element().assertThat(logo).matchesReferenceImage();
    }

    @Test
    void searchForQueryAndAssert() {
        driver.browser().navigateToURL(targetUrl)
                .and().element().type(searchBox, testData.get("searchQuery") + Keys.ENTER)
                .and().element().click(firstSearchResult);
        driver.assertThat().browser().title().contains(testData.get("expectedResultTitle"));
        driver.assertThat().element(By.tagName("body")).text().contains(testData.get("expectedResultText"));
    }

    @BeforeAll
    static void beforeAll() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @BeforeEach
    void beforeEach() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterEach
    void afterEach() {
        driver.quit();
    }
}
