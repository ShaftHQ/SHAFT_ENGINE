package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class TestClass {
    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;

    String targetUrl = "https://en.wikipedia.org/wiki/Main_Page";

    // Wikipedia's shared page-header logo lockup (icon + wordmark + tagline): present, static, and
    // identically rendered on every article -- unlike the small 50x50 icon alone, this larger
    // composite element reliably has a non-zero rendered size the moment the header paints.
    By logo = By.xpath("//a[@class='mw-logo']");
    By searchBox = Locator.hasAnyTagName().hasAttribute("id", "searchInput").build(); // synonym to By.id("searchInput");

    @Test
    public void navigateToWikipediaAndAssertBrowserTitleIsDisplayedCorrectly() {
        driver.browser().navigateToURL(targetUrl)
                .and().assertThat().title().contains(testData.get("expectedTitle"));
    }

    @Test
    public void navigateToWikipediaAndAssertLogoIsDisplayedCorrectly() {
        driver.browser().navigateToURL(targetUrl)
                .and().element().assertThat(logo).matchesReferenceImage();
    }

    // Typing the exact title of an existing Wikipedia article and submitting the search form
    // (Enter) deterministically redirects straight to that article -- MediaWiki's "go to page"
    // behavior for an exact title match, verified via `curl -L` (search=Software+testing ->
    // wikipedia.org/wiki/Software_testing). This avoids asserting on Wikipedia's live, ranked
    // full-text search results, which drift over time and are not a stable test oracle.
    @Test
    public void searchForQueryAndAssert() {
        driver.browser().navigateToURL(targetUrl)
                .and().element().type(searchBox, testData.get("searchQuery") + Keys.ENTER);
        driver.assertThat().browser().title().contains(testData.get("expectedResultTitle"));
        driver.assertThat().element(By.tagName("body")).text().contains(testData.get("expectedResultText"));
    }

    @BeforeClass
    public void beforeClass() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod
    public void afterMethod(){
        driver.quit();
    }
}
