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

    By logo = By.xpath("//img[@class='mw-logo-icon']");
    By searchBox = Locator.hasAnyTagName().hasAttribute("id", "searchInput").build(); // synonym to By.id("searchInput");
    By firstSearchResult = By.xpath("(//div[contains(@class,'mw-search-result-heading')])[1]//a");

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

    @Test
    public void searchForQueryAndAssert() {
        driver.browser().navigateToURL(targetUrl)
                .and().element().type(searchBox, testData.get("searchQuery") + Keys.ENTER)
                .and().element().click(firstSearchResult);
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
