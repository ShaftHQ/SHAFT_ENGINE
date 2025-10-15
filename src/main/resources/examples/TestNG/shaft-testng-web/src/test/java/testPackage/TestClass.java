package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class TestClass {
    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;

    String targetUrl = "https://duckduckgo.com/";

    By logo = By.xpath("//div[contains(@class,'container_fullWidth__1H_L8')]//img");
    By searchBox = Locator.hasAnyTagName().hasAttribute("name", "q").build(); // synonym to By.name("q");
    By firstSearchResult = Locator.hasTagName("article").isFirst().build(); // synonym to By.xpath("(//article)[1]");

    @Test
    public void navigateToDuckDuckGoAndAssertBrowserTitleIsDisplayedCorrectly() {
        driver.browser().navigateToURL(targetUrl)
                .and().assertThat().title().contains(testData.getTestData("expectedTitle"));
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
