package {{PACKAGE_NAME}};

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.*;

public class SampleWebTests {
    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;

    // SHAFT Engine User Guide Test
    String shaftUserGuideUrl = "https://shafthq.github.io/";
    By upgradeNowButton = Locator.hasTagName("a").hasAttribute("href", "/SHAFT_ENGINE").hasText("Upgrade Now").build();
    String expectedUrlAfterUpgrade = "https://github.com/ShaftHQ/SHAFT_ENGINE";

    // DuckDuckGo Search Test
    String duckDuckGoUrl = "https://duckduckgo.com/";
    By searchBox = Locator.hasAnyTagName().hasAttribute("name", "q").build();
    By firstSearchResult = Locator.hasTagName("article").isFirst().build();

    @BeforeClass
    public void beforeClass() {
        testData = new SHAFT.TestData.JSON("testData.json");
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test(description = "Navigate to SHAFT Engine user guide, click Upgrade Now button, and assert the current URL")
    public void testNavigateToSHAFTUserGuideAndClickUpgradeNow() {
        driver.browser().navigateToURL(shaftUserGuideUrl)
                .and().element().click(upgradeNowButton)
                .and().browser().navigateToURL(expectedUrlAfterUpgrade)
                .and().assertThat().browser().url().isEqualTo(expectedUrlAfterUpgrade);
    }

    @Test(description = "Navigate to DuckDuckGo, search for SHAFT_Engine, and assert that the first result text contains SHAFT_Engine")
    public void testSearchForSHAFTEngineOnDuckDuckGo() {
        driver.browser().navigateToURL(duckDuckGoUrl)
                .and().element().type(searchBox, testData.getTestData("searchQuery") + Keys.ENTER)
                .and().assertThat(firstSearchResult).text().contains(testData.getTestData("expectedTextInFirstResult"));
    }

    @AfterMethod
    public void afterMethod() {
        driver.quit();
    }
}
