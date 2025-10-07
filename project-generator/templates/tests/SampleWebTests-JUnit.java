package {{PACKAGE_NAME}};

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;

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

    @BeforeAll
    public static void beforeAll() {
        // Optional: Setup actions that run once before all tests
    }

    @BeforeEach
    public void beforeEach() {
        driver = new SHAFT.GUI.WebDriver();
        testData = new SHAFT.TestData.JSON("testData.json");
    }

    @Test
    @DisplayName("Navigate to SHAFT Engine user guide, click Upgrade Now button, and assert the current URL")
    public void testNavigateToSHAFTUserGuideAndClickUpgradeNow() {
        driver.browser().navigateToURL(shaftUserGuideUrl)
                .and().element().click(upgradeNowButton)
                .and().browser().navigateToURL(expectedUrlAfterUpgrade)
                .and().assertThat().browser().url().isEqualTo(expectedUrlAfterUpgrade);
    }

    @Test
    @DisplayName("Navigate to DuckDuckGo, search for SHAFT_Engine, and assert that the first result text contains SHAFT_Engine")
    public void testSearchForSHAFTEngineOnDuckDuckGo() {
        driver.browser().navigateToURL(duckDuckGoUrl)
                .and().element().type(searchBox, testData.getTestData("searchQuery") + Keys.ENTER)
                .and().assertThat(firstSearchResult).text().contains(testData.getTestData("expectedTextInFirstResult"));
    }

    @AfterEach
    public void afterEach() {
        driver.quit();
    }

    @AfterAll
    public static void afterAll() {
        // Optional: Cleanup actions that run once after all tests
    }
}
