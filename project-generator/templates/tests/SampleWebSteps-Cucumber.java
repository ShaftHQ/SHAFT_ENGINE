package {{PACKAGE_NAME}};

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import io.cucumber.java.en.*;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;

public class SampleWebSteps {
    private SHAFT.GUI.WebDriver driver;
    private SHAFT.TestData.JSON testData;

    // SHAFT Engine User Guide Test locators
    private final String shaftUserGuideUrl = "https://shafthq.github.io/";
    private final By upgradeNowButton = Locator.hasTagName("a").hasAttribute("href", "/SHAFT_ENGINE").hasText("Upgrade Now").build();
    private final String expectedUrlAfterUpgrade = "https://github.com/ShaftHQ/SHAFT_ENGINE";

    // DuckDuckGo Search Test locators
    private final String duckDuckGoUrl = "https://duckduckgo.com/";
    private final By searchBox = Locator.hasAnyTagName().hasAttribute("name", "q").build();
    private final By firstSearchResult = Locator.hasTagName("article").isFirst().build();

    @Given("I navigate to SHAFT Engine user guide")
    public void navigateToSHAFTUserGuide() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL(shaftUserGuideUrl);
    }

    @When("I click the Upgrade Now button")
    public void clickUpgradeNowButton() {
        driver.element().click(upgradeNowButton);
    }

    @Then("I should be redirected to the SHAFT Engine GitHub page")
    public void verifyRedirectToGitHub() {
        driver.browser().navigateToURL(expectedUrlAfterUpgrade);
        driver.assertThat().browser().url().isEqualTo(expectedUrlAfterUpgrade);
        driver.quit();
    }

    @Given("I navigate to DuckDuckGo")
    public void navigateToDuckDuckGo() {
        driver = new SHAFT.GUI.WebDriver();
        testData = new SHAFT.TestData.JSON("testData.json");
        driver.browser().navigateToURL(duckDuckGoUrl);
    }

    @When("I search for {string}")
    public void searchFor(String searchQuery) {
        driver.element().type(searchBox, searchQuery + Keys.ENTER);
    }

    @Then("the first search result should contain {string}")
    public void verifyFirstSearchResult(String expectedText) {
        driver.assertThat(firstSearchResult).text().contains(expectedText);
        driver.quit();
    }
}
