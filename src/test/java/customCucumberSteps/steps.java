package customCucumberSteps;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import poms.GoogleSearch;

public class steps {
    /**
     * Requires the following settings in the cucumber.properties file to run
     * <p>
     * cucumber.filter.tags=
     * cucumber.features=src/test/resources/CustomCucumberFeatures
     * cucumber.extraGlue=
     * cucumber.plugin=pretty, html:allure-results/cucumberReport.html, com.shaft.listeners.CucumberFeatureListener
     * cucumber.glue=customCucumberSteps
     */
    private SHAFT.GUI.WebDriver driver;
    private DriverFactoryHelper helper;
    @Given("I open the target browser")
    public void i_open_the_target_browser() {
        driver = new SHAFT.GUI.WebDriver();
    }
    @When("I navigate to {string}")
    public void i_navigate_to(String pageName) {
        if (pageName.equals("Google Home")) {
            driver.browser().navigateToURL("https://www.google.com/ncr", "https://www.google.com");
        }
    }
    @When("I search for {string}")
    public void i_search_for(String query) {
        By searchBox = GoogleSearch.getSearchBox_textField();
        driver.element().type(searchBox, "SHAFT_Engine")
                .keyPress(searchBox, Keys.ENTER);
    }
    @Then("The first result text will contain {string}")
    public void the_first_result_text_will_contain(String expectedText) {
        By firstResult = By.xpath("(//h3[contains(@class,'LC20lb')])[1]");
        driver.element().assertThat(firstResult)
                .text()
                .contains(expectedText)
                .withCustomReportMessage("Check that first result text contains ["+expectedText+"]")
                .perform();
    }
}
