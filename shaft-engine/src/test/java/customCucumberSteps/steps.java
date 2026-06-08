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

    @Then("I assert that the {string} attribute of the browser, equals {string}")
    public void iAssertThatTheAttributeOfTheBrowserEquals(String browserAttribute, String expectedValue) {
        driver.assertThat().browser()
                .attribute(browserAttribute)
                .isEqualTo(expectedValue)
                .withCustomReportMessage("I Assert that the [" + browserAttribute + "] attribute of the browser, equals [" + expectedValue + "]")
                .perform();
    }
}
