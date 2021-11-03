package customCucumberSteps;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import io.cucumber.java.Before;
import io.cucumber.java.BeforeAll;
import io.cucumber.java.BeforeStep;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;

public class steps {
    /**
     * Requires the following settings in the cucumber.properties file to run
     *
     * cucumber.filter.tags=
     * cucumber.features=src/test/resources/CustomCucumberFeatures
     * cucumber.extraGlue=
     * cucumber.plugin=pretty, html:allure-results/cucumberReport.html, com.shaft.tools.listeners.CucumberFeatureListener
     * cucumber.glue=customCucumberSteps
     */
    private WebDriver driver;
    @Given("I open the target browser")
    public void i_open_the_target_browser() {
        driver = DriverFactory.getDriver();
    }
    @When("I navigate to {string}")
    public void i_navigate_to(String pageName) {
        switch (pageName){
            case "Google Home" -> BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        }
    }
    @When("I search for {string}")
    public void i_search_for(String query) {
        By searchBox = By.name("q");
        new ElementActions(driver).type(searchBox, "SHAFT_Engine")
                .keyPress(searchBox, Keys.ENTER);
    }
    @Then("The first result text will contain {string}")
    public void the_first_result_text_will_contain(String expectedText) {
        By firstResult = By.xpath("(//h3[contains(@class,'LC20lb')])[1]");
        Validations.assertThat()
                .element(driver, firstResult)
                .text()
                .contains(expectedText)
                .withCustomReportMessage("Check that first result text contains ["+expectedText+"]")
                .perform();
    }
    @Before
    public void before(){
        ReportManager.log("This is @Before");
    }
    @BeforeAll
    public static void beforeAll(){
        ReportManager.log("This is @BeforeAll");
    }
    @BeforeStep
    public void beforeStep(){
        ReportManager.log("This is @BeforeStep");
    }
}
