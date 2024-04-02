package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;

public class CrossBrowserExecutionTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private static final By serchbox_input = By.cssSelector("input[name='q']");
    private static final By resultstats_label = By.cssSelector("div#result-stats");

    //@Test(description = "When I navigate to Google Home, and I search for 'SHAFT_Engine', then the result stats should not be empty.")
    public void test() {
        //navigate to google
        driver.get().browser().navigateToURL("https://www.google.com/");

        //search for SHAFT_Engine
        driver.get().element().type(serchbox_input, "SHAFT_Engine").keyPress(serchbox_input, Keys.ENTER);

        //assert that result stats is not empty
        driver.get().assertThat().element(resultstats_label).text().doesNotEqual("").withCustomReportMessage("Check that result stats are not empty").perform();
    }

    @BeforeMethod(description = "Initialize Browser")
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(description = "Teardown Browser", alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
