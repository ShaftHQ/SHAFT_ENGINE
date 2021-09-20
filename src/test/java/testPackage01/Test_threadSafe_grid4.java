package testPackage01;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_threadSafe_grid4 {
    private ThreadLocal<WebDriver> driver = new ThreadLocal<>();
    private final By searchBar = By.id("search_form_input_homepage");
    private final By secondSearchResult = By.xpath("(//a[contains(@class,'js-result-title-link')])[1]");

//    @BeforeClass
    public void setupRemoteParallelExecution(){
        System.setProperty("executionAddress","localhost:4444");
        System.setProperty("targetOperatingSystem","Linux-64");
        System.setProperty("maximumPerformanceMode","2");
    }

//    @Test
    public void testThread1(){
        driver.set(DriverFactory.getDriver());
        runTestSteps();
    }

//    @Test
    public void testThread2(){
        driver.set(DriverFactory.getDriver());
        runTestSteps();
    }

//    @Test
    public void testThread3(){
        driver.set(DriverFactory.getDriver());
        runTestSteps();
    }

    private void runTestSteps(){
        BrowserActions.navigateToURL(driver.get(), "https://duckduckgo.com/");
        new ElementActions(driver.get()).type(searchBar, "SHAFT_Engine")
                .keyPress(searchBar, Keys.ENTER);
        Validations.assertThat()
                .element(driver.get(), secondSearchResult)
                .text()
                .contains("MohabMohie/SHAFT_ENGINE")
                .withCustomReportMessage("Asserting that the second search result contains 'MohabMohie/SHAFT_ENGINE'")
                .perform();
        BrowserActions.closeCurrentWindow(driver.get());
    }
}
