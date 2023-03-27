package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;

public class Test_threadSafe_grid4 {
    private final ThreadLocal<WebDriver> driver = new ThreadLocal<>();
    private final By searchBar = By.id("search_form_input_homepage");
    private final By secondSearchResult = By.xpath("(//a[contains(@class,'js-result-title-link')])[1]");

//    @BeforeClass
    @SuppressWarnings({"CommentedOutCode", "EmptyMethod"})
    public void setupRemoteParallelExecution(){
//        System.setProperty("executionAddress","localhost:4444");
//        System.setProperty("targetOperatingSystem","Linux");
//        System.setProperty("maximumPerformanceMode","2");
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
        BrowserActions.getInstance().navigateToURL("https://duckduckgo.com/?");
        new ElementActions(driver.get()).type(searchBar, "SHAFT_Engine")
                .keyPress(searchBar, Keys.ENTER);
        Validations.assertThat()
                .element(driver.get(), secondSearchResult)
                .text()
                .contains("ShaftHQ/SHAFT_ENGINE")
                .withCustomReportMessage("Asserting that the second search result contains 'ShaftHQ/SHAFT_ENGINE'")
                .perform();
        BrowserActions.getInstance().closeCurrentWindow();
    }
}
