package testPackage01;

import com.shaft.driver.DriverFactoryHelper;
import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_lazyLoading {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private static final String url = "https://onepagelove.com/tag/lazy-load";

    @Test
    public void test() {
        driver.get().browser().navigateToURL(url);
        if (!DriverFactoryHelper.getTargetBrowserName().toLowerCase().contains("safari")) {
            By needsScrolling = By.cssSelector("h2 [title='The Rollercoaster Life']");
            driver.get().element().scrollToElement(needsScrolling);
            driver.get().assertThat().element(needsScrolling).text().doesNotEqual("").withCustomReportMessage("find element which requires scrolling to exist on the page DOM").perform();
        }
        driver.get().browser().capturePageSnapshot();
    }

    @BeforeMethod(description = "Initialize Driver")
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(description = "Quit Driver")
    public void afterMethod() {
        driver.get().quit();
    }

}
