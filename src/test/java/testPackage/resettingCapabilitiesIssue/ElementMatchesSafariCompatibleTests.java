package testPackage.resettingCapabilitiesIssue;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ElementMatchesSafariCompatibleTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String url = "https://duckduckgo.com/?";
    By searchbar = By.xpath("//*[@id='search_form_input_homepage'] | //input[@name='q']");

    @Test
    public void test(){
        driver.get().browser().navigateToURL(url);
        driver.get().assertThat().element(searchbar).matchesReferenceImage().perform();
    }
    @SuppressWarnings("CommentedOutCode")
    @BeforeMethod
    public void beforeMethod() {
        // remote browserstack server
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.platform.set().targetPlatform(Platform.MAC.name());
//        SHAFT.Properties.web.set().targetBrowserName(Browser.SAFARI.browserName());
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().browserVersion("15.3");
//        SHAFT.Properties.browserStack.set().osVersion("Monterey");
        driver.set(new SHAFT.GUI.WebDriver());
    }
    @AfterMethod(alwaysRun = true)
    public void afterMethod(){
        driver.get().quit();
    }
}
