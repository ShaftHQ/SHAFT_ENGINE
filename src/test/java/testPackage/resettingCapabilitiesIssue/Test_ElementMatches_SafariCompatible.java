package testPackage.resettingCapabilitiesIssue;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_ElementMatches_SafariCompatible {
    SHAFT.GUI.WebDriver driver;
    String url = "https://duckduckgo.com/?";
    By searchbar = By.xpath("//*[@id='search_form_input_homepage'] | //input[@name='q']");

    @Test
    public void test(){
        driver.browser().navigateToURL(url);
        driver.assertThat().element(searchbar).matchesReferenceImage().perform();
    }
    @SuppressWarnings("CommentedOutCode")
    @BeforeMethod
    public void beforeMethod(){
        // remote browserstack server
//        System.setProperty("executionAddress","browserstack");
//        System.setProperty("targetOperatingSystem","Mac");
//        System.setProperty("targetBrowserName","Safari");
//        System.setProperty("browserStack.browserVersion","15.3");
//        System.setProperty("browserStack.osVersion", "Monterey");
        driver =  new SHAFT.GUI.WebDriver();
    }
    @AfterMethod(alwaysRun = true)
    public void afterMethod(){
        driver.quit();
    }
}
