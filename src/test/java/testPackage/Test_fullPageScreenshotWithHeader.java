package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_fullPageScreenshotWithHeader {
    WebDriver driver;

    @Test(description = "TC001 - Navigate to URL and Assert page title")
    public void navigateToURLandAssertPageTitle() {
        BrowserActions.navigateToURL(driver, "https://www.w3schools.com/howto/howto_js_sticky_header.asp");

        // element screenshot
        Validations.assertThat().element(driver,By.xpath("//div[@id='main']/h2")).exists().perform();

        // element screenshot when element is larger than visible area
        Validations.assertThat().element(driver,By.xpath("//div[@id='main']")).exists().perform();
    }

    @SuppressWarnings("CommentedOutCode")
    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
//	System.setProperty("screenshotParams_skippedElementsFromScreenshot",
//		"//div[@id='topnav'];//div[@id='leftmenuinner']");
        driver = DriverFactory.getDriver();
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        BrowserActions.closeCurrentWindow(driver);
    }
}