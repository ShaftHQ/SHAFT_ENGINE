package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionType;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_fullPageScreenshotWithHeader {
    WebDriver driver;

    @Test(priority = 0, description = "TC001 - Navigate to URL and Assert page title")
    public void navigateToURLandAssertPageTitle() {
        BrowserActions.navigateToURL(driver, "https://www.w3schools.com/howto/howto_js_sticky_header.asp");

        // element screenshot
        Assertions.assertElementExists(driver, By.xpath("//div[@id='main']/h2"), AssertionType.POSITIVE);

        // element screenshot when element is larger than visible area
        Assertions.assertElementExists(driver, By.xpath("//div[@id='main']"), AssertionType.POSITIVE);

    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
//	System.setProperty("screenshotParams_skippedElementsFromScreenshot",
//		"//div[@id='topnav'];//div[@id='leftmenuinner']");
        driver = BrowserFactory.getBrowser();
    }

    @AfterClass
    public void afterClass(){
        BrowserActions.closeCurrentWindow(driver);
    }
}