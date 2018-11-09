package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserActions;
import com.shaft.browser.BrowserFactory;
import com.shaft.validation.Assertions;

public class Test_fullPageScreenshotWithHeader {
	WebDriver driver;

	@Test(priority = 0, description = "TC001 - Navigate to URL and Assert page title")
	public void navigateToURLandAssertPageTitle() {
		BrowserActions.navigateToURL(driver, "https://www.w3schools.com/howto/howto_js_sticky_header.asp");
		Assertions.assertElementExists(driver, By.xpath("//*[@id='main']/h2"), true);
	}

	@BeforeClass // Set-up method, to be run once before the first test
	public void beforeClass() {
		System.setProperty("screenshotParams_skippedElementsFromScreenshot", "//div[@id='topnav'];//div[@id='leftmenuinner']");
		driver = BrowserFactory.getBrowser();
	}
}