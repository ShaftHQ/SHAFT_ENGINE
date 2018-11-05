package testPackage01;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserActions;
import com.shaft.browser.BrowserFactory;
import com.shaft.io.ReportManager;

public class Test_navigation {
    WebDriver driver;

    @Test
    public void f() {
	BrowserActions.navigateToURL(driver, "http://35.184.27.139:9091/incorta/#/login");
	// BrowserActions.navigateToURL(driver, "https://www.google.com/ncr",
	// "google.com");
	// BrowserActions.navigateToURL(driver, "https://www.w3schools.com/");
	// BrowserActions.navigateToURL(driver,
	// "http://35.184.27.139:9091/incorta/#/~/security/users",
	// "http://35.184.27.139:9091/incorta/#/login");
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
	driver = BrowserFactory.getBrowser();
    }

    @AfterClass(alwaysRun = true) // Tear-down method, to be run once after the last test
    public void afterClass() {
	BrowserFactory.closeAllDrivers();
	ReportManager.getFullLog();
    }

    @AfterMethod
    public void afterMethod() {
	ReportManager.getTestLog();
    }
}
