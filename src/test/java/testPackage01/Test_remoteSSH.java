package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaftEngine.browserActionLibrary.BrowserActions;
import com.shaftEngine.browserActionLibrary.BrowserFactory;
import com.shaftEngine.elementActionLibrary.ElementActions;
import com.shaftEngine.ioActionLibrary.ReportManager;
import com.shaftEngine.validationsLibrary.Assertions;

public class Test_remoteSSH {
	// Declaring webdriver instance
	WebDriver driver;

	@Test
	public void test_executeRemoteSSH() {
		BrowserActions.navigateToURL(driver, "https://www.google.com/ncr");
		ElementActions.type(driver, By.id("lst-ib"),
				"INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
		Assertions.assertElementAttribute(driver, By.id("lst-ib"), "text",
				"INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.", true);
	}

	@BeforeClass // Set-up method, to be run once before the first test
	public void beforeClass() {
		driver = BrowserFactory.getBrowser();
	}

	@AfterClass(alwaysRun = true) // Tear-down method, to be run once after the last test
	public void afterClass() {
		BrowserFactory.closeAllDrivers();
	}

	@AfterMethod
	public void afterMethod() {
		ReportManager.getTestLog();
	}
}
