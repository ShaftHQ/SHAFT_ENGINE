package testPackage01;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaftEngine.browserActionLibrary.BrowserFactory;
import com.shaftEngine.ioActionLibrary.ReportManager;

import poms.GoogleSearch;

public class Test_clipboardActions {
	// Declaring webdriver and excelreader instances
	WebDriver driver;
	GoogleSearch searchObject;

	@Test
	public void typeTextAndCopyPaste() {
		searchObject = new GoogleSearch(driver); // initialize a new instance of the page
		searchObject.navigateToURL(); // Navigate to Page URL

		searchObject.typeQuery("FIRST");

		searchObject.selectQuery();
		searchObject.copyQuery();
		searchObject.unSelectQuery();
		searchObject.pasteQuery();

		searchObject.selectQuery();
		searchObject.cutQuery();
		searchObject.pasteQuery();
		searchObject.pasteQuery();

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