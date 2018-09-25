package testPackage01;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserFactory;
import com.shaft.io.ReportManager;

import poms.GoogleSearch;
import poms.GoogleSearchResults;

public class Test_getElementsCount {
	// Declaring webdriver and excelreader instances
	WebDriver driver;
	GoogleSearch searchObject;
	GoogleSearchResults resultsObject;

	@Test
	public void assertElementsCount() {
		searchObject = new GoogleSearch(driver); // initialize a new instance of the page
		searchObject.navigateToURL(); // Navigate to Page URL

		searchObject.searchForQuery("Selenium"); // Perform search for the query
		resultsObject = new GoogleSearchResults(driver); // initialize a new instance of the page

		resultsObject.assert10ResultsPerPage();
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