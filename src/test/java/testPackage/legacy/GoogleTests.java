package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ExcelFileManager;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;
import poms.GoogleSearchResults;

public class GoogleTests {
    // Declaring webdriver and excelreader instances
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    ExcelFileManager testDataReader;
    // Declaring Page Objects that will be used throughout the test
    GoogleSearch searchObject;
    GoogleSearchResults resultsObject;

    @Test(description = "TC001 - Navigate to URL and Verify Google Logo is Displayed")
    public void navigateToURLandVerifyGoogleLogoIsDisplayed() {
        searchObject = new GoogleSearch(driver.get().getDriver()); // initialize a new instance of the page
        searchObject.navigateToURL(); // Navigate to Page URL
        searchObject.assertPageIsOpen(); // Check that the correct page has been opened
    }

    @Test(description = "TC002 - Search for Query and Assert that the number of results is displayed")
    public void searchForQueryandAssertResultsNumDisplayed() {
        searchObject = new GoogleSearch(driver.get().getDriver()); // initialize a new instance of the page
        searchObject.navigateToURL(); // Navigate to Page URL
        // temp steps for debugging changes to the type function
        searchObject.searchForQuery(testDataReader.getCellData("Search Query 2")); // Perform search for the query that
        // is
        // retrieved from test data
        resultsObject = new GoogleSearchResults(driver.get().getDriver()); // initialize a new instance of the page
        resultsObject.assertResultsStatsExistsAndIsNotEmpty(); // Check that search results counter holds a value
        // (expected to pass)
    }

    @BeforeMethod // Set-up method
    public void beforeClass() {
        testDataReader = new ExcelFileManager(SHAFT.Properties.paths.testData() + "testSuite01/TestData.xlsx");
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterClass() {
        driver.get().quit();
    }
}