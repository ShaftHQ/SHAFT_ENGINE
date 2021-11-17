package testPackage01;

import com.microsoft.playwright.Page;
import com.shaft.driver.DriverFactory;
import com.shaft.tools.io.ExcelFileManager;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import poms.GoogleSearch;
import poms.GoogleSearchResults;

public class Test_google_playwright {
    // Declaring webdriver and excelreader instances
    Page page;
    ExcelFileManager testDataReader;
    // Declaring Page Objects that will be used throughout the test
    GoogleSearch searchObject;
    GoogleSearchResults resultsObject;

    @Test(description = "TC001 - Navigate to URL and Verify Google Logo is Displayed")
    public void navigateToURLandVerifyGoogleLogoIsDisplayed() {
        searchObject = new GoogleSearch(page); // initialize a new instance of the page
        searchObject.navigateToURL(); // Navigate to Page URL
        searchObject.assertPageIsOpen(); // Check that the correct page has been opened
    }

    @Test(dependsOnMethods = {"navigateToURLandVerifyGoogleLogoIsDisplayed"}, description = "TC002 - Search for Query and Assert that the number of results is displayed")
    public void searchForQueryandAssertResultsNumDisplayed() {
        searchObject = new GoogleSearch(page); // initialize a new instance of the page
        // temp steps for debugging changes to the type function
        searchObject.searchForQuery(testDataReader.getCellData("Search Query 2")); // Perform search for the query that
        // is
        // retrieved from test data
        resultsObject = new GoogleSearchResults(page); // initialize a new instance of the page
        resultsObject.assertResultsStatsExistsAndIsNotEmpty(); // Check that search results counter holds a value
        // (expected to pass)
    }

     @Test(dependsOnMethods = {"searchForQueryandAssertResultsNumDisplayed"}, description = "TC003 - Clicks the next button to make sure that the framework can scroll element into view before clicking it")
    public void clickNextThrice() {
        resultsObject = new GoogleSearchResults(page); // initialize a new instance of the page
        resultsObject.clickNext(); // Clicks the next button to make sure that the framework can scroll element
        // into
        // view before clicking it
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
        System.setProperty("testDataFilePath", System.getProperty("testDataFolderPath")+"testSuite01/TestData.xlsx");
        testDataReader = new ExcelFileManager(System.getProperty("testDataFilePath"));
        page = DriverFactory.getPlaywrightDriver();
    }

    @AfterClass
    public void afterClass() {
        DriverFactory.closePlayWrightDriver();
    }
}