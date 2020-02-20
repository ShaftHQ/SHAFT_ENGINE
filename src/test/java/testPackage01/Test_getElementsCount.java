package testPackage01;

import com.shaft.gui.browser.BrowserFactory;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
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
}