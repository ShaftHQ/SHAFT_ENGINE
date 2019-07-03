package cucumberSteps;

import org.openqa.selenium.WebDriver;

import com.shaft.gui.browser.BrowserFactory;
import com.shaft.tools.io.ExcelFileManager;

import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import poms.GoogleSearch;
import poms.GoogleSearchResults;

public class GoogleSearchStepdefs {
    // Declaring web driver and excel reader instances
    WebDriver driver;
    ExcelFileManager testDataReader;

    // Declaring Page Objects that will be used throughout the test
    GoogleSearch searchObject;
    GoogleSearchResults resultsObject;

    @Given("Test data file {string} is ready")
    public void testData_file_is_ready(String filePath) throws Throwable {
	System.setProperty("testDataFilePath", "src/test/resources/TestDataFiles/testSuite01/TestData.xlsx");
	testDataReader = new ExcelFileManager(System.getProperty("testDataFilePath"));
    }

    @Given("Target browser is launched")
    public void target_browser_is_launched() throws Throwable {
	driver = BrowserFactory.getBrowser();
    }

    @When("I navigate to Google search page URL")
    public void i_navigate_to_Google_search_page_URL() throws Throwable {
	searchObject = new GoogleSearch(driver); // initialize a new instance of the page
	searchObject.navigateToURL(); // Navigate to Page URL
    }

    @Then("I validate that the correct page has been opened")
    public void i_validate_that_the_correct_page_has_been_opened() throws Throwable {
	searchObject = new GoogleSearch(driver);
	searchObject.assertPageIsOpen(); // Check that the correct page has been opened
    }

    @Then("I validate that page title is equal to expected title from test data")
    public void i_validate_that_page_title_is_equal_to_expected_title_from_test_data() throws Throwable {
	searchObject = new GoogleSearch(driver);
	searchObject.verifyPageTitle(testDataReader.getCellData("Expected Page Title")); // Check that page title is
											 // equal to expected title from
											 // test data
    }

    @When("I perform search for the query that is retrieved from test data")
    public void i_perform_search_for_the_query_that_is_retrieved_from_test_data() throws Throwable {
	searchObject = new GoogleSearch(driver);
	searchObject.searchForQuery(testDataReader.getCellData("Search Query 2")); // Perform search for the query that
										   // is
										   // retrieved from test data
    }

    @Then("I validate that search results counter holds a value")
    public void i_validate_that_search_results_counter_holds_a_value() throws Throwable {
	resultsObject = new GoogleSearchResults(driver); // initialize a new instance of the page
	resultsObject.assertResultsStatsExistsAndIsNotEmpty(); // Check that search results counter holds a value
							       // (expected to pass)
    }

    @Then("I validate that search results counter exists")
    public void i_validate_that_search_results_counter_exists() throws Throwable {
	resultsObject = new GoogleSearchResults(driver);
	resultsObject.verifyResultsStatsExists(); // Check that search results counter exists (expected to pass)
    }

    @When("I click the next button to make sure that the framework can scroll element into view before clicking it")
    public void i_click_the_next_button_to_make_sure_that_the_framework_can_scroll_element_into_view_before_clicking_it()
	    throws Throwable {
	resultsObject = new GoogleSearchResults(driver);
	resultsObject.clickNext(); // Clicks the next button to make sure that the framework can scroll element
				   // into view before clicking it
	resultsObject.clickNext(); // Clicks the next button to make sure that the framework can scroll element
	// into view before clicking it
	resultsObject.clickNext(); // Clicks the next button to make sure that the framework can scroll element
	// into view before clicking it
	resultsObject.assert10ResultsPerPage();
    }

    @Then("I find that ten results are displayed in this page")
    public void tenResultsAreDisplayed() throws Throwable {
	resultsObject = new GoogleSearchResults(driver);
	resultsObject.assert10ResultsPerPage();
    }

}
