package cucumberSteps;

import org.openqa.selenium.WebDriver;

import com.shaft.browser.BrowserFactory;
import com.shaft.io.ExcelFileManager;

import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import poms.GoogleSearch;
import poms.GoogleSearchResults;

public class GoogleSearchSteps {

    // Declaring web driver and excel reader instances
    WebDriver driver;
    ExcelFileManager testDataReader;

    // Declaring Page Objects that will be used throughout the test
    GoogleSearch searchObject;
    GoogleSearchResults resultsObject;

    @Given("^Target browser is launched$")
    public void target_browser_is_launched() throws Throwable {
	System.setProperty("testDataFilePath", "src/test/resources/TestDataFiles/testSuite01/TestData.xlsx");
	testDataReader = new ExcelFileManager(System.getProperty("testDataFilePath"));
	driver = BrowserFactory.getBrowser(testDataReader);
    }

    @When("^I navigate to Google search page URL$")
    public void i_navigate_to_Google_search_page_URL() throws Throwable {
	searchObject = new GoogleSearch(driver); // initialize a new instance of the page
	searchObject.navigateToURL(); // Navigate to Page URL
    }

    @Then("^I validate that the correct page has been opened$")
    public void i_validate_that_the_correct_page_has_been_opened() throws Throwable {
	searchObject.assertPageIsOpen(); // Check that the correct page has been opened
    }

    @Then("^I validate that page title is equal to expected title from test data$")
    public void i_validate_that_page_title_is_equal_to_expected_title_from_test_data() throws Throwable {
	searchObject.verifyPageTitle(testDataReader.getCellData("Expected Page Title")); // Check that page title is
											 // equal to expected title from
											 // test data
    }

    @When("^I perform search for the query that is retrieved from test data$")
    public void i_perform_search_for_the_query_that_is_retrieved_from_test_data() throws Throwable {
	searchObject.searchForQuery(testDataReader.getCellData("Search Query")); // Perform search for the query that is
										 // retrieved from test data
    }

    @Then("^I validate that search results counter holds a value \\(expected to pass\\)$")
    public void i_validate_that_search_results_counter_holds_a_value_expected_to_pass() throws Throwable {
	resultsObject = new GoogleSearchResults(driver); // initialize a new instance of the page
	resultsObject.assertResultsStatsExistsAndIsNotEmpty(); // Check that search results counter holds a value
							       // (expected to pass)
    }

    @Then("^I validate that search results counter exists \\(expected to pass\\)$")
    public void i_validate_that_search_results_counter_exists_expected_to_pass() throws Throwable {
	resultsObject.verifyResultsStatsExists(); // Check that search results counter exists (expected to pass)
    }

    @When("^I click the next button to make sure that the framework can scroll element into view before clicking it$")
    public void i_click_the_next_button_to_make_sure_that_the_framework_can_scroll_element_into_view_before_clicking_it() throws Throwable {
	resultsObject.clickNext(); // Clicks the next button to make sure that the framework can scroll element
				   // into view before clicking it
    }
}
