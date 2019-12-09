package testPackage01;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.gui.browser.BrowserFactory;

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
}