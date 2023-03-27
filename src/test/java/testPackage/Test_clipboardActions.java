package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class Test_clipboardActions {
    // Declaring webdriver and excelreader instances
    WebDriver driver;
    GoogleSearch searchObject;

    @Test
    public void typeTextAndCopyPaste() {
        if (!System.getProperty("targetOperatingSystem").toLowerCase().contains("mac")) {
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
            Validations.assertThat().element(driver, GoogleSearch.getSearchBox_textField()).text().equals("FIRSTFIRST");
        } else {
            ReportManager.log("Native actions don't work on MAC.");
        }
    }

    @BeforeMethod // Set-up method, to be run once before the first test
    public void beforeMethod() {
        driver = DriverFactory.getDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver);
    }
}