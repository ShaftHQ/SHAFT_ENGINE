package testPackage.legacy;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class ClipboardActionsTests {
    // Declaring webdriver and excelreader instances
    private static final ThreadLocal<WebDriver> driver = new ThreadLocal<>();
    GoogleSearch searchObject;

    @Test
    public void typeTextAndCopyPaste() {
        if (!SHAFT.Properties.platform.targetPlatform().equals(Platform.MAC.name())) {
            searchObject = new GoogleSearch(driver.get()); // initialize a new instance of the page
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
            Validations.assertThat().element(driver.get(), GoogleSearch.getSearchBox_textField()).text().isEqualTo("FIRSTFIRST").perform();
        } else {
            ReportManager.log("Native actions don't work on MAC.");
        }
    }

    @BeforeMethod // Set-up method, to be run once before the first test
    public void beforeMethod() {
        driver.set(new DriverFactory().getDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        new BrowserActions(driver.get()).closeCurrentWindow();
    }
}