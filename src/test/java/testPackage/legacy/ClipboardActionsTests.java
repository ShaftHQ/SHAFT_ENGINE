package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.openqa.selenium.Platform;
import org.testng.annotations.Test;
import poms.GoogleSearch;
import testPackage.Tests;

public class ClipboardActionsTests extends Tests {
    GoogleSearch searchObject;

    @Test
    public void typeTextAndCopyPaste() {
        if (!SHAFT.Properties.platform.targetPlatform().equals(Platform.MAC.name())) {
            searchObject = new GoogleSearch(driver.get().getDriver()); // initialize a new instance of the page
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
            Validations.assertThat().element(driver.get().getDriver(), GoogleSearch.getSearchBox_textField()).text().isEqualTo("FIRSTFIRST").perform();
        } else {
            ReportManager.log("Native actions don't work on MAC.");
        }
    }
}