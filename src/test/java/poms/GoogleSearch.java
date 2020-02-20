package poms;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import com.shaft.validation.Verifications;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class GoogleSearch {
    WebDriver driver;
    // ExcelFileManager testDataReader = new
    // ExcelFileManager(System.getProperty("testDataFilePath"));

    // String url = testDataReader.getCellData("URL");
    By googleLogo_image = By.id("hplogo");
    By searchBox_textField = By.xpath("//input[@id='lst-ib' or @class='lst' or @name='q']");

    public GoogleSearch(WebDriver driver) {
        this.driver = driver;
    }

    public void navigateToURL() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
    }

    public void searchForQuery(String searchQuery) {
        ElementActions.type(driver, searchBox_textField, "351");
        Assertions.assertEquals(1, 1);
        Assertions.assertEquals(1, 111, AssertionComparisonType.EQUALS, AssertionType.NEGATIVE);

        Assertions.assertElementAttribute(driver, searchBox_textField, "text", "351", AssertionComparisonType.EQUALS,
                AssertionType.POSITIVE);
        Assertions.assertElementAttribute(driver, searchBox_textField, "text", "1", AssertionComparisonType.EQUALS,
                AssertionType.NEGATIVE);

        ElementActions.type(driver, searchBox_textField, searchQuery);
        ElementActions.keyPress(driver, searchBox_textField, "Enter");
    }

    public void typeQuery(String searchQuery) {
        ElementActions.type(driver, searchBox_textField, searchQuery);
    }

    public void copyQuery() {
        ElementActions.clipboardActions(driver, searchBox_textField, "copy");
    }

    public void pasteQuery() {
        ElementActions.clipboardActions(driver, searchBox_textField, "paste");
    }

    public void cutQuery() {
        ElementActions.clipboardActions(driver, searchBox_textField, "cut");
    }

    public void selectQuery() {
        ElementActions.clipboardActions(driver, searchBox_textField, "select all");
    }

    public void unSelectQuery() {
        ElementActions.clipboardActions(driver, searchBox_textField, "unselect");
    }

    public void assertPageIsOpen() {
        Assertions.assertElementExists(driver, googleLogo_image, AssertionType.POSITIVE);
    }

    public void verifyPageTitle(String expectedValue) {
        Verifications.verifyBrowserAttribute(driver, "Title", expectedValue, 1, true);
        Verifications.verifyBrowserAttribute(driver, "Title", "Not Google", 1, false);

    }

}
