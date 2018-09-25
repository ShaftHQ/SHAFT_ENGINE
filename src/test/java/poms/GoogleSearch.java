package poms;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.shaft.browser.BrowserActions;
import com.shaft.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Verifications;

public class GoogleSearch {
	WebDriver driver;
	// ExcelFileManager testDataReader = new
	// ExcelFileManager(System.getProperty("testDataFilePath"));

	// String url = testDataReader.getCellData("URL");
	By googleLogo_image = By.id("hplogo");
	By searchBox_textField = By.xpath("//input[@id='lst-ib' or @class='lst']");

	public GoogleSearch(WebDriver driver) {
		this.driver = driver;
	}

	public void navigateToURL() {
		BrowserActions.navigateToURL(driver, "https://www.google.com/ncr");
	}

	public void searchForQuery(String searchQuery) {
		ElementActions.type(driver, searchBox_textField, "351");
		Assertions.assertEquals(1, 1, true);
		Assertions.assertEquals(1, 111, false);

		Assertions.assertElementAttribute(driver, searchBox_textField, "text", "351", true);
		Assertions.assertElementAttribute(driver, searchBox_textField, "text", "1", false);

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
		Assertions.assertElementExists(driver, googleLogo_image, true);
	}

	public void verifyPageTitle(String expectedValue) {
		Verifications.verifyBrowserAttribute(driver, "Title", expectedValue, true);
		Verifications.verifyBrowserAttribute(driver, "Title", "Not Google", false);

	}

}
