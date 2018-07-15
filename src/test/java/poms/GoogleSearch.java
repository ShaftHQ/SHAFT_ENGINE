package poms;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.shaftEngine.browserActionLibrary.BrowserActions;
import com.shaftEngine.elementActionLibrary.ElementActions;
import com.shaftEngine.ioActionLibrary.ExcelFileManager;
import com.shaftEngine.validationsLibrary.Assertions;
import com.shaftEngine.validationsLibrary.Verifications;

public class GoogleSearch {
	WebDriver driver;
	ExcelFileManager testDataReader = new ExcelFileManager(System.getProperty("testDataFilePath"));

	String url = testDataReader.getCellData("URL");
	By googleLogo_image = By.id("hplogo");
	By searchBox_textField = By.xpath("//input[@id='lst-ib' or @class='lst']");

	public GoogleSearch(WebDriver driver) {
		this.driver = driver;
	}

	public void navigateToURL() {
		BrowserActions.navigateToURL(driver, url);
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

	public void assertPageIsOpen() {
		Assertions.assertElementExists(driver, googleLogo_image, true);
	}

	public void verifyPageTitle(String expectedValue) {
		Verifications.verifyBrowserAttribute(driver, "Title", expectedValue, true);
		Verifications.verifyBrowserAttribute(driver, "Title", "Not Google", false);

	}

}
