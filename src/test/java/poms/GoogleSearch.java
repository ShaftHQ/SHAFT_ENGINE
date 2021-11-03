package poms;

import com.microsoft.playwright.Page;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionType;
import com.shaft.validation.Validations;
import com.shaft.validation.Verifications;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class GoogleSearch {
    WebDriver driver = null;
    Page page = null;
    // ExcelFileManager testDataReader = new
    // ExcelFileManager(System.getProperty("testDataFilePath"));

     String url = "https://www.google.com/ncr";
     String urlAfterRedirection = "https://www.google.com";
     
    public static By googleLogo_image = By.xpath("//*[@id='hplogo' or @alt='Google']");
    String googleLogo_image_stringLocator = "xpath=//*[@id='hplogo' or @alt='Google']";
    
    By searchBox_textField = By.xpath("//input[@id='lst-ib' or @class='lst' or @name='q']");
    String searchBox_textField_stringLocator = "xpath=//input[@id='lst-ib' or @class='lst' or @name='q']";

    public GoogleSearch(WebDriver driver) {
        this.driver = driver;
    }
    public GoogleSearch(Page page) {
        this.page = page;
    }

    public void navigateToURL() {
    	if(driver != null) {
        BrowserActions.navigateToURL(driver, url, urlAfterRedirection);
    	}else {
    		BrowserActions.performBrowserAction(page).navigateToURL(url, urlAfterRedirection);
    	}
    }

    public void searchForQuery(String searchQuery) {
    	if(driver != null) {
        ElementActions.type(driver, searchBox_textField, searchQuery);
        ElementActions.keyPress(driver, searchBox_textField, "Enter");
    	}else {
    		ElementActions.performElementAction(page)
    		.type(searchBox_textField_stringLocator, searchQuery)
    		.keyPress(searchBox_textField_stringLocator, "Enter");
    	}
    }

    public void typeQuery(String searchQuery) {
    	if(driver != null) {
        ElementActions.type(driver, searchBox_textField, searchQuery);
    	}
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
    	if(driver != null) {
            Validations.assertThat()
                    .element(driver, googleLogo_image)
                    .exists()
                    .perform();
    	}else {
            Assertions.assertElementExists(page, googleLogo_image_stringLocator, AssertionType.POSITIVE);
    	}
    }

    public void verifyPageTitle(String expectedValue) {
    	if(driver != null) {
        Verifications.verifyBrowserAttribute(driver, "Title", expectedValue, Verifications.VerificationComparisonType.EQUALS, Verifications.VerificationType.POSITIVE);
        Verifications.verifyBrowserAttribute(driver, "Title", "Not Google", Verifications.VerificationComparisonType.EQUALS, Verifications.VerificationType.NEGATIVE);
    	}else {
    		
    	}

    }

}
