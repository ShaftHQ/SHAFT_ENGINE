package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserActions;
import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;
import com.shaft.io.ReportManager;

public class Test_dragAndDrop {
	WebDriver driver;

	@Test(priority = 0, description = "TC001 - Test Drag and Drop function.")
	public void dragAndDrop() {
		BrowserActions.navigateToURL(driver, "http://the-internet.herokuapp.com/drag_and_drop"); // PASSED
		By dropDestinationLocator = By.xpath("//div[@id='columns']//*[contains (text(),'B')]");
		By dragTarget1Locator = By.xpath("//div[@id='columns']//*[contains (text(),'A')]");

		// BrowserActions.navigateToURL(driver, "http://rubaxa.github.io/Sortable/");
		// By dropDestinationLocator = By.xpath("//*[@id='bar']");
		// By dragTarget1Locator = By.xpath("//*[@id='foo']/li[1]");

		// BrowserActions.navigateToURL(driver,
		// "http://a5hik.github.io/ng-sortable/#/kanban");
		// http://jqueryui.com/resources/demos/sortable/connect-lists.html

		// ElementActions.click(driver, dragTarget1Locator);
		ElementActions.dragAndDrop(driver, dragTarget1Locator, dropDestinationLocator);

	}

	@BeforeClass // Set-up method, to be run once before the first test
	public void beforeClass() {
		driver = BrowserFactory.getBrowser("GoogleChrome");
	}

	@AfterClass(alwaysRun = true) // Tear-down method, to be run once after the last test
	public void afterClass() {
		BrowserFactory.closeAllDrivers();
	}

	@AfterMethod
	public void afterMethod() {
		ReportManager.getTestLog();
	}
}
