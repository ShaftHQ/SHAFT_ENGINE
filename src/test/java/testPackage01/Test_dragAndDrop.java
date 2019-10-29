package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;

public class Test_dragAndDrop {
    WebDriver driver;

    @Test(priority = 1, description = "TC001 - Test Drag and Drop function.")
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

    @Test(priority = 2, description = "TC002 - Test Drag and Drop by offset function.")
    public void dragAndDropByOffset() {
	BrowserActions.navigateToURL(driver, "https://jqueryui.com/resources/demos/draggable/default.html");
	By dragTargetLocator = By.id("draggable");

	ElementActions.dragAndDropByOffset(driver, dragTargetLocator, 1000, 100);

    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
	driver = BrowserFactory.getBrowser();
    }
}