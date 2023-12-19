package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class DragAndDropTests {
    WebDriver driver;
    DriverFactoryHelper helper;

    @Test(description = "TC001 - Test Drag and Drop function.")
    public void dragAndDrop() {
        BrowserActions.getInstance(helper).navigateToURL("http://the-internet.herokuapp.com/drag_and_drop"); // PASSED
        By dropDestinationLocator = By.xpath("//div[@id='columns']//*[contains (text(),'B')]");
        By dragTarget1Locator = By.xpath("//div[@id='columns']//*[contains (text(),'A')]");

        // BrowserActions.navigateToURL(driver, "http://rubaxa.github.io/Sortable/");
        // By dropDestinationLocator = By.xpath("//*[@id='bar']");
        // By dragTarget1Locator = By.xpath("//*[@id='foo']/li[1]");

        // BrowserActions.navigateToURL(driver,
        // "http://a5hik.github.io/ng-sortable/#/kanban");
        // http://jqueryui.com/resources/demos/sortable/connect-lists.html

        // ElementActions.click(driver, dragTarget1Locator);
        ReportManager.log("Attempting Drag and Drop");
        ElementActions.getInstance(helper).dragAndDrop(dragTarget1Locator, dropDestinationLocator);

    }

    @Test(description = "TC002 - Test Drag and Drop by offset function.")
    public void dragAndDropByOffset() {
        BrowserActions.getInstance(helper).navigateToURL("https://jqueryui.com/resources/demos/draggable/default.html");
        By dragTargetLocator = By.id("draggable");

        ElementActions.getInstance(helper).dragAndDropByOffset(dragTargetLocator, 100, 50);
    }

    @Test
    public void dragAndDropJquery() {
        BrowserActions.getInstance(helper).navigateToURL("https://jqueryui.com/resources/demos/droppable/default.html");
        ElementActions actions = new ElementActions(driver);
        String initialDroppableText = actions.getText(By.id("droppable"));
        actions.dragAndDrop(By.id("draggable"), By.id("droppable"));
        String finalDroppableText = actions.getText(By.id("droppable"));
        Validations.assertThat().object(finalDroppableText).doesNotEqual(initialDroppableText)
                .withCustomReportMessage("Checking to see if the text has changed after performing drag and drop")
                .perform();
    }

    @Test
    public void dragAndDropTouchEnabled() {
        BrowserActions.getInstance(helper).navigateToURL("https://jqueryui.com/resources/demos/droppable/default.html");
        ElementActions actions = ElementActions.getInstance(helper);
        String initialDroppableText = actions.getText(By.id("droppable"));
        actions.touch().swipeToElement(By.id("draggable"), By.id("droppable"));
        String finalDroppableText = actions.getText(By.id("droppable"));
        Validations.assertThat().object(finalDroppableText).doesNotEqual(initialDroppableText)
                .withCustomReportMessage("Checking to see if the text has changed after performing drag and drop")
                .perform();
    }

    @Test
    public void dragAndDropByOffsetTouchEnabled() {
        BrowserActions.getInstance(helper).navigateToURL("https://jqueryui.com/resources/demos/draggable/default.html");
        By dragTargetLocator = By.id("draggable");
        TouchActions.getInstance(helper).swipeByOffset(dragTargetLocator, 100, 50);
    }

    @BeforeMethod
    public void beforeMethod() {
        helper = DriverFactory.getHelper();
        driver = helper.getDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        BrowserActions.getInstance(helper).closeCurrentWindow();
    }
}