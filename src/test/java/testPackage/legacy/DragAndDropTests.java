package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class DragAndDropTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    @Test(description = "TC001 - Test Drag and Drop function.")
    public void dragAndDrop() {
        driver.get().browser().navigateToURL("http://the-internet.herokuapp.com/drag_and_drop"); // PASSED
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
        driver.get().element().dragAndDrop(dragTarget1Locator, dropDestinationLocator);

    }

    @Test(description = "TC002 - Test Drag and Drop by offset function.")
    public void dragAndDropByOffset() {
        driver.get().browser().navigateToURL("https://jqueryui.com/resources/demos/draggable/default.html");
        By dragTargetLocator = By.id("draggable");
        driver.get().element().dragAndDropByOffset(dragTargetLocator, 100, 50);
    }

    @Test
    public void dragAndDropJquery() {
        driver.get().browser().navigateToURL("https://jqueryui.com/resources/demos/droppable/default.html");
        ElementActions actions = driver.get().element();
        String initialDroppableText = actions.getText(By.id("droppable"));
        actions.dragAndDrop(By.id("draggable"), By.id("droppable"));
        String finalDroppableText = actions.getText(By.id("droppable"));
        Validations.assertThat().object(finalDroppableText).doesNotEqual(initialDroppableText)
                .withCustomReportMessage("Checking to see if the text has changed after performing drag and drop")
                .perform();
    }

    @Test
    public void dragAndDropTouchEnabled() {
        driver.get().browser().navigateToURL("https://jqueryui.com/resources/demos/droppable/default.html");
        ElementActions actions = driver.get().element();
        String initialDroppableText = actions.getText(By.id("droppable"));
        actions.touch().swipeToElement(By.id("draggable"), By.id("droppable"));
        String finalDroppableText = actions.getText(By.id("droppable"));
        Validations.assertThat().object(finalDroppableText).doesNotEqual(initialDroppableText)
                .withCustomReportMessage("Checking to see if the text has changed after performing drag and drop")
                .perform();
    }

    @Test
    public void dragAndDropByOffsetTouchEnabled() {
        driver.get().browser().navigateToURL("https://jqueryui.com/resources/demos/draggable/default.html");
        By dragTargetLocator = By.id("draggable");
        driver.get().touch().swipeByOffset(dragTargetLocator, 100, 50);
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}