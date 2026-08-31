package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

public class DragAndDropTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    @Test(description = "TC001 - Test Drag and Drop function.")
    public void dragAndDrop() {
        driver.get().browser().navigateToURL(TestPageServer.url("dragAndDropFixture.html"));
        By dropDestinationLocator = By.id("drop-target");
        By dragTargetLocator = By.id("drag-source");

        ReportManager.log("Attempting Drag and Drop");
        driver.get().element().dragAndDrop(dragTargetLocator, dropDestinationLocator);

        Assert.assertEquals(driver.get().element().get().text(dropDestinationLocator), "Dropped");
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
        var actions = driver.get().element();
        String initialDroppableText = actions.get().text(By.id("droppable"));
        actions.dragAndDrop(By.id("draggable"), By.id("droppable"));
        String finalDroppableText = actions.get().text(By.id("droppable"));
        Validations.assertThat().object(finalDroppableText).doesNotEqual(initialDroppableText)
                .withCustomReportMessage("Checking to see if the text has changed after performing drag and drop")
                .perform();
    }

    @Test
    public void dragAndDropTouchEnabled() {
        driver.get().browser().navigateToURL("https://jqueryui.com/resources/demos/droppable/default.html");
        var actions = driver.get().element();
        String initialDroppableText = actions.get().text(By.id("droppable"));
        actions.touch().swipeToElement(By.id("draggable"), By.id("droppable"));
        String finalDroppableText = actions.get().text(By.id("droppable"));
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
        if (driver.get() != null) {
            driver.get().quit();
        }
        driver.remove();
    }
}
