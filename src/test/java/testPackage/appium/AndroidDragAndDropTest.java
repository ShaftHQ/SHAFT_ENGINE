package testPackage.appium;

import io.appium.java_client.AppiumBy;
import org.openqa.selenium.By;
import org.testng.annotations.Test;

public class AndroidDragAndDropTest extends MobileTest {
    private static final By dragAndDropScreen = AppiumBy.accessibilityId("Drag-drop-screen");

    @Test
    public void wizard_scrollInExpandableLists_verticalScrolling_insideScreen() {
        By dragButton = By.xpath("//android.widget.Button[@content-desc='Drag']");
        By draggableRobotEyes = By.xpath("//android.view.ViewGroup[@content-desc='drag-c1']/android.widget.ImageView");
        By dropRobotEyes = By.xpath("//android.view.ViewGroup[@content-desc='drop-c1']/android.view.ViewGroup");

        // WebDriver code -> really fails to drag
//        driver.get().element().click(dragButton)
//                .dragAndDrop(draggableRobotEyes, dropRobotEyes)
//                .and().assertThat(dragAndDropScreen).matchesReferenceImage().perform();

        // Appium code -> drag and drop happens but shows up as failed
        driver.get().touch().tap(dragButton)
                .swipeToElement(draggableRobotEyes, dropRobotEyes)
                .and().assertThat(dragAndDropScreen).matchesReferenceImage().perform();
    }

    @Test
    public void multipleDragAndDrop() {
        driver.get().touch().tap(AppiumBy.accessibilityId("Drag"));
        driver.get().element()
                .dragAndDrop(AppiumBy.accessibilityId("drag-l2"), AppiumBy.accessibilityId("drop-l2"))
                .dragAndDrop(AppiumBy.accessibilityId("drag-r3"), AppiumBy.accessibilityId("drop-r3"))
                .dragAndDrop(AppiumBy.accessibilityId("drag-c3"), AppiumBy.accessibilityId("drop-c3"))
                .dragAndDrop(AppiumBy.accessibilityId("drag-r1"), AppiumBy.accessibilityId("drop-r1"))
                .and().assertThat(dragAndDropScreen).matchesReferenceImage().perform();
    }
}