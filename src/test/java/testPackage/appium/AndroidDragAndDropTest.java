package testPackage.appium;

import io.appium.java_client.AppiumBy;
import org.openqa.selenium.By;
import org.testng.annotations.Test;

public class AndroidDragAndDropTest extends MobileTest {
    private static final By dragAndDropScreen = AppiumBy.accessibilityId("Drag-drop-screen");

    @Test(groups = {"NativeAndroidDemo"})
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