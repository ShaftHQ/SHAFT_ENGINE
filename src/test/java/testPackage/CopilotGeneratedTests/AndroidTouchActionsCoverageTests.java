package testPackage.CopilotGeneratedTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.By;
import org.testng.annotations.Test;
import testPackage.appium.MobileTest;

public class AndroidTouchActionsCoverageTests extends MobileTest {
    private static final String PACKAGE = "io.appium.android.apis";

    @Test(groups = {"ApiDemosDebug"})
    public void testAppBackgroundAndActivate() {
        driver.get().element().performTouchAction().sendAppToBackground(5) // Send to background for 5 seconds
                .activateAppFromBackground(PACKAGE)
                .and().assertThat(AppiumBy.accessibilityId("Views")).exists();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testTapAndDoubleTapActions() {
        driver.get().element().performTouchAction().swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Buttons"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Buttons"))
                .doubleTap(AppiumBy.accessibilityId("Toggle"))
                .and().assertThat(AppiumBy.accessibilityId("Toggle")).exists();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testPinchToZoom() {
        // Navigate to photo view for zoom testing
        driver.get().element().performTouchAction().swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Gallery"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Gallery"))
                .tap(AppiumBy.accessibilityId("1. Photos"))
                .pinchToZoom(TouchActions.ZoomDirection.IN)
                .pinchToZoom(TouchActions.ZoomDirection.OUT);
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testSwipeElementIntoViewWithScrollableContainer() {
        driver.get().element().performTouchAction().swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Lists"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Lists"))
                .swipeElementIntoView(
                        By.id("android:id/list"),  // Scrollable container
                        By.xpath("//android.widget.TextView[@text='17. Activate items']"),
                        TouchActions.SwipeDirection.DOWN
                )
                .and().assertThat(By.xpath("//android.widget.TextView[@text='17. Activate items']")).exists();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testSwipeByOffset() {
        driver.get().element().performTouchAction().swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Gallery"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Gallery"))
                .tap(AppiumBy.accessibilityId("1. Photos"))
                .swipeByOffset(By.className("android.widget.Gallery"), 200, 0)  // Swipe right
                .swipeByOffset(By.className("android.widget.Gallery"), -200, 0); // Swipe left
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testSwipeToElement() {
        driver.get().element().performTouchAction().swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Drag and Drop"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Drag and Drop"))
                .swipeToElement(
                        By.id("io.appium.android.apis:id/drag_dot_1"),
                        By.id("io.appium.android.apis:id/drag_dot_2")
                )
                .and().assertThat(By.id("io.appium.android.apis:id/drag_result_text")).exists();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testVisualElementIdentification() {
        String referenceImageFile = "content.png";
        if (SHAFT.Properties.platform.executionAddress().toLowerCase().contains("browserstack")) {
            referenceImageFile = "content_local.png";
        }
        String referenceImagePath = "src/main/resources/dynamicObjectRepository/Android/" + referenceImageFile;
        driver.get().element().performTouchAction().swipeElementIntoView(referenceImagePath, TouchActions.SwipeDirection.DOWN)
                .waitUntilElementIsVisible(referenceImagePath)
                .tap(referenceImagePath)
                .and().assertThat(AppiumBy.accessibilityId("Assets")).exists();
    }
}
