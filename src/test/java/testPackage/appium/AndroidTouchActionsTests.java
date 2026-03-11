package testPackage.appium;

import com.google.common.collect.ImmutableMap;
import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.ScreenOrientation;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * E2E tests covering the full functionality of the {@link TouchActions} class for Android native apps.
 * Tests run against BrowserStack using ApiDemos-debug.apk and the Native Android Demo app.
 */
public class AndroidTouchActionsTests extends MobileTest {
    private static final String PACKAGE = "io.appium.android.apis";

    /**
     * Verifies that {@link TouchActions#doubleTap(By)} performs a double-tap gesture on a text
     * field. The text value must remain intact after the gesture (double-tap selects text but
     * does not clear it).
     */
    @Test(groups = {"ApiDemosDebug"})
    public void doubleTapOnTextFieldAndAssertTextValueIsPreserved() {
        ((AndroidDriver) driver.get().getDriver()).executeScript(
                "mobile: startActivity", ImmutableMap.of("intent", PACKAGE + "/.app.SearchInvoke"));
        driver.get().element().type(By.id("txt_query_prefill"), "double tap test");
        driver.get().touch().doubleTap(By.id("txt_query_prefill"));
        driver.get().assertThat()
                .element(By.id("txt_query_prefill"))
                .text()
                .contains("double tap test")
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#longTap(By)} performs a long-press gesture on a text
     * field. The text value must remain intact after the gesture (long-press selects text /
     * shows a context menu but does not clear the field).
     */
    @Test(groups = {"ApiDemosDebug"})
    public void longTapOnTextFieldAndAssertTextValueIsPreserved() {
        ((AndroidDriver) driver.get().getDriver()).executeScript(
                "mobile: startActivity", ImmutableMap.of("intent", PACKAGE + "/.app.SearchInvoke"));
        driver.get().element().type(By.id("txt_query_prefill"), "long tap test");
        driver.get().touch().longTap(By.id("txt_query_prefill"));
        driver.get().assertThat()
                .element(By.id("txt_query_prefill"))
                .text()
                .contains("long tap test")
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#activateAppFromBackground(String)} brings the app
     * back to the foreground after {@link TouchActions#sendAppToBackground(int)} suspends it.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void sendAppToBackgroundAndActivateFromBackground() {
        driver.get().touch()
                .sendAppToBackground(2)
                .activateAppFromBackground(PACKAGE);
        driver.get().assertThat()
                .element(AppiumBy.accessibilityId("App"))
                .exists()
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#swipeByOffset(By, int, int)} scrolls a list
     * by performing a drag-by-offset gesture, and the list container remains accessible.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void swipeListByOffsetAndAssertListIsStillVisible() {
        driver.get().touch()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("Expandable Lists")
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .tap(By.xpath("//android.widget.TextView[@text='Group 1']"))
                .swipeByOffset(By.className("android.widget.ExpandableListView"), 0, -300);
        driver.get().assertThat()
                .element(By.className("android.widget.ExpandableListView"))
                .exists()
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#swipeElementIntoView(String)} uses UIAutomator to
     * scroll a vertically scrollable list until an element with the given text is visible.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void swipeElementIntoViewByTextAndAssertTargetIsVisible() {
        driver.get().touch()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("Expandable Lists")
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .tap(By.xpath("//android.widget.TextView[@text='Group 1']"))
                .swipeElementIntoView("Group 18");
        driver.get().assertThat()
                .element(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .exists()
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#swipeElementIntoView(String, TouchActions.SwipeMovement)}
     * with {@link TouchActions.SwipeMovement#VERTICAL} uses UIAutomator to scroll a vertical
     * list until the target text element is visible.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void swipeElementIntoViewByTextWithVerticalMovementAndAssertTargetIsVisible() {
        driver.get().touch()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("Expandable Lists")
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .tap(By.xpath("//android.widget.TextView[@text='Group 1']"))
                .swipeElementIntoView("Group 18", TouchActions.SwipeMovement.VERTICAL);
        driver.get().assertThat()
                .element(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .exists()
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#swipeElementIntoView(String, TouchActions.SwipeMovement)}
     * with {@link TouchActions.SwipeMovement#HORIZONTAL} uses UIAutomator to scroll a horizontal
     * list until the target tab text is visible.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void swipeElementIntoViewByTextWithHorizontalMovementAndAssertTargetIsVisible() {
        driver.get().touch()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("Tabs")
                .tap(AppiumBy.accessibilityId("Tabs"))
                .swipeElementIntoView("5. Scrollable")
                .tap(AppiumBy.accessibilityId("5. Scrollable"))
                .swipeElementIntoView("TAB 12", TouchActions.SwipeMovement.HORIZONTAL);
        driver.get().assertThat()
                .element(By.xpath("//android.widget.TextView[@text='TAB 12']"))
                .exists()
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#rotate(ScreenOrientation)} rotates the screen to
     * landscape and back to portrait without errors, and the app remains usable.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void rotateScreenToLandscapeAndBackToPortraitAndAssertAppIsVisible() {
        driver.get().touch()
                .rotate(ScreenOrientation.LANDSCAPE)
                .rotate(ScreenOrientation.PORTRAIT);
        driver.get().assertThat()
                .element(AppiumBy.accessibilityId("App"))
                .exists()
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#pinchToZoom(TouchActions.ZoomDirection)} performs
     * zoom-in and zoom-out gestures on a WebView. The WebView must remain visible after both
     * gestures, confirming that the pinch gestures executed without crashing the session.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void pinchToZoomInAndOutOnWebViewAndAssertWebViewIsStillVisible() {
        driver.get().touch()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("WebView")
                .tap(AppiumBy.accessibilityId("WebView"))
                .pinchToZoom(TouchActions.ZoomDirection.IN)
                .pinchToZoom(TouchActions.ZoomDirection.OUT);
        driver.get().assertThat()
                .element(By.className("android.webkit.WebView"))
                .exists()
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#nativeKeyboardKeyPress(TouchActions.KeyboardKeys)} and
     * {@link TouchActions#hideNativeKeyboard()} interact with the soft keyboard correctly
     * without causing the activity to crash.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void hideNativeKeyboardAndPressKeyboardActionKeyWithoutErrors() {
        ((AndroidDriver) driver.get().getDriver()).executeScript(
                "mobile: startActivity", ImmutableMap.of("intent", PACKAGE + "/.app.SearchInvoke"));
        driver.get().element().type(By.id("txt_query_prefill"), "SHAFT keyboard test");
        driver.get().touch()
                .nativeKeyboardKeyPress(TouchActions.KeyboardKeys.DONE)
                .hideNativeKeyboard();
        driver.get().assertThat()
                .element(By.id("txt_query_prefill"))
                .exists()
                .perform();
    }

    /**
     * Verifies that {@link TouchActions#pushFile(String, String)} and
     * {@link TouchActions#pullFile(String, String)} round-trip a file to/from the
     * device with content integrity preserved.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void pushFileWithStringPathAndPullBackAndAssertContentMatches() throws IOException {
        String testContent = "SHAFT Engine push/pull file test content";
        String deviceFilePath = "/sdcard/Download/shaft_test.txt";
        String localDownloadPath = "target/shaft_pulled.txt";
        Path tempFile = Files.createTempFile("shaft_push_", ".txt");
        Files.writeString(tempFile, testContent);
        try {
            driver.get().touch()
                    .pushFile(deviceFilePath, tempFile.toString())
                    .pullFile(deviceFilePath, localDownloadPath);
            File downloadedFile = new File(localDownloadPath);
            SHAFT.Validations.assertThat()
                    .object(downloadedFile.exists())
                    .isTrue()
                    .withCustomReportMessage("Pulled file should exist at: " + localDownloadPath)
                    .perform();
            SHAFT.Validations.assertThat()
                    .object(Files.readString(downloadedFile.toPath()))
                    .isEqualTo(testContent)
                    .withCustomReportMessage("Pulled file content should match the pushed content")
                    .perform();
        } finally {
            Files.deleteIfExists(tempFile);
            Files.deleteIfExists(Path.of(localDownloadPath));
        }
    }

    /**
     * Verifies that {@link TouchActions#pushFile(String, File)} uploads a {@link File}
     * object to the device. The file is then pulled back and its content is verified to
     * confirm the upload succeeded.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void pushFileWithFileObjectAndPullBackAndAssertContentMatches() throws IOException {
        String testContent = "SHAFT Engine File-object push test";
        String deviceFilePath = "/sdcard/Download/shaft_file_obj.txt";
        String localDownloadPath = "target/shaft_file_obj_pulled.txt";
        Path tempFile = Files.createTempFile("shaft_push_file_", ".txt");
        Files.writeString(tempFile, testContent);
        try {
            driver.get().touch()
                    .pushFile(deviceFilePath, tempFile.toFile())
                    .pullFile(deviceFilePath, localDownloadPath);
            File downloadedFile = new File(localDownloadPath);
            SHAFT.Validations.assertThat()
                    .object(downloadedFile.exists())
                    .isTrue()
                    .withCustomReportMessage("Pulled file should exist at: " + localDownloadPath)
                    .perform();
            SHAFT.Validations.assertThat()
                    .object(Files.readString(downloadedFile.toPath()))
                    .isEqualTo(testContent)
                    .withCustomReportMessage("Pulled file content should match the pushed File object content")
                    .perform();
        } finally {
            Files.deleteIfExists(tempFile);
            Files.deleteIfExists(Path.of(localDownloadPath));
        }
    }

    /**
     * Verifies that {@link TouchActions#swipeToElement(By, By)} performs a drag-and-drop
     * gesture from a source element to a destination element on the Native Android Demo app.
     */
    @Test(groups = {"NativeAndroidDemo"})
    public void swipeToElementUsingDragAndDropGestureAndAssertDropTargetIsVisible() {
        driver.get().touch()
                .tap(AppiumBy.accessibilityId("Drag"))
                .swipeToElement(AppiumBy.accessibilityId("drag-l2"), AppiumBy.accessibilityId("drop-l2"));
        driver.get().assertThat()
                .element(AppiumBy.accessibilityId("drop-l2"))
                .exists()
                .perform();
    }
}
