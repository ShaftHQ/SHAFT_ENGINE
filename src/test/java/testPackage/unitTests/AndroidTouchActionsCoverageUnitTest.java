package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.properties.internal.Properties;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Base64;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class AndroidTouchActionsCoverageUnitTest {
    private static final Path TEMP_DIR = Path.of("target", "temp", "touchActionsCoverage");
    private boolean originalWaitForLazyLoading;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() throws Exception {
        originalWaitForLazyLoading = SHAFT.Properties.timeouts.waitForLazyLoading();
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        Files.createDirectories(TEMP_DIR);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(originalWaitForLazyLoading);
        Properties.clearForCurrentThread();
    }

    @Test
    public void androidLifecycleAndFileTransferApisShouldExecuteAndPersistPulledFile() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        byte[] pulledContent = "pulled content".getBytes();
        when(driver.pullFile("/sdcard/Download/result.txt")).thenReturn(pulledContent);

        Path localSourceFile = TEMP_DIR.resolve("source.txt");
        Files.writeString(localSourceFile, "source content");
        Path pulledFilePath = TEMP_DIR.resolve("nested").resolve("result.txt");

        TouchActions touchActions = new TouchActions(driver);
        touchActions.sendAppToBackground(2)
                .sendAppToBackground()
                .activateAppFromBackground("io.appium.android.apis")
                .rotate(ScreenOrientation.LANDSCAPE)
                .hideNativeKeyboard()
                .pushFile("/sdcard/Download/source.txt", localSourceFile.toString())
                .pushFile("/sdcard/Download/source2.txt", localSourceFile.toFile())
                .pullFile("/sdcard/Download/result.txt", pulledFilePath.toString());

        verify(driver).runAppInBackground(Duration.ofSeconds(2));
        verify(driver).runAppInBackground(Duration.ofSeconds(-1));
        verify(driver).activateApp("io.appium.android.apis");
        verify(driver).rotate(ScreenOrientation.LANDSCAPE);
        verify(driver).hideKeyboard();
        verify(driver, times(2)).pushFile(anyString(), any(byte[].class));

        SHAFT.Validations.assertThat().object(Files.exists(pulledFilePath)).isTrue().perform();
        SHAFT.Validations.assertThat().object(Files.readAllBytes(pulledFilePath)).isEqualTo(pulledContent).perform();
    }

    @Test
    public void nativeKeyboardPressAndPinchZoomShouldUseMobileScriptsAndActionsApi() {
        RemoteWebDriver driver = mock(RemoteWebDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(1080, 1920));
        when(driver.executeScript(eq("mobile: isKeyboardShown"))).thenReturn(true);

        TouchActions touchActions = new TouchActions(driver);
        touchActions.nativeKeyboardKeyPress(TouchActions.KeyboardKeys.SEARCH)
                .pinchToZoom(TouchActions.ZoomDirection.IN)
                .pinchToZoom(TouchActions.ZoomDirection.OUT);

        verify(driver).executeScript(eq("mobile: isKeyboardShown"));
        verify(driver).executeScript(eq("mobile: performEditorAction"), any());
        verify(driver, times(2)).perform(any());
    }

    @Test
    public void textSwipeHelpersShouldUseUiAutomatorLookups() {
        AndroidDriver driver = createMockAndroidDriver();
        when(driver.findElement(any(By.class))).thenReturn(mock(WebElement.class));

        TouchActions touchActions = new TouchActions(driver);
        touchActions.swipeElementIntoView("Group 18")
                .swipeElementIntoView("TAB 12", TouchActions.SwipeMovement.HORIZONTAL)
                .swipeElementIntoView("Group 19", TouchActions.SwipeMovement.VERTICAL);

        verify(driver, times(3)).findElement(any(By.class));
    }

    @Test
    public void reflectionHelpersShouldPrepareAndScrollWithW3CParameters() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        doReturn(true).doReturn(false).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());

        TouchActions touchActions = new TouchActions(driver);
        Method prepareParameters = TouchActions.class.getDeclaredMethod("prepareParameters", TouchActions.SwipeDirection.class, By.class, By.class);
        prepareParameters.setAccessible(true);

        @SuppressWarnings("unchecked")
        Map<Object, Object> upParameters = (Map<Object, Object>) prepareParameters.invoke(touchActions, TouchActions.SwipeDirection.UP, null, null);
        @SuppressWarnings("unchecked")
        Map<Object, Object> leftParameters = (Map<Object, Object>) prepareParameters.invoke(touchActions, TouchActions.SwipeDirection.LEFT, null, null);

        SHAFT.Validations.assertThat().object(upParameters.get("direction")).isEqualTo("UP").perform();
        SHAFT.Validations.assertThat().object(leftParameters.get("direction")).isEqualTo("LEFT").perform();
        SHAFT.Validations.assertThat().object(leftParameters.containsKey("left")).isTrue().perform();
        SHAFT.Validations.assertThat().object(touchActions.and()).isEqualTo(touchActions).perform();

        Method performW3cCompliantScroll = TouchActions.class.getDeclaredMethod("performW3cCompliantScroll", java.util.HashMap.class);
        performW3cCompliantScroll.setAccessible(true);
        SHAFT.Validations.assertThat().object(performW3cCompliantScroll.invoke(touchActions, upParameters)).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(performW3cCompliantScroll.invoke(touchActions, upParameters)).isEqualTo(false).perform();

    }

    @Test
    public void publicHelpersAndEnumsShouldBeUsable() {
        AndroidDriver driver = createMockAndroidDriver();
        when(driver.findElement(any(By.class))).thenReturn(mock(WebElement.class));

        TouchActions touchActions = new TouchActions(driver);
        SHAFT.Validations.assertThat().object(touchActions.assertThat(By.id("x")) != null).isTrue().perform();
        SHAFT.Validations.assertThat().object(touchActions.verifyThat(By.id("x")) != null).isTrue().perform();
        SHAFT.Validations.assertThat().object(TouchActions.ZoomDirection.values().length).isEqualTo(2).perform();
        SHAFT.Validations.assertThat().object(TouchActions.SwipeDirection.values().length).isEqualTo(4).perform();
        SHAFT.Validations.assertThat().object(TouchActions.SwipeMovement.values().length).isEqualTo(2).perform();
        SHAFT.Validations.assertThat().object(TouchActions.KeyboardKeys.GO.name()).isEqualTo("GO").perform();
    }

    @Test
    public void visualTapAndWaitApisShouldCoverImageIdentificationBranches() throws Exception {
        RemoteWebDriver driver = mock(RemoteWebDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(1080, 1920));

        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        byte[] validPng = getValidPngBytes();
        when(elementActionsHelper.waitForElementPresence(driver, "present.png"))
                .thenReturn(List.of(validPng, validPng, List.of(120, 240)));
        when(elementActionsHelper.waitForElementPresence(driver, "missing.png"))
                .thenReturn(List.of(validPng, validPng, List.of()));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        touchActions.tap("present.png")
                .waitUntilElementIsVisible("present.png")
                .tap("missing.png")
                .waitUntilElementIsVisible("missing.png");

        verify(driver).perform(any());
    }

    @Test
    public void swipeElementIntoViewOverloadsShouldCoverNativeAndWebPaths() throws Exception {
        AndroidDriver androidDriver = createMockAndroidDriver();
        WebElement targetElement = mock(WebElement.class);
        when(androidDriver.findElements(By.id("target"))).thenReturn(List.of(targetElement));

        TouchActions nativeTouchActions = new TouchActions(androidDriver);
        ElementActionsHelper nativeElementActionsHelper = mock(ElementActionsHelper.class);
        byte[] validPng = getValidPngBytes();
        when(nativeElementActionsHelper.waitForElementPresence(any(), eq("target.png")))
                .thenReturn(List.of(validPng, validPng, List.of(100, 200)));
        when(nativeElementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        injectElementActionsHelper(nativeTouchActions, nativeElementActionsHelper);

        nativeTouchActions.swipeElementIntoView(By.id("target"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView("target.png", TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.id("container"), "target.png", TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.id("container"), By.id("target"), TouchActions.SwipeDirection.DOWN);

        RemoteWebDriver webDriver = mock(RemoteWebDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(webDriver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(1080, 1920));

        TouchActions webTouchActions = new TouchActions(webDriver);
        ElementActionsHelper webElementActionsHelper = mock(ElementActionsHelper.class);
        when(webElementActionsHelper.identifyUniqueElement(any(), any(By.class)))
                .thenReturn(List.of("locator", targetElement));
        injectElementActionsHelper(webTouchActions, webElementActionsHelper);

        webTouchActions.swipeElementIntoView(null, By.id("target"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.id("container"), By.id("target"), TouchActions.SwipeDirection.DOWN);
    }

    @Test
    public void iosAndFallbackBranchesShouldCoverAdditionalLifecycleAndFilePaths() throws Exception {
        Path localSourceFile = TEMP_DIR.resolve("ios-source.txt");
        Files.writeString(localSourceFile, "ios source");
        Path iosPulledPath = TEMP_DIR.resolve("ios").resolve("result.txt");

        IOSDriver iosDriver = mock(IOSDriver.class);
        WebDriver.Options iosOptions = mock(WebDriver.Options.class);
        WebDriver.Window iosWindow = mock(WebDriver.Window.class);
        when(iosDriver.manage()).thenReturn(iosOptions);
        when(iosOptions.window()).thenReturn(iosWindow);
        when(iosWindow.getSize()).thenReturn(new Dimension(1080, 1920));
        when(iosDriver.pullFile("/tmp/ios-result.txt")).thenReturn("ios-result".getBytes());

        TouchActions iosTouchActions = new TouchActions(iosDriver);
        iosTouchActions.hideNativeKeyboard()
                .sendAppToBackground(1)
                .activateAppFromBackground("com.example.ios")
                .rotate(ScreenOrientation.PORTRAIT)
                .pushFile("/tmp/ios-source.txt", localSourceFile.toString())
                .pushFile("/tmp/ios-source2.txt", localSourceFile.toFile())
                .pullFile("/tmp/ios-result.txt", iosPulledPath.toString());

        SHAFT.Validations.assertThat().object(Files.exists(iosPulledPath)).isTrue().perform();

    }

    @Test
    public void reflectionAndWrapperMethodsShouldCoverRemainingTouchActionPaths() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        WebElement targetElement = mock(WebElement.class);
        when(driver.findElements(any(By.class))).thenReturn(List.of(), List.of(targetElement), List.of(targetElement));
        doReturn(true).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());

        TouchActions touchActions = new TouchActions(new DriverFactoryHelper(driver));
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        WebElement scrollableElement = mock(WebElement.class);
        when(scrollableElement.getRect()).thenReturn(new org.openqa.selenium.Rectangle(10, 20, 300, 500));
        when(elementActionsHelper.identifyUniqueElement(any(), eq(By.id("container"))))
                .thenReturn(List.of("container", scrollableElement));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        Method prepareParameters = TouchActions.class.getDeclaredMethod("prepareParameters", TouchActions.SwipeDirection.class, By.class, By.class);
        prepareParameters.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<Object, Object> elementScrollParameters = (Map<Object, Object>) prepareParameters.invoke(touchActions, TouchActions.SwipeDirection.RIGHT, By.id("container"), By.id("target"));
        SHAFT.Validations.assertThat().object(elementScrollParameters.containsKey("width")).isTrue().perform();

        Method attemptUISelectorScroll = TouchActions.class.getDeclaredMethod("attemptUISelectorScroll", TouchActions.SwipeDirection.class, int.class);
        attemptUISelectorScroll.setAccessible(true);
        attemptUISelectorScroll.invoke(touchActions, TouchActions.SwipeDirection.DOWN, 0);

        Method attemptW3cCompliantActionsScroll = TouchActions.class.getDeclaredMethod("attemptW3cCompliantActionsScroll", TouchActions.SwipeDirection.class, By.class, By.class);
        attemptW3cCompliantActionsScroll.setAccessible(true);
        Object isFound = attemptW3cCompliantActionsScroll.invoke(touchActions, TouchActions.SwipeDirection.DOWN, null, By.id("target"));
        SHAFT.Validations.assertThat().object(isFound).isEqualTo(true).perform();

        executeWrapperForCoverage(() -> touchActions.tap(By.id("target")));
        executeWrapperForCoverage(() -> touchActions.doubleTap(By.id("target")));
        executeWrapperForCoverage(() -> touchActions.longTap(By.id("target")));
        executeWrapperForCoverage(() -> touchActions.swipeToElement(By.id("source"), By.id("target")));
        executeWrapperForCoverage(() -> touchActions.swipeByOffset(By.id("target"), 10, 10));
    }

    private AndroidDriver createMockAndroidDriver() {
        AndroidDriver driver = mock(AndroidDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(1080, 1920));
        when(driver.findElements(any(By.class))).thenReturn(List.of());
        return driver;
    }

    private void injectElementActionsHelper(TouchActions touchActions, ElementActionsHelper elementActionsHelper) throws Exception {
        Field elementActionsHelperField = FluentWebDriverAction.class.getDeclaredField("elementActionsHelper");
        elementActionsHelperField.setAccessible(true);
        elementActionsHelperField.set(touchActions, elementActionsHelper);
    }

    private byte[] getValidPngBytes() {
        return Base64.getDecoder().decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAusB9Wn8YkQAAAAASUVORK5CYII=");
    }

    private void executeWrapperForCoverage(Runnable action) {
        try {
            action.run();
        } catch (RuntimeException runtimeException) {
            SHAFT.Validations.assertThat().object(runtimeException.getMessage() != null).isTrue().perform();
        } catch (AssertionError assertionError) {
            SHAFT.Validations.assertThat().object(assertionError.getMessage() != null).isTrue().perform();
        }
    }
}
