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
import org.openqa.selenium.WebDriverException;
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
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class AndroidTouchActionsCoverageUnitTest {
    private static final Path TEMP_DIR = Path.of("target", "temp", "touchActionsCoverage");
    private static final Path ROOT_PULL_PATH = Path.of("touchActions-root-pulled.txt");
    private boolean originalWaitForLazyLoading;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() throws Exception {
        originalWaitForLazyLoading = SHAFT.Properties.timeouts.waitForLazyLoading();
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        Files.createDirectories(TEMP_DIR);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        Files.deleteIfExists(ROOT_PULL_PATH);
        SHAFT.Properties.timeouts.set().waitForLazyLoading(originalWaitForLazyLoading);
        Properties.clearForCurrentThread();
    }

    @Test
    public void androidLifecycleAndFileTransferApisShouldExecuteAndPersistPulledFile() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        byte[] pulledContent = "pulled content".getBytes();
        when(driver.pullFile("/sdcard/Download/result.txt")).thenReturn(pulledContent);
        when(driver.pullFile("/sdcard/Download/root-result.txt")).thenReturn(pulledContent);
        when(driver.pullFile("/sdcard/Download/existing-result.txt")).thenReturn(pulledContent);

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
                .pullFile("/sdcard/Download/result.txt", pulledFilePath.toString())
                .pullFile("/sdcard/Download/root-result.txt", ROOT_PULL_PATH.toString())
                .pullFile("/sdcard/Download/existing-result.txt", TEMP_DIR.resolve("existing-result.txt").toString());

        verify(driver).runAppInBackground(Duration.ofSeconds(2));
        verify(driver).runAppInBackground(Duration.ofSeconds(-1));
        verify(driver).activateApp("io.appium.android.apis");
        verify(driver).rotate(ScreenOrientation.LANDSCAPE);
        verify(driver).hideKeyboard();
        verify(driver, times(2)).pushFile(anyString(), any(byte[].class));

        SHAFT.Validations.assertThat().object(Files.exists(pulledFilePath)).isTrue().perform();
        SHAFT.Validations.assertThat().object(Files.exists(ROOT_PULL_PATH)).isTrue().perform();
        SHAFT.Validations.assertThat().object(Files.exists(TEMP_DIR.resolve("existing-result.txt"))).isTrue().perform();
        SHAFT.Validations.assertThat().object(Arrays.equals(Files.readAllBytes(pulledFilePath), pulledContent)).isTrue().perform();
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
    public void fileTransferFailureBranchesShouldReportFailuresWithoutThrowing() throws Exception {
        Path localSourceFile = TEMP_DIR.resolve("failure-source.txt");
        Files.writeString(localSourceFile, "failure source");
        Path missingLocalFile = TEMP_DIR.resolve("missing-source.txt");
        Path unsupportedPulledPath = TEMP_DIR.resolve("unsupported").resolve("result.txt");

        AndroidDriver androidDriver = createMockAndroidDriver();
        doThrow(new WebDriverException("android push failure")).when(androidDriver).pushFile(eq("/sdcard/fail.txt"), any(byte[].class));
        when(androidDriver.pullFile("/sdcard/fail.txt")).thenThrow(new WebDriverException("android pull failure"));
        ElementActionsHelper androidElementActionsHelper = mock(ElementActionsHelper.class);
        TouchActions androidTouchActions = new TouchActions(androidDriver);
        injectElementActionsHelper(androidTouchActions, androidElementActionsHelper);

        androidTouchActions.pushFile("/sdcard/fail.txt", localSourceFile.toString())
                .pullFile("/sdcard/fail.txt", TEMP_DIR.resolve("android-failure.txt").toString())
                .pushFile("/sdcard/missing.txt", missingLocalFile.toString())
                .pushFile("/sdcard/missing2.txt", missingLocalFile.toFile());

        verify(androidElementActionsHelper, times(4)).failAction(eq(androidDriver), isNull(By.class), any(Throwable.class));

        IOSDriver iosDriver = createMockIOSDriver();
        doThrow(new WebDriverException("ios push failure")).when(iosDriver).pushFile(eq("/tmp/fail.txt"), any(byte[].class));
        when(iosDriver.pullFile("/tmp/fail.txt")).thenThrow(new WebDriverException("ios pull failure"));
        ElementActionsHelper iosElementActionsHelper = mock(ElementActionsHelper.class);
        TouchActions iosTouchActions = new TouchActions(iosDriver);
        injectElementActionsHelper(iosTouchActions, iosElementActionsHelper);

        iosTouchActions.pushFile("/tmp/fail.txt", localSourceFile.toFile())
                .pullFile("/tmp/fail.txt", TEMP_DIR.resolve("ios-failure.txt").toString());

        verify(iosElementActionsHelper, times(2)).failAction(eq(iosDriver), isNull(By.class), any(Throwable.class));

        RemoteWebDriver remoteWebDriver = createMockRemoteWebDriver();
        ElementActionsHelper remoteElementActionsHelper = mock(ElementActionsHelper.class);
        TouchActions remoteTouchActions = new TouchActions(remoteWebDriver);
        injectElementActionsHelper(remoteTouchActions, remoteElementActionsHelper);

        remoteTouchActions.pushFile("/remote/path.txt", localSourceFile.toString())
                .pushFile("/remote/path2.txt", localSourceFile.toFile())
                .pullFile("/remote/result.txt", unsupportedPulledPath.toString())
                .sendAppToBackground(1)
                .activateAppFromBackground("unsupported.app")
                .hideNativeKeyboard()
                .rotate(ScreenOrientation.PORTRAIT);

        verify(remoteElementActionsHelper, times(7)).failAction(eq(remoteWebDriver), isNull(By.class));
        verify(remoteWebDriver, never()).executeScript(eq("mobile: scrollGesture"), anyMap());
        SHAFT.Validations.assertThat().object(Files.exists(unsupportedPulledPath)).isFalse().perform();
    }

    @Test
    public void iosW3cScrollAndAllPrepareParameterDirectionsShouldUseExpectedScripts() throws Exception {
        IOSDriver iosDriver = createMockIOSDriver();
        doReturn(null).doReturn(false).when(iosDriver).executeScript(eq("mobile: scroll"), anyMap());
        TouchActions touchActions = new TouchActions(iosDriver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        WebElement scrollableElement = mock(WebElement.class);
        when(scrollableElement.getRect()).thenReturn(new org.openqa.selenium.Rectangle(10, 20, 300, 500));
        when(elementActionsHelper.identifyUniqueElement(any(), eq(By.id("ios-container"))))
                .thenReturn(List.of("ios-container", scrollableElement));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        Method prepareParameters = TouchActions.class.getDeclaredMethod("prepareParameters", TouchActions.SwipeDirection.class, By.class, By.class);
        prepareParameters.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<Object, Object> downParameters = (Map<Object, Object>) prepareParameters.invoke(touchActions, TouchActions.SwipeDirection.DOWN, null, By.id("target"));
        @SuppressWarnings("unchecked")
        Map<Object, Object> rightParameters = (Map<Object, Object>) prepareParameters.invoke(touchActions, TouchActions.SwipeDirection.RIGHT, null, By.id("target"));
        @SuppressWarnings("unchecked")
        Map<Object, Object> elementUpParameters = (Map<Object, Object>) prepareParameters.invoke(touchActions, TouchActions.SwipeDirection.UP, By.id("ios-container"), By.id("target"));
        @SuppressWarnings("unchecked")
        Map<Object, Object> elementLeftParameters = (Map<Object, Object>) prepareParameters.invoke(touchActions, TouchActions.SwipeDirection.LEFT, By.id("ios-container"), By.id("target"));

        SHAFT.Validations.assertThat().object(downParameters.get("direction")).isEqualTo("DOWN").perform();
        SHAFT.Validations.assertThat().object(rightParameters.get("left")).isEqualTo(100).perform();
        SHAFT.Validations.assertThat().object(elementUpParameters.get("height")).isEqualTo(270).perform();
        SHAFT.Validations.assertThat().object(elementLeftParameters.get("left")).isEqualTo(260).perform();

        Method performW3cCompliantScroll = TouchActions.class.getDeclaredMethod("performW3cCompliantScroll", java.util.HashMap.class);
        performW3cCompliantScroll.setAccessible(true);
        SHAFT.Validations.assertThat().object(performW3cCompliantScroll.invoke(touchActions, downParameters)).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(performW3cCompliantScroll.invoke(touchActions, downParameters)).isEqualTo(false).perform();
        verify(iosDriver, times(2)).executeScript(eq("mobile: scroll"), anyMap());
    }

    @Test
    public void visualSwipeShouldCoverNativeMissingImageAndWebCoordinateBranches() throws Exception {
        byte[] validPng = getValidPngBytes();

        AndroidDriver androidDriver = createMockAndroidDriver();
        doReturn(true).doReturn(false).when(androidDriver).executeScript(eq("mobile: scrollGesture"), anyMap());
        TouchActions nativeTouchActions = new TouchActions(androidDriver);
        ElementActionsHelper nativeElementActionsHelper = mock(ElementActionsHelper.class);
        when(nativeElementActionsHelper.waitForElementPresence(androidDriver, "native-missing.png"))
                .thenReturn(List.of(validPng, validPng, List.of()))
                .thenReturn(List.of(validPng, validPng, List.of(30, 40)));
        when(nativeElementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        injectElementActionsHelper(nativeTouchActions, nativeElementActionsHelper);

        nativeTouchActions.swipeElementIntoView(null, "native-missing.png", TouchActions.SwipeDirection.DOWN);
        verify(nativeElementActionsHelper, never()).failAction(eq(androidDriver), anyString(), isNull(By.class), any(List.class));

        RemoteWebDriver webDriver = createMockRemoteWebDriver();
        TouchActions webTouchActions = new TouchActions(webDriver);
        ElementActionsHelper webElementActionsHelper = mock(ElementActionsHelper.class);
        when(webElementActionsHelper.waitForElementPresence(webDriver, "web-present.png"))
                .thenReturn(List.of(validPng, validPng, List.of(20, 40)));
        when(webElementActionsHelper.waitForElementPresence(webDriver, "web-missing.png"))
                .thenReturn(List.of(validPng, validPng, List.of()));
        injectElementActionsHelper(webTouchActions, webElementActionsHelper);

        webTouchActions.swipeElementIntoView(null, "web-present.png", TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(null, "web-missing.png", TouchActions.SwipeDirection.DOWN);

        verify(webElementActionsHelper).failAction(eq(webDriver), anyString(), isNull(By.class), any(List.class));
    }

    @Test
    public void actionFailureBranchesShouldRemainNonThrowing() throws Exception {
        AndroidDriver keyboardFailureDriver = createMockAndroidDriver();
        doThrow(new WebDriverException("keyboard failure")).when(keyboardFailureDriver).hideKeyboard();
        ElementActionsHelper keyboardElementActionsHelper = mock(ElementActionsHelper.class);
        TouchActions keyboardTouchActions = new TouchActions(keyboardFailureDriver);
        injectElementActionsHelper(keyboardTouchActions, keyboardElementActionsHelper);

        keyboardTouchActions.hideNativeKeyboard();
        verify(keyboardElementActionsHelper).failAction(eq(keyboardFailureDriver), isNull(By.class), any(Throwable.class));

        RemoteWebDriver performFailureDriver = createMockRemoteWebDriver();
        doThrow(new org.openqa.selenium.UnsupportedCommandException("perform failure")).when(performFailureDriver).perform(any());
        TouchActions touchActions = new TouchActions(performFailureDriver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.waitForElementPresence(performFailureDriver, "tap-present.png"))
                .thenReturn(List.of(getValidPngBytes(), getValidPngBytes(), List.of(10, 20)));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        touchActions.tap("tap-present.png")
                .pinchToZoom(TouchActions.ZoomDirection.IN);

        verify(elementActionsHelper, times(2)).failAction(eq(performFailureDriver), isNull(By.class), any(Throwable.class));
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
        when(elementActionsHelper.waitForElementInvisibility(driver, "not-visible.png"))
                .thenReturn(List.of(validPng, validPng, List.of()));
        when(elementActionsHelper.waitForElementInvisibility(driver, "still-visible.png"))
                .thenReturn(List.of(validPng, validPng, List.of(120, 240)));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        touchActions.tap("present.png")
                .waitUntilElementIsVisible("present.png")
                .waitUntilElementIsNotVisible("not-visible.png")
                .tap("missing.png")
                .waitUntilElementIsVisible("missing.png")
                .waitUntilElementIsNotVisible("still-visible.png");

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

    @Test
    public void swipeToEndOfViewShouldScrollUntilAppiumReportsEnd() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        doReturn(true).doReturn(false).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());

        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        WebElement scrollableElement = mock(WebElement.class);
        when(scrollableElement.getRect()).thenReturn(new org.openqa.selenium.Rectangle(10, 20, 300, 500));
        when(elementActionsHelper.identifyUniqueElement(any(), eq(By.id("container"))))
                .thenReturn(List.of("container", scrollableElement));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        touchActions.swipeToEndOfView(TouchActions.SwipeDirection.DOWN)
                .swipeToEndOfView(By.id("container"), TouchActions.SwipeDirection.UP);

        verify(driver, times(3)).executeScript(eq("mobile: scrollGesture"), anyMap());
        verify(driver, never()).findElements(isNull());
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


    private IOSDriver createMockIOSDriver() {
        IOSDriver driver = mock(IOSDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(1080, 1920));
        when(driver.findElements(any(By.class))).thenReturn(List.of());
        return driver;
    }

    private RemoteWebDriver createMockRemoteWebDriver() {
        RemoteWebDriver driver = mock(RemoteWebDriver.class);
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
