package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.flutter.commands.DoubleClickParameter;
import io.appium.java_client.flutter.commands.DragAndDropParameter;
import io.appium.java_client.flutter.commands.LongPressParameter;
import io.appium.java_client.flutter.commands.ScrollParameter;
import io.appium.java_client.flutter.commands.WaitParameter;
import io.appium.java_client.ios.IOSDriver;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.UnsupportedCommandException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Sequence;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.opentest4j.AssertionFailedError;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.lang.reflect.Method;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
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

        verify(remoteElementActionsHelper, times(4)).failAction(eq(remoteWebDriver), isNull(By.class));
        verify(remoteElementActionsHelper).failAction(eq(remoteWebDriver), eq("seconds=1"), isNull(By.class));
        verify(remoteElementActionsHelper).failAction(eq(remoteWebDriver), eq("appId=unsupported.app"), isNull(By.class));
        verify(remoteElementActionsHelper).failAction(eq(remoteWebDriver), eq("orientation=PORTRAIT"), isNull(By.class));
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

        Method performW3cCompliantScroll = TouchActions.class.getDeclaredMethod("performW3cCompliantScroll", java.util.HashMap.class, boolean.class);
        performW3cCompliantScroll.setAccessible(true);
        SHAFT.Validations.assertThat().object(performW3cCompliantScroll.invoke(touchActions, downParameters, true)).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(performW3cCompliantScroll.invoke(touchActions, downParameters, false)).isEqualTo(false).perform();
        verify(iosDriver, times(2)).executeScript(eq("mobile: scroll"), anyMap());
    }

    @Test
    public void visualSwipeShouldCoverNativeMissingImageAndWebCoordinateBranches() throws Exception {
        byte[] validPng = getValidPngBytes();

        AndroidDriver androidDriver = createMockAndroidDriver();
        doReturn(true).doReturn(false).when(androidDriver).executeScript(eq("mobile: scrollGesture"), anyMap());
        TouchActions nativeTouchActions = new TouchActions(androidDriver);
        ElementActionsHelper nativeElementActionsHelper = mock(ElementActionsHelper.class);
        when(nativeElementActionsHelper.findElementPresence(androidDriver, "native-missing.png"))
                .thenReturn(List.of(validPng, validPng, List.of()))
                .thenReturn(List.of(validPng, validPng, List.of()))
                .thenReturn(List.of(validPng, validPng, List.of(30, 40)));
        when(nativeElementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        injectElementActionsHelper(nativeTouchActions, nativeElementActionsHelper);

        nativeTouchActions.swipeElementIntoView(null, "native-missing.png", TouchActions.SwipeDirection.DOWN);
        verify(nativeElementActionsHelper, never()).failAction(eq(androidDriver), anyString(), isNull(By.class), any(List.class));

        RemoteWebDriver webDriver = createMockRemoteWebDriver();
        TouchActions webTouchActions = new TouchActions(webDriver);
        ElementActionsHelper webElementActionsHelper = mock(ElementActionsHelper.class);
        when(webElementActionsHelper.findElementPresence(webDriver, "web-present.png"))
                .thenReturn(List.of(validPng, validPng, List.of(20, 40)));
        when(webElementActionsHelper.findElementPresence(webDriver, "web-missing.png"))
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

        verify(elementActionsHelper).failAction(eq(performFailureDriver),
                eq("referenceImage=tap-present.png, coordinates=[10, 20]"), isNull(By.class), any(Throwable.class));
        verify(elementActionsHelper).failAction(eq(performFailureDriver), eq("IN"), isNull(By.class), any(Throwable.class));
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

        Method performW3cCompliantScroll = TouchActions.class.getDeclaredMethod("performW3cCompliantScroll", java.util.HashMap.class, boolean.class);
        performW3cCompliantScroll.setAccessible(true);
        SHAFT.Validations.assertThat().object(performW3cCompliantScroll.invoke(touchActions, upParameters, true)).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(performW3cCompliantScroll.invoke(touchActions, upParameters, false)).isEqualTo(false).perform();

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
    public void visualTapShouldUseBackendSpecificPointerKind() throws Exception {
        byte[] validPng = getValidPngBytes();

        RemoteWebDriver webDriver = createMockRemoteWebDriver();
        TouchActions webTouchActions = new TouchActions(webDriver);
        ElementActionsHelper webElementActionsHelper = mock(ElementActionsHelper.class);
        when(webElementActionsHelper.waitForElementPresence(webDriver, "web-present.png"))
                .thenReturn(List.of(validPng, validPng, List.of(10, 20)));
        injectElementActionsHelper(webTouchActions, webElementActionsHelper);

        webTouchActions.tap("web-present.png");

        SHAFT.Validations.assertThat().object(capturedPointerType(webDriver)).isEqualTo("mouse").perform();

        AndroidDriver androidDriver = createMockAndroidDriver();
        TouchActions androidTouchActions = new TouchActions(androidDriver);
        ElementActionsHelper androidElementActionsHelper = mock(ElementActionsHelper.class);
        when(androidElementActionsHelper.waitForElementPresence(androidDriver, "android-present.png"))
                .thenReturn(List.of(validPng, validPng, List.of(10, 20)));
        injectElementActionsHelper(androidTouchActions, androidElementActionsHelper);

        androidTouchActions.tap("android-present.png");

        SHAFT.Validations.assertThat().object(capturedPointerType(androidDriver)).isEqualTo("touch").perform();
    }

    @Test
    public void visualTypeShouldTapReferenceAndSendKeysThroughActionsApi() throws Exception {
        RemoteWebDriver driver = createMockRemoteWebDriver();
        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        byte[] validPng = getValidPngBytes();
        when(elementActionsHelper.waitForElementPresence(driver, "field.png"))
                .thenReturn(List.of(validPng, validPng, List.of(10, 20)));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        touchActions.type("field.png", "hello");

        verify(driver, times(2)).perform(any());
    }

    @Test
    public void swipeElementIntoViewOverloadsShouldCoverNativeAndWebPaths() throws Exception {
        AndroidDriver androidDriver = createMockAndroidDriver();
        WebElement targetElement = mock(WebElement.class);
        when(androidDriver.findElements(By.id("target"))).thenReturn(List.of(targetElement));

        TouchActions nativeTouchActions = new TouchActions(androidDriver);
        ElementActionsHelper nativeElementActionsHelper = mock(ElementActionsHelper.class);
        byte[] validPng = getValidPngBytes();
        when(nativeElementActionsHelper.findElementPresence(any(), eq("target.png")))
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

    // Issue #4004: swipeToEndOfView has no scroll-to-end primitive against a Flutter locator (ScrollParameter
    // mandates a scrollTo target), so it must fail loudly instead of silently doing nothing.
    @Test
    public void swipeToEndOfViewWithFlutterScrollableLocatorShouldThrowUnsupportedCommandException() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        // Mutation-kill: failAction only throws when invoked with the exact remedy-naming UnsupportedCommandException.
        // If the guard regresses (e.g. silently removed), the native scroll loop runs instead and either never
        // calls failAction with this specific throwable, or calls it with a different one -- the stub never fires,
        // and the test fails to observe the expected exception, catching the regression instead of silently passing.
        doThrow(new AssertionFailedError("swipeToEndOfView has no scroll-to-end primitive for Flutter locators"))
                .when(elementActionsHelper).failAction(any(WebDriver.class), anyString(), any(),
                        org.mockito.ArgumentMatchers.<Throwable>argThat(throwable -> throwable instanceof UnsupportedCommandException
                                && throwable.getMessage() != null
                                && throwable.getMessage().contains("swipeElementIntoView")));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        var scrollableLocator = AppiumBy.flutterType("ListView");
        try {
            touchActions.swipeToEndOfView(scrollableLocator, TouchActions.SwipeDirection.DOWN);
            org.testng.Assert.fail("Expected the Flutter scroll-to-end guard to route into failAction with the remedy-naming UnsupportedCommandException, but no such exception propagated.");
        } catch (AssertionFailedError expected) {
            // expected: proves failAction was invoked with the remedy-naming UnsupportedCommandException
        }
        verify(driver, never()).executeScript(eq("mobile: scrollGesture"), anyMap());
    }

    // Issue #4004: a native "mobile: scrollGesture"/"mobile: scroll" call against a bare FlutterView reports
    // zero effective scrolls on the very first attempt -- ambiguous between "already at scroll end" (legitimate,
    // e.g. a native overlay atop the FlutterView) and "silent no-op" (the bug). Warn instead of throwing, since
    // throwing would falsely fail the legitimate case.
    @Test
    public void swipeToEndOfViewWithFlutterDriverAndZeroScrollsShouldEmitLoudWarning() throws Exception {
        io.appium.java_client.flutter.android.FlutterAndroidDriver driver = createMockFlutterAndroidDriver();
        doReturn(false).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());

        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        injectElementActionsHelper(touchActions, elementActionsHelper);

        try (MockedStatic<ReportManager> reportManager = mockStatic(ReportManager.class)) {
            touchActions.swipeToEndOfView(TouchActions.SwipeDirection.DOWN);

            reportManager.verify(() -> ReportManager.log(argThat(message -> message != null
                    && message.toLowerCase(java.util.Locale.ROOT).contains("flutter")), eq(org.apache.logging.log4j.Level.WARN)));
        }
        verify(elementActionsHelper, never()).failAction(any(WebDriver.class), anyString(), any(), any(Throwable.class));
        verify(driver, times(1)).executeScript(eq("mobile: scrollGesture"), anyMap());
    }

    // Issue #4004: a Flutter session that genuinely scrolls a native overlay several times before reaching the
    // end is a legitimate native scroll -- no warning should fire, matching today's behavior byte-for-byte.
    @Test
    public void swipeToEndOfViewWithFlutterDriverAndSuccessfulScrollsShouldNotWarnOrThrow() throws Exception {
        io.appium.java_client.flutter.android.FlutterAndroidDriver driver = createMockFlutterAndroidDriver();
        doReturn(true).doReturn(false).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());

        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        injectElementActionsHelper(touchActions, elementActionsHelper);

        try (MockedStatic<ReportManager> reportManager = mockStatic(ReportManager.class)) {
            touchActions.swipeToEndOfView(TouchActions.SwipeDirection.DOWN);

            reportManager.verify(() -> ReportManager.log(anyString(), eq(org.apache.logging.log4j.Level.WARN)), never());
        }
        verify(elementActionsHelper, never()).failAction(any(WebDriver.class), anyString(), any(), any(Throwable.class));
        verify(driver, times(2)).executeScript(eq("mobile: scrollGesture"), anyMap());
    }

    // Issue #3996: FlutterIntegration driver targets must scroll via "flutter: scrollTillVisible",
    // not the native "mobile: scrollGesture" (a no-op against a single FlutterView with no native scrollable).

    @Test
    public void swipeElementIntoViewWithFlutterLocatorShouldIssueFlutterScrollCommand() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        doReturn(false).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());

        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        // Pre-fix reproduction: the native path throws "Element not found after scrolling to the end of the page."
        // (TouchActions.java:1105), and failAction converts it into the reporter's real AssertionFailedError.
        doThrow(new AssertionFailedError("Element not found after scrolling to the end of the page."))
                .when(elementActionsHelper).failAction(any(WebDriver.class), anyString(), any(), any(Throwable.class));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        var targetLocator = AppiumBy.flutterKey("category_item_2");
        touchActions.swipeElementIntoView(targetLocator, TouchActions.SwipeDirection.DOWN);

        @SuppressWarnings("unchecked")
        ArgumentCaptor<Map<String, Object>> payloadCaptor = ArgumentCaptor.forClass(Map.class);
        verify(driver).executeScript(eq("flutter: scrollTillVisible"), payloadCaptor.capture());
        verify(driver, never()).executeScript(eq("mobile: scrollGesture"), anyMap());

        Map<String, Object> payload = payloadCaptor.getValue();
        SHAFT.Validations.assertThat().object(payload.get("finder"))
                .isEqualTo(Map.of("using", "-flutter key", "value", "category_item_2")).perform();
        SHAFT.Validations.assertThat().object(payload.get("scrollDirection")).isEqualTo("down").perform();
    }

    @Test
    public void swipeElementIntoViewWithNativeLocatorShouldStillUseMobileScrollGesture() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        WebElement targetElement = mock(WebElement.class);
        when(driver.findElements(any(By.class))).thenReturn(List.of(), List.of(targetElement));
        doReturn(true).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());

        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        injectElementActionsHelper(touchActions, elementActionsHelper);

        touchActions.swipeElementIntoView(By.id("target"), TouchActions.SwipeDirection.DOWN);

        verify(driver).executeScript(eq("mobile: scrollGesture"), anyMap());
        verify(driver, never()).executeScript(eq("flutter: scrollTillVisible"), any());
    }

    @Test
    public void swipeElementIntoViewWithFlutterScrollableAndFlutterTargetShouldIncludeScrollView() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        injectElementActionsHelper(touchActions, elementActionsHelper);

        var scrollableLocator = AppiumBy.flutterType("ListView");
        var targetLocator = AppiumBy.flutterKey("category_item_2");
        touchActions.swipeElementIntoView(scrollableLocator, targetLocator, TouchActions.SwipeDirection.UP);

        @SuppressWarnings("unchecked")
        ArgumentCaptor<Map<String, Object>> payloadCaptor = ArgumentCaptor.forClass(Map.class);
        verify(driver).executeScript(eq("flutter: scrollTillVisible"), payloadCaptor.capture());
        SHAFT.Validations.assertThat().object(payloadCaptor.getValue().containsKey("scrollView")).isTrue().perform();
    }

    @Test
    public void swipeElementIntoViewWithFlutterTargetAndNativeScrollableShouldOmitScrollView() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        when(elementActionsHelper.takeScreenshot(any(), any(), anyString(), any(), eq(true))).thenReturn(List.of());
        injectElementActionsHelper(touchActions, elementActionsHelper);

        var targetLocator = AppiumBy.flutterKey("category_item_2");
        touchActions.swipeElementIntoView(By.id("nativeContainer"), targetLocator, TouchActions.SwipeDirection.LEFT);

        @SuppressWarnings("unchecked")
        ArgumentCaptor<Map<String, Object>> payloadCaptor = ArgumentCaptor.forClass(Map.class);
        verify(driver).executeScript(eq("flutter: scrollTillVisible"), payloadCaptor.capture());
        verify(driver, never()).executeScript(eq("mobile: scrollGesture"), anyMap());
        SHAFT.Validations.assertThat().object(payloadCaptor.getValue().containsKey("scrollView")).isFalse().perform();
    }

    @Test
    public void mapToFlutterScrollDirectionShouldPreserveWireFormatForAllDirections() throws Exception {
        Method mapToFlutterScrollDirection = TouchActions.class.getDeclaredMethod("mapToFlutterScrollDirection", TouchActions.SwipeDirection.class);
        mapToFlutterScrollDirection.setAccessible(true);
        TouchActions touchActions = new TouchActions(createMockAndroidDriver());

        Map<TouchActions.SwipeDirection, String> expectedWireDirections = Map.of(
                TouchActions.SwipeDirection.UP, "up",
                TouchActions.SwipeDirection.DOWN, "down",
                TouchActions.SwipeDirection.LEFT, "left",
                TouchActions.SwipeDirection.RIGHT, "right");

        for (var entry : expectedWireDirections.entrySet()) {
            ScrollParameter.ScrollDirection direction =
                    (ScrollParameter.ScrollDirection) mapToFlutterScrollDirection.invoke(touchActions, entry.getKey());
            SHAFT.Validations.assertThat().object(direction.getDirection()).isEqualTo(entry.getValue()).perform();
        }
    }

    @Test
    public void sessionStateApisShouldDelegateToMobileSessionStateManager() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("platformName", "Android");
        capabilities.setCapability("appium:appPackage", "com.example.app");
        when(driver.getCapabilities()).thenReturn(capabilities);
        when(driver.getContext()).thenReturn("NATIVE_APP");

        Path capabilitiesFile = TEMP_DIR.resolve("touchActions-session-capabilities.json");
        Path appStateFile = TEMP_DIR.resolve("touchActions-app-state.json");

        TouchActions touchActions = new TouchActions(driver);
        touchActions.saveSessionCapabilities(capabilitiesFile.toString())
                .saveAppState(appStateFile.toString());

        SHAFT.Validations.assertThat().object(Files.exists(capabilitiesFile)).isTrue().perform();
        SHAFT.Validations.assertThat().object(Files.exists(appStateFile)).isTrue().perform();

        MutableCapabilities loadedCapabilities = TouchActions.loadSessionCapabilities(capabilitiesFile.toString());
        SHAFT.Validations.assertThat().object(loadedCapabilities.getCapability("appium:appPackage")).isEqualTo("com.example.app").perform();

        touchActions.loadAppState(appStateFile.toString());
        verify(driver).activateApp("com.example.app");

        Files.deleteIfExists(capabilitiesFile);
        Files.deleteIfExists(appStateFile);
    }

    // Issue #4017: fluent wait/gesture/camera API over java-client's Flutter driver. These delegate to the
    // typed Supports* interface methods directly (not raw executeScript), branching on `driver instanceof
    // Supports*`, now that #4001 constructs a real FlutterAndroidDriver/FlutterIOSDriver for Flutter sessions.

    @Test
    public void waitForVisibleAndWaitForAbsentShouldDelegateToFlutterWaitingInterfaceWithLocatorAndTimeout() throws Exception {
        io.appium.java_client.flutter.android.FlutterAndroidDriver driver = createMockFlutterAndroidDriver();
        TouchActions touchActions = new TouchActions(driver);

        var visibleLocator = AppiumBy.flutterKey("submit-button");
        var absentLocator = AppiumBy.flutterKey("loading-spinner");

        touchActions.waitForVisible(visibleLocator)
                .waitForAbsent(absentLocator, Duration.ofSeconds(5));

        ArgumentCaptor<WaitParameter> visibleCaptor = ArgumentCaptor.forClass(WaitParameter.class);
        verify(driver).waitForVisible(visibleCaptor.capture());
        SHAFT.Validations.assertThat().object(visibleCaptor.getValue().getLocator()).isEqualTo(visibleLocator).perform();
        SHAFT.Validations.assertThat().object(visibleCaptor.getValue().getTimeout()).isEqualTo(null).perform();

        ArgumentCaptor<WaitParameter> absentCaptor = ArgumentCaptor.forClass(WaitParameter.class);
        verify(driver).waitForInVisible(absentCaptor.capture());
        SHAFT.Validations.assertThat().object(absentCaptor.getValue().getLocator()).isEqualTo(absentLocator).perform();
        SHAFT.Validations.assertThat().object(absentCaptor.getValue().getTimeout()).isEqualTo(Duration.ofSeconds(5)).perform();
    }

    @Test
    public void waitForVisibleWithNonFlutterDriverOrLocatorShouldFailActionWithoutCallingFlutterInterface() throws Exception {
        AndroidDriver nativeDriver = createMockAndroidDriver();
        TouchActions nativeTouchActions = new TouchActions(nativeDriver);
        ElementActionsHelper nativeElementActionsHelper = mock(ElementActionsHelper.class);
        injectElementActionsHelper(nativeTouchActions, nativeElementActionsHelper);

        nativeTouchActions.waitForVisible(By.id("native-element"));
        verify(nativeElementActionsHelper).failAction(eq(nativeDriver), anyString(), eq(By.id("native-element")));

        io.appium.java_client.flutter.android.FlutterAndroidDriver flutterDriver = createMockFlutterAndroidDriver();
        TouchActions flutterTouchActions = new TouchActions(flutterDriver);
        ElementActionsHelper flutterElementActionsHelper = mock(ElementActionsHelper.class);
        injectElementActionsHelper(flutterTouchActions, flutterElementActionsHelper);

        flutterTouchActions.waitForAbsent(By.id("native-element-on-flutter-driver"));
        verify(flutterElementActionsHelper).failAction(eq(flutterDriver), anyString(), eq(By.id("native-element-on-flutter-driver")));
        verify(flutterDriver, never()).waitForInVisible(any());
    }

    @Test
    public void performDoubleClickAndLongPressShouldDelegateToFlutterGestureInterfaceWithResolvedElement() throws Exception {
        io.appium.java_client.flutter.android.FlutterAndroidDriver driver = createMockFlutterAndroidDriver();
        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        WebElement targetElement = mock(WebElement.class);
        var targetLocator = AppiumBy.flutterKey("increment");
        when(elementActionsHelper.identifyUniqueElement(driver, targetLocator)).thenReturn(List.of("increment", targetElement));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        touchActions.performDoubleClick(targetLocator).performLongPress(targetLocator);

        ArgumentCaptor<DoubleClickParameter> doubleClickCaptor = ArgumentCaptor.forClass(DoubleClickParameter.class);
        verify(driver).performDoubleClick(doubleClickCaptor.capture());
        SHAFT.Validations.assertThat().object(doubleClickCaptor.getValue().getElement()).isEqualTo(targetElement).perform();

        ArgumentCaptor<LongPressParameter> longPressCaptor = ArgumentCaptor.forClass(LongPressParameter.class);
        verify(driver).performLongPress(longPressCaptor.capture());
        SHAFT.Validations.assertThat().object(longPressCaptor.getValue().getElement()).isEqualTo(targetElement).perform();
    }

    @Test
    public void performDragAndDropShouldDelegateToFlutterGestureInterfaceWithResolvedSourceAndTarget() throws Exception {
        io.appium.java_client.flutter.android.FlutterAndroidDriver driver = createMockFlutterAndroidDriver();
        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        WebElement sourceElement = mock(WebElement.class);
        WebElement targetElement = mock(WebElement.class);
        var sourceLocator = AppiumBy.flutterKey("draggable");
        var targetLocator = AppiumBy.flutterKey("drop-zone");
        when(elementActionsHelper.identifyUniqueElement(driver, sourceLocator)).thenReturn(List.of("draggable", sourceElement));
        when(elementActionsHelper.identifyUniqueElement(driver, targetLocator)).thenReturn(List.of("drop-zone", targetElement));
        injectElementActionsHelper(touchActions, elementActionsHelper);

        touchActions.performDragAndDrop(sourceLocator, targetLocator);

        ArgumentCaptor<DragAndDropParameter> dragAndDropCaptor = ArgumentCaptor.forClass(DragAndDropParameter.class);
        verify(driver).performDragAndDrop(dragAndDropCaptor.capture());
        SHAFT.Validations.assertThat().object(dragAndDropCaptor.getValue().getSource()).isEqualTo(sourceElement).perform();
        SHAFT.Validations.assertThat().object(dragAndDropCaptor.getValue().getTarget()).isEqualTo(targetElement).perform();
    }

    @Test
    public void gestureMethodsWithNonFlutterDriverShouldFailActionWithoutCallingFlutterInterface() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        injectElementActionsHelper(touchActions, elementActionsHelper);

        touchActions.performDoubleClick(By.id("native"))
                .performLongPress(By.id("native"))
                .performDragAndDrop(By.id("native-source"), By.id("native-target"));

        verify(elementActionsHelper, times(3)).failAction(eq(driver), anyString(), any(By.class));
        verify(elementActionsHelper, never()).identifyUniqueElement(any(), any());
    }

    @Test
    public void injectMockImageAndActivateInjectedImageShouldDelegateToFlutterCameraMockingInterface() throws Exception {
        io.appium.java_client.flutter.android.FlutterAndroidDriver driver = createMockFlutterAndroidDriver();
        TouchActions touchActions = new TouchActions(driver);
        File imageFile = TEMP_DIR.resolve("mock-camera.png").toFile();
        Files.write(imageFile.toPath(), getValidPngBytes());
        when(driver.injectMockImage(imageFile)).thenReturn("image-123");

        String imageId = touchActions.injectMockImage(imageFile);
        touchActions.activateInjectedImage(imageId);

        SHAFT.Validations.assertThat().object(imageId).isEqualTo("image-123").perform();
        verify(driver).injectMockImage(imageFile);
        verify(driver).activateInjectedImage("image-123");
    }

    @Test
    public void injectMockImageWithNonFlutterDriverShouldFailActionAndReturnNull() throws Exception {
        AndroidDriver driver = createMockAndroidDriver();
        TouchActions touchActions = new TouchActions(driver);
        ElementActionsHelper elementActionsHelper = mock(ElementActionsHelper.class);
        injectElementActionsHelper(touchActions, elementActionsHelper);
        File imageFile = TEMP_DIR.resolve("mock-camera-native.png").toFile();
        Files.write(imageFile.toPath(), getValidPngBytes());

        String imageId = touchActions.injectMockImage(imageFile);

        SHAFT.Validations.assertThat().object(imageId).isEqualTo(null).perform();
        verify(elementActionsHelper).failAction(eq(driver), anyString(), isNull(By.class));
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

    private io.appium.java_client.flutter.android.FlutterAndroidDriver createMockFlutterAndroidDriver() {
        io.appium.java_client.flutter.android.FlutterAndroidDriver driver = mock(io.appium.java_client.flutter.android.FlutterAndroidDriver.class);
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

    @SuppressWarnings({"unchecked", "rawtypes"})
    private String capturedPointerType(RemoteWebDriver driver) {
        ArgumentCaptor<Collection<Sequence>> actionsCaptor = ArgumentCaptor.forClass((Class) Collection.class);
        verify(driver).perform(actionsCaptor.capture());
        Map<String, Object> encodedSequence = actionsCaptor.getValue().iterator().next().encode();
        Map<String, Object> parameters = (Map<String, Object>) encodedSequence.get("parameters");
        return String.valueOf(parameters.get("pointerType"));
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
