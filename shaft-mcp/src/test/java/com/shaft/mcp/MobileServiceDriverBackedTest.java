package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.Actions;
import io.appium.java_client.remote.SupportsContextSwitching;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.openqa.selenium.By;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

/**
 * Exercises the driver-backed (success and failure) paths of {@link MobileService} that
 * {@link McpNoDriverServiceTest} only reaches up to the initial {@code getDriver()} guard: context
 * inspection, screenshots, context switching, touch/keyboard tool calls, and the wrapped Appium
 * Inspector delegation methods. Uses a Mockito-mocked {@link SHAFT.GUI.WebDriver} behind a
 * {@code mockStatic(EngineService.class, CALLS_REAL_METHODS)} so the real, pure
 * {@code EngineService.getLocator(...)} helper still runs while only {@code getDriver()} is
 * stubbed — never a real Appium/emulator session.
 */
class MobileServiceDriverBackedTest {
    @TempDir
    Path temp;

    private static SHAFT.GUI.WebDriver mockShaftDriver(WebDriver seleniumDriver, TouchActions touch, Actions element) {
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(seleniumDriver);
        when(shaftDriver.touch()).thenReturn(touch);
        when(shaftDriver.element()).thenReturn(element);
        return shaftDriver;
    }

    @Test
    void inspectorAndToolchainToolsDelegateToTheUnderlyingRecordingServices() {
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));

        McpMobileToolchainStatus toolchain = service.toolchainStatus("Android");
        McpMobileInspectorPlan plan = service.inspectorRecordPrepare(
                "Android", "recordings/native.json", true,
                "", "", "", "", "", "", "", "", 0, "", "", "", 0, 0, false);
        // mobile_inspector_record_status absorbs mobile_inspector_record_control via an optional
        // action param (design doc Decision 2): a blank action just returns status.
        McpMobileInspectorRecordingStatus status = service.inspectorRecordStatus(null, null);
        McpMobileInspectorRecordingStatus control = service.inspectorRecordStatus("status", "");
        McpMobileInspectorRecordingStatus stopped = service.inspectorRecordStop(false);

        assertFalse(toolchain.platformName().isBlank());
        assertFalse(plan.readyToStart());
        assertFalse(status.active());
        assertFalse(control.active());
        assertFalse(stopped.active());
        // mobile_inspector_record_start now auto-runs prepare (absorbing mobile_inspector_record_prepare,
        // design doc Decision 2): with no real Appium toolchain, the auto-prepared plan is never
        // ready-to-start, so it fails deterministically with an actionable message instead of the old
        // "unknown confirmation token" shape.
        IllegalStateException failure = assertThrows(IllegalStateException.class,
                () -> service.inspectorRecordStart(
                        "Android", "recordings/native.json", true,
                        "", "", "", "", "", "", "", "", 0, "", "", "", 0, 0, false, false));
        assertTrue(failure.getMessage().contains("not ready to start"), failure.getMessage());
    }

    @Test
    void driverBackedContextAndAccessibilityToolsReturnDeviceStateAndTruncateWhenTooLong() {
        WebDriver seleniumDriver = mock(WebDriver.class,
                withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching contextDriver = (SupportsContextSwitching) seleniumDriver;
        when(contextDriver.getContextHandles()).thenReturn(Set.of("NATIVE_APP", "WEBVIEW_1"));
        when(contextDriver.getContext()).thenReturn("NATIVE_APP");
        String longSource = "<hierarchy>" + "x".repeat(50) + "</hierarchy>";
        when(seleniumDriver.getPageSource()).thenReturn(longSource);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(seleniumDriver, mock(TouchActions.class), mock(Actions.class));
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));

        try (MockedStatic<EngineService> m = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            m.when(EngineService::getDriver).thenReturn(shaftDriver);

            McpMobileContextSnapshot contexts = service.getContexts(10);
            McpMobileContextSnapshot fullContexts = service.getContexts(0);
            McpMobileAccessibilityTree tree = service.getAccessibilityTree(10);

            assertEquals("NATIVE_APP", contexts.currentContext());
            assertTrue(contexts.contexts().containsAll(Set.of("NATIVE_APP", "WEBVIEW_1")));
            assertTrue(contexts.truncated());
            assertEquals(10, contexts.pageSource().length());
            assertFalse(fullContexts.truncated());
            assertEquals(longSource, fullContexts.pageSource());
            assertTrue(tree.truncated());
            assertEquals("NATIVE_APP", tree.currentContext());
        }
    }

    @Test
    void driverBackedScreenshotWritesFileAndRejectsDriversWithoutScreenshotSupport() throws Exception {
        WebDriver screenshotCapableDriver = mock(WebDriver.class, withSettings().extraInterfaces(TakesScreenshot.class));
        byte[] png = {5, 6, 7};
        when(((TakesScreenshot) screenshotCapableDriver).getScreenshotAs(OutputType.BYTES)).thenReturn(png);
        SHAFT.GUI.WebDriver shaftDriverWithScreenshot =
                mockShaftDriver(screenshotCapableDriver, mock(TouchActions.class), mock(Actions.class));
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));

        try (MockedStatic<EngineService> m = mockStatic(EngineService.class)) {
            m.when(EngineService::getDriver).thenReturn(shaftDriverWithScreenshot);

            McpScreenshotResult result = service.takeScreenshot("shots/mobile.png", true);

            assertEquals(java.util.Base64.getEncoder().encodeToString(png), result.base64());
            assertTrue(Files.isRegularFile(temp.resolve("shots/mobile.png")));
        }

        WebDriver plainDriver = mock(WebDriver.class);
        SHAFT.GUI.WebDriver shaftDriverWithoutScreenshot =
                mockShaftDriver(plainDriver, mock(TouchActions.class), mock(Actions.class));
        MobileService serviceWithoutScreenshotSupport =
                new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        try (MockedStatic<EngineService> m = mockStatic(EngineService.class)) {
            m.when(EngineService::getDriver).thenReturn(shaftDriverWithoutScreenshot);

            assertThrows(IllegalStateException.class,
                    () -> serviceWithoutScreenshotSupport.takeScreenshot("shots/none.png", false));
        }
    }

    @Test
    void driverBackedSwitchContextSucceedsFailsAndRejectsNonAppiumDrivers() {
        WebDriver seleniumDriver = mock(WebDriver.class,
                withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching contextDriver = (SupportsContextSwitching) seleniumDriver;
        when(contextDriver.getContext()).thenReturn("NATIVE_APP");
        when(contextDriver.getContextHandles()).thenReturn(Set.of("NATIVE_APP", "WEBVIEW_1"));
        when(seleniumDriver.getPageSource()).thenReturn("<hierarchy/>");
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(seleniumDriver, mock(TouchActions.class), mock(Actions.class));
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));

        try (MockedStatic<EngineService> m = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            m.when(EngineService::getDriver).thenReturn(shaftDriver);

            McpMobileContextSnapshot snapshot = service.switchContext("WEBVIEW_1");

            assertEquals("NATIVE_APP", snapshot.currentContext());
            verify(contextDriver).context("WEBVIEW_1");
        }

        WebDriver failingDriver = mock(WebDriver.class,
                withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching failingContextDriver = (SupportsContextSwitching) failingDriver;
        when(failingContextDriver.getContext()).thenReturn("NATIVE_APP");
        doThrow(new org.openqa.selenium.WebDriverException("no such context"))
                .when(failingContextDriver).context("MISSING");
        SHAFT.GUI.WebDriver failingShaftDriver =
                mockShaftDriver(failingDriver, mock(TouchActions.class), mock(Actions.class));
        MobileService failingService = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        try (MockedStatic<EngineService> m = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            m.when(EngineService::getDriver).thenReturn(failingShaftDriver);

            assertThrows(org.openqa.selenium.WebDriverException.class,
                    () -> failingService.switchContext("MISSING"));
        }

        WebDriver notAppiumDriver = mock(WebDriver.class);
        SHAFT.GUI.WebDriver shaftDriverNotAppium =
                mockShaftDriver(notAppiumDriver, mock(TouchActions.class), mock(Actions.class));
        MobileService serviceNotAppium = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        try (MockedStatic<EngineService> m = mockStatic(EngineService.class)) {
            m.when(EngineService::getDriver).thenReturn(shaftDriverNotAppium);

            assertThrows(IllegalStateException.class, () -> serviceNotAppium.switchContext("WEBVIEW_1"));
        }
    }

    @Test
    void driverBackedTouchAndKeyboardToolsRecordCodeAgainstAMockedDriver() {
        TouchActions touch = mock(TouchActions.class);
        Actions element = mock(Actions.class);
        RemoteWebDriver remoteDriver = mock(RemoteWebDriver.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(remoteDriver, touch, element);
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        service.recordStart(temp.resolve("recordings/touch.json").toString(), "native", true);

        try (MockedStatic<EngineService> m = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            m.when(EngineService::getDriver).thenReturn(shaftDriver);

            assertTrue(service.tap(locatorStrategy.ACCESSIBILITY_ID, "login").recorded());
            assertTrue(service.doubleTap(locatorStrategy.ACCESSIBILITY_ID, "login").recorded());
            assertTrue(service.longTap(locatorStrategy.ACCESSIBILITY_ID, "login").recorded());
            assertTrue(service.clear(locatorStrategy.ACCESSIBILITY_ID, "username").recorded());
            assertTrue(service.type(locatorStrategy.ACCESSIBILITY_ID, "username", "demo").recorded());
            assertTrue(service.swipeByOffset(locatorStrategy.ACCESSIBILITY_ID, "list", 10, 20).recorded());
            assertTrue(service.swipeElementIntoView(locatorStrategy.ACCESSIBILITY_ID, "target", "down").recorded());
            assertTrue(service.swipeTextIntoView("Find me", "vertical").recorded());
            assertTrue(service.tapCoordinates(10, 20).recorded());
            assertTrue(service.swipeCoordinates(1, 2, 3, 4, 100).recorded());
            assertTrue(service.rotate("landscape").recorded());
            assertTrue(service.hideKeyboard().recorded());
            assertTrue(service.keyboardKey("done").recorded());
            assertTrue(service.backgroundApp(5).recorded());
            assertTrue(service.activateApp("com.example").recorded());

            verify(touch).tap(any(By.class));
            verify(touch).doubleTap(any(By.class));
            verify(touch).longTap(any(By.class));
            verify(element).clear(any(By.class));
            verify(element).type(any(By.class), org.mockito.ArgumentMatchers.anyString());
            verify(touch).swipeByOffset(any(By.class), org.mockito.ArgumentMatchers.eq(10), org.mockito.ArgumentMatchers.eq(20));
            verify(touch).rotate(org.openqa.selenium.ScreenOrientation.LANDSCAPE);
            verify(touch).hideNativeKeyboard();
            verify(touch).nativeKeyboardKeyPress(TouchActions.KeyboardKeys.DONE);
            verify(touch).sendAppToBackground(5);
            verify(touch).activateAppFromBackground("com.example");
            verify(remoteDriver, org.mockito.Mockito.times(2)).perform(org.mockito.ArgumentMatchers.anyCollection());
        }
    }

    @Test
    void driverBackedTypeAndSwipeToolsLogAndRethrowUnderlyingFailures() {
        TouchActions touch = mock(TouchActions.class);
        Actions element = mock(Actions.class);
        doThrow(new RuntimeException("stale element"))
                .when(element).type(any(By.class), org.mockito.ArgumentMatchers.anyString());
        doThrow(new RuntimeException("swipe failed"))
                .when(touch).swipeByOffset(any(By.class), org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.anyInt());
        doThrow(new RuntimeException("swipe-into-view failed"))
                .when(touch).swipeElementIntoView(any(By.class), any(TouchActions.SwipeDirection.class));
        doThrow(new RuntimeException("swipe-text failed"))
                .when(touch).swipeElementIntoView(org.mockito.ArgumentMatchers.anyString(), any(TouchActions.SwipeMovement.class));
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(mock(WebDriver.class), touch, element);
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));

        try (MockedStatic<EngineService> m = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            m.when(EngineService::getDriver).thenReturn(shaftDriver);

            assertThrows(RuntimeException.class,
                    () -> service.type(locatorStrategy.ACCESSIBILITY_ID, "username", "demo"));
            assertThrows(RuntimeException.class,
                    () -> service.swipeByOffset(locatorStrategy.ACCESSIBILITY_ID, "list", 1, 2));
            assertThrows(RuntimeException.class,
                    () -> service.swipeElementIntoView(locatorStrategy.ACCESSIBILITY_ID, "target", "down"));
            assertThrows(RuntimeException.class,
                    () -> service.swipeTextIntoView("Find me", "vertical"));
        }
    }
}
