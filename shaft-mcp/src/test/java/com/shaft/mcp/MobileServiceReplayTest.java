package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.Actions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Exercises {@code mobile_replay_recording} (the {@code /record-mobile} and {@code /codegen}
 * business logic behind {@link MobileService#replayRecording}), including the mobile-specific
 * rule that a redacted {@code type} action must throw rather than replay silently, and the private
 * {@code replay(...)} action dispatch that no other test in the suite exercises.
 */
class MobileServiceReplayTest {
    @TempDir
    Path temp;

    @Test
    void replayRejectsARedactedTypeActionWithoutRequiringAnActiveDriver() {
        McpMobileRecordingService fixture = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("redacted-type.json");
        fixture.start(recording.toString(), "native", false);
        fixture.record("type", locatorStrategy.ACCESSIBILITY_ID, "username", Map.of("value", "secret"),
                "driver.element().type(SHAFT.GUI.Locator.accessibilityId(\"username\"), \"secret\");",
                "driver.element().type(SHAFT.GUI.Locator.accessibilityId(\"username\"), \"<redacted>\");",
                true);
        fixture.stop(false);
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service.replayRecording(recording.toString(), "driver"));

        assertTrue(failure.getMessage().contains("omitted sensitive values"));
    }

    @Test
    void replayExecutesEachSupportedMobileActionAgainstAMockedDriver() {
        McpMobileRecordingService fixture = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("full-journey.json");
        fixture.start(recording.toString(), "native", true);
        fixture.record("tap", locatorStrategy.ACCESSIBILITY_ID, "login", Map.of(),
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));", "…", false);
        fixture.record("doubleTap", locatorStrategy.ACCESSIBILITY_ID, "login", Map.of(),
                "driver.element().touch().doubleTap(SHAFT.GUI.Locator.accessibilityId(\"login\"));", "…", false);
        fixture.record("longTap", locatorStrategy.ACCESSIBILITY_ID, "login", Map.of(),
                "driver.element().touch().longTap(SHAFT.GUI.Locator.accessibilityId(\"login\"));", "…", false);
        fixture.record("type", locatorStrategy.ACCESSIBILITY_ID, "username", Map.of("value", "demo"),
                "driver.element().type(SHAFT.GUI.Locator.accessibilityId(\"username\"), \"demo\");", "…", false);
        fixture.record("clear", locatorStrategy.ACCESSIBILITY_ID, "username", Map.of(),
                "driver.element().clear(SHAFT.GUI.Locator.accessibilityId(\"username\"));", "…", false);
        fixture.record("swipeByOffset", locatorStrategy.ACCESSIBILITY_ID, "list",
                Map.of("xOffset", "10", "yOffset", "20"),
                "driver.element().touch().swipeByOffset(SHAFT.GUI.Locator.accessibilityId(\"list\"), 10, 20);", "…", false);
        fixture.record("swipeElementIntoView", locatorStrategy.ACCESSIBILITY_ID, "target",
                Map.of("direction", "DOWN"),
                "driver.element().touch().swipeElementIntoView(SHAFT.GUI.Locator.accessibilityId(\"target\"), \"DOWN\");",
                "…", false);
        fixture.record("swipeTextIntoView", null, "",
                Map.of("targetText", "Find me", "movement", "VERTICAL"),
                "driver.element().touch().swipeElementIntoView(\"Find me\", \"VERTICAL\");", "…", false);
        fixture.record("tapCoordinates", null, "", Map.of("x", "10", "y", "20"),
                "driver.element().touch().tapByCoordinates(10, 20);", "…", false);
        fixture.record("swipeCoordinates", null, "",
                Map.of("startX", "1", "startY", "2", "endX", "3", "endY", "4", "durationMillis", "100"),
                "driver.element().touch().swipeByCoordinates(1, 2, 3, 4, 100);", "…", false);
        fixture.record("rotate", null, "", Map.of("orientation", "PORTRAIT"),
                "driver.element().touch().rotate(\"PORTRAIT\");", "…", false);
        fixture.record("hideKeyboard", null, "", Map.of(),
                "driver.element().touch().hideNativeKeyboard();", "…", false);
        fixture.record("keyboardKey", null, "", Map.of("key", "DONE"),
                "driver.element().touch().nativeKeyboardKeyPress(\"DONE\");", "…", false);
        fixture.record("backgroundApp", null, "", Map.of("seconds", "5"),
                "driver.element().touch().sendAppToBackground(5);", "…", false);
        fixture.record("activateApp", null, "", Map.of("appId", "com.example"),
                "driver.element().touch().activateAppFromBackground(\"com.example\");", "…", false);
        fixture.stop(false);

        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        TouchActions touch = mock(TouchActions.class);
        Actions element = mock(Actions.class);
        RemoteWebDriver remoteDriver = mock(RemoteWebDriver.class);
        when(shaftDriver.touch()).thenReturn(touch);
        when(shaftDriver.element()).thenReturn(element);
        when(shaftDriver.getDriver()).thenReturn(remoteDriver);

        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        McpMobileReplayResult replay;
        // CALLS_REAL_METHODS is required here: mockStatic(EngineService.class) otherwise stubs
        // every static method on the class to a default answer (null for objects), which would
        // silently turn the real, pure EngineService.getLocator(...) helper that replay() calls
        // for each locator-based action into a null-returning stub too, and every By-typed
        // argument below would arrive as null instead of a real locator.
        try (MockedStatic<EngineService> engineServiceMock =
                     mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            engineServiceMock.when(EngineService::getDriver).thenReturn(shaftDriver);
            replay = service.replayRecording(recording.toString(), "driver");
        }

        assertEquals(15, replay.replayedActionCount());
        verify(touch).tap(any(By.class));
        verify(touch).doubleTap(any(By.class));
        verify(touch).longTap(any(By.class));
        verify(element).type(any(By.class), anyString());
        verify(element).clear(any(By.class));
        verify(touch).swipeByOffset(any(By.class), eq(10), eq(20));
        verify(touch).swipeElementIntoView(any(By.class), eq(TouchActions.SwipeDirection.DOWN));
        verify(touch).swipeElementIntoView(eq("Find me"), eq(TouchActions.SwipeMovement.VERTICAL));
        verify(touch).rotate(eq(org.openqa.selenium.ScreenOrientation.PORTRAIT));
        verify(touch).hideNativeKeyboard();
        verify(touch).nativeKeyboardKeyPress(eq(TouchActions.KeyboardKeys.DONE));
        verify(touch).sendAppToBackground(5);
        verify(touch).activateAppFromBackground("com.example");
        verify(remoteDriver, times(2)).perform(anyCollection());
    }
}
