package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.MobileGestureActionsContract;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.mobile.MobileActions;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Constructor;
import java.time.Duration;
import java.util.List;
import java.util.Map;

@SuppressWarnings("PMD.AvoidAccessibilityAlteration") // Internal delegate injection crosses the real legacy boundary.
public class MobileGestureNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
    }

    @Test
    public void gestureNamespaceShouldOwnOneEventAndSuppressNestedLegacyEvidence() throws Exception {
        AndroidDriver driver = liveDriver("gesture-trace");
        TouchActions touch = Mockito.mock(TouchActions.class, Mockito.RETURNS_SELF);
        Mockito.doAnswer(invocation -> {
            var nested = TraceEventRecorder.start("legacy-touch", "tap", "nested", driver);
            TraceEventRecorder.finish(nested, "passed", "nested", null, Map.of(), List.of());
            return touch;
        }).when(touch).tapByCoordinates(7, 9);
        MobileGestureActionsContract gestures = gestures(driver, touch);

        gestures.tap().at(7, 9);

        assertSingle("tap-at", "passed");
    }

    @Test
    public void invalidAndStaleGesturesShouldFailInsideOneOwnerEventWithoutDelegation() throws Exception {
        AndroidDriver driver = liveDriver("gesture-validation");
        TouchActions touch = Mockito.mock(TouchActions.class, Mockito.RETURNS_SELF);
        MobileGestureActionsContract gestures = gestures(driver, touch);

        Assert.expectThrows(IllegalArgumentException.class,
                () -> gestures.swipe().fromTo(1, 2, 3, 4, Duration.ofMillis(-1)));
        assertSingle("swipe-coordinates", "failed");
        Mockito.verify(touch, Mockito.never()).swipeByCoordinates(
                Mockito.anyInt(), Mockito.anyInt(), Mockito.anyInt(), Mockito.anyInt(), Mockito.anyInt());
        TraceEventRecorder.clear();

        Mockito.when(driver.getSessionId()).thenReturn(null);
        Assert.expectThrows(UnsupportedOperationException.class, () -> gestures.tap().at(1, 2));
        assertSingle("tap-at", "failed");
        Mockito.verify(touch, Mockito.never()).tapByCoordinates(1, 2);
    }

    @Test
    public void providerGestureFailureShouldPreserveItsOriginalIdentity() throws Exception {
        AndroidDriver driver = liveDriver("gesture-provider");
        TouchActions touch = Mockito.mock(TouchActions.class, Mockito.RETURNS_SELF);
        IllegalStateException providerFailure = new IllegalStateException("gesture provider failed");
        Mockito.when(touch.tap(By.id("target"))).thenThrow(providerFailure);
        MobileGestureActionsContract gestures = gestures(driver, touch);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> gestures.tap().on(By.id("target")));

        Assert.assertSame(thrown, providerFailure);
        assertSingle("tap", "failed");
    }

    private static AndroidDriver liveDriver(String id) {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        return driver;
    }

    private static MobileGestureActionsContract gestures(AndroidDriver driver, TouchActions touch) throws Exception {
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        Class<?> type = Class.forName("com.shaft.gui.mobile.GestureActions");
        Constructor<?> constructor = type.getDeclaredConstructor(MobileActions.class, TouchActions.class);
        constructor.setAccessible(true);
        return (MobileGestureActionsContract) constructor.newInstance(mobile, touch);
    }

    private static void assertSingle(String operation, String status) {
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "mobile/gesture");
        Assert.assertEquals(events.getFirst().name(), operation);
        Assert.assertEquals(events.getFirst().status(), status);
    }
}
