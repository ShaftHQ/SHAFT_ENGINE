package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileClipboardActionsContract;
import com.shaft.gui.driver.MobileDeviceActionsContract;
import com.shaft.listeners.internal.TestExecutionInfo;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.time.Duration;
import java.util.List;

public class MobileDeviceNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
        ReportContext.clear();
    }

    @Test
    public void invalidDeviceInputsShouldEachEmitOneFailedEvent() {
        MobileDeviceActionsContract device = liveDevice(Mockito.mock(AndroidDriver.class));

        Assert.expectThrows(NullPointerException.class, () -> device.lock(null));
        assertSingleFailure("lock");
        TraceEventRecorder.clear();

        Assert.expectThrows(IllegalArgumentException.class, () -> device.lock(Duration.ofSeconds(-1)));
        assertSingleFailure("lock");
        TraceEventRecorder.clear();

        Assert.expectThrows(NullPointerException.class, () -> device.orientation(null));
        assertSingleFailure("orientation");
        TraceEventRecorder.clear();

        Assert.expectThrows(IllegalArgumentException.class, () -> device.time(" "));
        assertSingleFailure("time");
        TraceEventRecorder.clear();

        Assert.expectThrows(NullPointerException.class, () -> device.clipboard().text(null));
        assertSingleFailure("clipboard-set-text");
        Assert.assertTrue(TraceEventRecorder.snapshot().getFirst().exceptionMessage().contains("clipboard text"));
    }

    @Test
    public void clipboardProviderFailureShouldPreserveIdentityWithoutExposingSubmittedText() {
        String secret = "opaque clipboard value 7821";
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        IllegalStateException providerFailure = new IllegalStateException("Rejected clipboard: " + secret);
        Mockito.doThrow(providerFailure).when(driver).setClipboardText(secret);
        MobileClipboardActionsContract clipboard = liveDevice(driver).clipboard();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> clipboard.text(secret));

        Assert.assertSame(thrown, providerFailure);
        assertSingleFailure("clipboard-set-text");
        String report = FailureTraceReporter.renderTraceJson(info(thrown), "provider rejected " + secret, List.of());
        Assert.assertFalse(report.contains(secret), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
    }

    @Test
    public void clipboardReadFailureShouldOmitProviderTextThatMayEchoExistingClipboardData() {
        String secret = "existing clipboard value 219";
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        IllegalStateException providerFailure = new IllegalStateException("Cannot read clipboard containing " + secret);
        Mockito.when(driver.getClipboardText()).thenThrow(providerFailure);
        MobileClipboardActionsContract clipboard = liveDevice(driver).clipboard();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, clipboard::text);

        Assert.assertSame(thrown, providerFailure);
        assertSingleFailure("clipboard-text");
        String report = FailureTraceReporter.renderTraceJson(info(thrown), providerFailure.getMessage(), List.of());
        Assert.assertFalse(report.contains(secret), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
    }

    @Test
    public void clipboardQueryShouldReturnTextWithoutPublishingItInActionMetadata() {
        String secret = "returned clipboard value 491";
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getClipboardText()).thenReturn(secret);
        MobileClipboardActionsContract clipboard = liveDevice(driver).clipboard();

        Assert.assertEquals(clipboard.text(), secret);

        var event = TraceEventRecorder.snapshot().getFirst();
        Assert.assertEquals(event.status(), "passed");
        Assert.assertFalse(event.toString().contains(secret));
        String report = FailureTraceReporter.renderTraceJson(info(new AssertionError("later " + secret)),
                "later " + secret, List.of());
        Assert.assertFalse(report.contains(secret), report);
    }

    @Test
    public void unsupportedAndStaleDeviceOperationsShouldFailOnceBeforeProviderMutation() {
        AppiumDriver generic = Mockito.mock(AppiumDriver.class);
        Mockito.when(generic.getSessionId()).thenReturn(new SessionId("generic-device"));
        MobileDeviceActionsContract unsupported = new SHAFT.GUI.WebDriver(generic).mobile().device();

        Assert.expectThrows(UnsupportedOperationException.class, unsupported::lock);
        assertSingleFailure("lock");
        TraceEventRecorder.clear();

        AndroidDriver staleDriver = Mockito.mock(AndroidDriver.class);
        Mockito.when(staleDriver.getSessionId()).thenReturn(new SessionId("stale-device"), (SessionId) null);
        MobileDeviceActionsContract stale = new SHAFT.GUI.WebDriver(staleDriver).mobile().device();

        Assert.expectThrows(UnsupportedOperationException.class, stale::unlock);
        assertSingleFailure("unlock");
        Mockito.verify(staleDriver, Mockito.never()).unlockDevice();
    }

    @Test
    public void unsupportedClipboardPreflightShouldRetainItsSafeDiagnostic() {
        AppiumDriver generic = Mockito.mock(AppiumDriver.class);
        Mockito.when(generic.getSessionId()).thenReturn(new SessionId("generic-clipboard"));
        MobileClipboardActionsContract clipboard = new SHAFT.GUI.WebDriver(generic).mobile().device().clipboard();

        Assert.expectThrows(UnsupportedOperationException.class, clipboard::text);

        assertSingleFailure("clipboard-text");
        Assert.assertTrue(TraceEventRecorder.snapshot().getFirst().exceptionMessage()
                .contains("does not support text clipboard"));
    }

    @Test
    public void traceRenderingWithoutExecutionInfoShouldKeepTheDiagnosticTimeline() {
        String report = FailureTraceReporter.renderTraceJson(null, "diagnostic log", List.of());

        Assert.assertTrue(report.contains("diagnostic log"), report);
        Assert.assertTrue(report.contains("\"timeline\""), report);
    }

    private static MobileDeviceActionsContract liveDevice(AndroidDriver driver) {
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("android-device-trace"));
        return new SHAFT.GUI.WebDriver(driver).mobile().device();
    }

    private static void assertSingleFailure(String operation) {
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "mobile/device");
        Assert.assertEquals(events.getFirst().name(), operation);
        Assert.assertEquals(events.getFirst().status(), "failed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.APPIUM);
    }

    private static TestExecutionInfo info(Throwable throwable) {
        return new TestExecutionInfo("mobile-device-id", MobileDeviceNamespaceTraceTest.class.getName(),
                "deviceTrace", "deviceTrace", "mobile device trace", null, throwable, false);
    }
}
