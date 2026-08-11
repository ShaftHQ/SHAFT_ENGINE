package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileFingerprintActionsContract;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;

public class MobileBiometricNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
        ReportContext.clear();
    }

    @Test
    public void everySupportedBiometricOperationShouldEmitOnePayloadFreeAppiumEvent() {
        AndroidDriver android = Mockito.mock(AndroidDriver.class);
        Mockito.when(android.getSessionId()).thenReturn(new SessionId("android-biometric-trace"));
        new SHAFT.GUI.WebDriver(android).mobile().biometrics().fingerprint().authenticate(7);

        IOSDriver ios = Mockito.mock(IOSDriver.class);
        Mockito.when(ios.getSessionId()).thenReturn(new SessionId("ios-biometric-trace"));
        var touchId = new SHAFT.GUI.WebDriver(ios).mobile().biometrics().touchId();
        touchId.match().reject().enroll().unenroll();

        List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 5);
        Assert.assertEquals(events.stream().map(TraceEventRecorder.ActionEvent::name).toList(), List.of(
                "fingerprint-authenticate", "touch-id-match", "touch-id-reject",
                "touch-id-enroll", "touch-id-unenroll"));
        for (TraceEventRecorder.ActionEvent event : events) {
            Assert.assertEquals(event.category(), "mobile/biometrics");
            Assert.assertEquals(event.status(), "passed");
            Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
            Assert.assertEquals(event.locator(), "<biometric>");
            Assert.assertTrue(event.metadata().isEmpty());
            Assert.assertTrue(event.actionability().isEmpty());
            Assert.assertTrue(event.domSnapshotBefore().isEmpty());
            Assert.assertTrue(event.domSnapshotAfter().isEmpty());
            Assert.assertTrue(event.screenshot().isEmpty());
        }
    }

    @Test
    public void validationAndProviderFailuresShouldEmitOneTruthfulEventAndPreserveIdentity() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("android-biometric-failure-trace"));
        MobileFingerprintActionsContract fingerprint =
                new SHAFT.GUI.WebDriver(driver).mobile().biometrics().fingerprint();

        Assert.expectThrows(IllegalArgumentException.class, () -> fingerprint.authenticate(0));
        assertSingleFailed("fingerprint-authenticate", IllegalArgumentException.class.getName());
        TraceEventRecorder.clear();

        IllegalStateException providerFailure = new IllegalStateException("biometric provider failed");
        Mockito.doThrow(providerFailure).when(driver).fingerPrint(3);
        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> fingerprint.authenticate(3));

        Assert.assertSame(thrown, providerFailure);
        assertSingleFailed("fingerprint-authenticate", IllegalStateException.class.getName());
    }

    private static void assertSingleFailed(String operation, String exceptionType) {
        List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        TraceEventRecorder.ActionEvent event = events.getFirst();
        Assert.assertEquals(event.category(), "mobile/biometrics");
        Assert.assertEquals(event.name(), operation);
        Assert.assertEquals(event.status(), "failed");
        Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
        Assert.assertEquals(event.locator(), "<biometric>");
        Assert.assertEquals(event.exceptionType(), exceptionType);
    }
}
