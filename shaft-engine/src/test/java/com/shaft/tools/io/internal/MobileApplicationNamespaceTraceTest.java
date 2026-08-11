package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileApplicationActionsContract;
import com.shaft.listeners.internal.TestExecutionInfo;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.time.Duration;
import java.util.List;

public class MobileApplicationNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
        ReportContext.clear();
    }

    @Test
    public void invalidApplicationInputsShouldEachEmitOneFailedEvent() throws Exception {
        MobileApplicationActionsContract app = liveAndroidApp(Mockito.mock(AndroidDriver.class));

        Assert.expectThrows(IllegalArgumentException.class, () -> app.activate(" "));
        assertSingleFailure("activate");
        TraceEventRecorder.clear();

        Assert.expectThrows(IllegalArgumentException.class,
                () -> app.install(Files.createTempDirectory("shaft-missing-app").resolve("missing.apk")));
        assertSingleFailure("install");
        TraceEventRecorder.clear();

        Assert.expectThrows(IllegalArgumentException.class,
                () -> app.background(Duration.ofSeconds(-1)));
        assertSingleFailure("background");
        TraceEventRecorder.clear();

        Assert.expectThrows(UnsupportedOperationException.class, app::launchConfiguredApp);
        assertSingleFailure("launch-configured");
    }

    @Test
    public void staleApplicationFacadeShouldEmitOneFailedEventWithoutCallingProvider() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("live-then-closed"), null);
        MobileApplicationActionsContract app = new SHAFT.GUI.WebDriver(driver).mobile().app();

        Assert.expectThrows(UnsupportedOperationException.class, () -> app.activate("com.example.app"));

        assertSingleFailure("activate");
        Mockito.verify(driver, Mockito.never()).activateApp(Mockito.anyString());
    }

    @Test
    public void providerFailureShouldNotExposeSubmittedAppSourceAndShouldPreserveIdentity() {
        String source = "https://opaque-token-7821@example.test/private/app.apk?signature=secret-491";
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        IllegalStateException providerFailure = new IllegalStateException("Rejected app source " + source);
        Mockito.doThrow(providerFailure).when(driver).installApp(source);
        MobileApplicationActionsContract app = liveAndroidApp(driver);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> app.install(source));

        Assert.assertSame(thrown, providerFailure);
        assertSingleFailure("install");
        Assert.assertFalse(TraceEventRecorder.snapshot().getFirst().toString().contains(source));
        String report = FailureTraceReporter.renderTraceJson(info(thrown), "provider rejected " + source, List.of());
        Assert.assertFalse(report.contains(source), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
    }

    private static MobileApplicationActionsContract liveAndroidApp(AndroidDriver driver) {
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("android-app-trace"));
        return new SHAFT.GUI.WebDriver(driver).mobile().app();
    }

    private static void assertSingleFailure(String operation) {
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "mobile/app");
        Assert.assertEquals(events.getFirst().name(), operation);
        Assert.assertEquals(events.getFirst().status(), "failed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.APPIUM);
    }

    private static TestExecutionInfo info(Throwable throwable) {
        return new TestExecutionInfo("mobile-app-id", MobileApplicationNamespaceTraceTest.class.getName(),
                "providerFailure", "providerFailure", "mobile application trace", null,
                throwable, false);
    }
}
