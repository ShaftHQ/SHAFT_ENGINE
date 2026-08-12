package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileRecordingActionsContract;
import com.shaft.gui.driver.MobileRecordingOptions;
import com.shaft.listeners.internal.TestExecutionInfo;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.screenrecording.BaseStartScreenRecordingOptions;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Base64;
import java.util.List;
import java.util.Map;

public class MobileRecordingNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
        ReportContext.clear();
    }

    @Test
    public void everyOperationShouldEmitOnePayloadAndPathFreeAppiumEvent() throws Exception {
        byte[] privateMedia = "private-media-payload-4740".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        String encoded = Base64.getEncoder().encodeToString(privateMedia);
        Path privateTarget = Files.createTempDirectory("private-recording-target-4740")
                .resolve("private-name-4740.mp4");
        AndroidDriver driver = liveDriver("recording-trace");
        Mockito.when(driver.stopRecordingScreen()).thenReturn(encoded);
        MobileRecordingActionsContract recording = new SHAFT.GUI.WebDriver(driver).mobile().recording();

        recording.start(new MobileRecordingOptions(Duration.ofSeconds(12), 1024));
        recording.stop();
        recording.start();
        recording.stopAndSave(privateTarget);

        List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.stream().map(TraceEventRecorder.ActionEvent::name).toList(),
                List.of("start", "stop", "start", "stop-and-save"));
        Assert.assertEquals(events.get(0).metadata(), Map.of("configuredSeconds", "12"));
        Assert.assertEquals(events.get(1).metadata(), Map.of("decodedBytes", Integer.toString(privateMedia.length)));
        Assert.assertEquals(events.get(2).metadata(), Map.of("configuredSeconds", "180"));
        Assert.assertEquals(events.get(3).metadata(), Map.of("decodedBytes", Integer.toString(privateMedia.length)));
        for (TraceEventRecorder.ActionEvent event : events) {
            Assert.assertEquals(event.category(), "mobile/recording");
            Assert.assertEquals(event.status(), "passed");
            Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
            Assert.assertEquals(event.locator(), "<screen-recording>");
            Assert.assertTrue(event.domSnapshotBefore().isEmpty());
            Assert.assertTrue(event.domSnapshotAfter().isEmpty());
            Assert.assertTrue(event.screenshot().isEmpty());
            Assert.assertFalse(event.toString().contains(encoded));
            Assert.assertFalse(event.toString().contains(privateTarget.toString()));
        }
        IllegalStateException laterFailure = new IllegalStateException("Later echo " + privateTarget);
        String laterReport = FailureTraceReporter.renderTraceJson(
                info(laterFailure), laterFailure.getMessage(), List.of());
        Assert.assertFalse(laterReport.contains(privateTarget.toString()), laterReport);
    }

    @Test
    public void providerFailureShouldPreserveIdentityAndRedactTargetAndThrowablePayload() throws Exception {
        Path privateTarget = Files.createTempDirectory("private-recording-failure-4740")
                .resolve("private-target-4740.mp4");
        String privatePayload = "private-provider-recording-payload-4740";
        AndroidDriver driver = liveDriver("recording-provider-failure");
        RuntimeException providerFailure = new RuntimeException(
                "Rejected " + privateTarget + " " + privatePayload);
        Mockito.when(driver.stopRecordingScreen()).thenThrow(providerFailure);
        MobileRecordingActionsContract recording = new SHAFT.GUI.WebDriver(driver).mobile().recording();
        recording.start();
        TraceEventRecorder.clear();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> recording.stopAndSave(privateTarget));

        Assert.assertSame(thrown, providerFailure);
        List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        TraceEventRecorder.ActionEvent event = events.getFirst();
        Assert.assertEquals(event.category(), "mobile/recording");
        Assert.assertEquals(event.name(), "stop-and-save");
        Assert.assertEquals(event.status(), "failed");
        Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
        Assert.assertEquals(event.locator(), "<screen-recording>");
        Assert.assertEquals(event.exceptionType(), RuntimeException.class.getName());
        String report = FailureTraceReporter.renderTraceJson(info(thrown), thrown.getMessage(), List.of());
        Assert.assertFalse(report.contains(privateTarget.toString()), report);
        Assert.assertFalse(report.contains(privatePayload), report);
        Assert.assertTrue(report.contains(RuntimeException.class.getName()), report);
    }

    private static AndroidDriver liveDriver(String id) {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        Mockito.when(driver.startRecordingScreen(Mockito.any(BaseStartScreenRecordingOptions.class)))
                .thenReturn("");
        return driver;
    }

    private static TestExecutionInfo info(Throwable throwable) {
        return new TestExecutionInfo("mobile-recording-id", MobileRecordingNamespaceTraceTest.class.getName(),
                "providerFailure", "providerFailure", "mobile recording trace", null, throwable, false);
    }
}
