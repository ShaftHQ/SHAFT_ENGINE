package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileEvidenceBundle;
import com.shaft.listeners.internal.TestExecutionInfo;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.remote.SupportsContextSwitching;
import org.mockito.Mockito;
import org.openqa.selenium.ImmutableCapabilities;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

public class MobileEvidenceNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
        ReportContext.clear();
    }

    @Test
    public void captureShouldEmitOnePayloadAndPathFreeCountOnlyAppiumEvent() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean source = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("private-evidence-trace-");
        Path target = directory.resolve("private-bundle-name.zip");
        try {
            SHAFT.Properties.reporting.set().traceIncludeScreenshots(false).traceIncludeNativePageSource(false);
            MobileEvidenceBundle bundle = new SHAFT.GUI.WebDriver(liveDriver("evidence-trace"))
                    .mobile().evidence().capture(target);

            List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
            Assert.assertEquals(events.size(), 1);
            TraceEventRecorder.ActionEvent event = events.getFirst();
            Assert.assertEquals(event.category(), "mobile/evidence");
            Assert.assertEquals(event.name(), "capture");
            Assert.assertEquals(event.status(), "passed");
            Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
            Assert.assertEquals(event.locator(), "<evidence-bundle>");
            Assert.assertEquals(event.metadata(), Map.of(
                    "artifactCount", Integer.toString(bundle.artifacts().size()),
                    "omissionCount", Integer.toString(bundle.omissions().size()),
                    "logMessageCount", Integer.toString(bundle.logMessages().size()),
                    "logErrorCount", Integer.toString(bundle.logErrors().size()),
                    "performanceSampleCount", Integer.toString(bundle.performanceSamples().size())));
            Assert.assertTrue(event.domSnapshotBefore().isEmpty());
            Assert.assertTrue(event.domSnapshotAfter().isEmpty());
            Assert.assertTrue(event.screenshot().isEmpty());
            Assert.assertTrue(event.attachments().isEmpty());
            Assert.assertTrue(event.url().isEmpty());
            Assert.assertTrue(event.actionability().isEmpty());
            Assert.assertTrue(event.exceptionType().isEmpty());
            Assert.assertTrue(event.exceptionMessage().isEmpty());
            Assert.assertFalse(event.toString().contains(target.toString()));
            Assert.assertFalse(event.toString().contains("private-bundle-name"));
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots).traceIncludeNativePageSource(source);
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void failedCaptureShouldPreserveIdentityAndRedactTargetFromLaterReports() throws Exception {
        Path directory = Files.createTempDirectory("private-evidence-failure-");
        Path target = directory.resolve("private-failure-bundle.zip");
        String privatePath = target.toString();
        try {
            AppiumDriver driver = liveDriver("evidence-failure");
            RuntimeException sentinel = new RuntimeException("provider rejected " + privatePath);
            SHAFT.GUI.WebDriver facade = new SHAFT.GUI.WebDriver(driver);
            var evidence = facade.mobile().evidence();
            Mockito.when(driver.getSessionId()).thenThrow(sentinel);
            RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                    () -> evidence.capture(target));
            Assert.assertSame(thrown, sentinel);

            List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
            Assert.assertEquals(events.size(), 1);
            TraceEventRecorder.ActionEvent event = events.getFirst();
            Assert.assertEquals(event.category(), "mobile/evidence");
            Assert.assertEquals(event.name(), "capture");
            Assert.assertEquals(event.status(), "failed");
            Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
            Assert.assertEquals(event.locator(), "<evidence-bundle>");
            Assert.assertEquals(event.exceptionType(), RuntimeException.class.getName());
            Assert.assertTrue(event.metadata().isEmpty());
            Assert.assertTrue(event.attachments().isEmpty());
            Assert.assertTrue(event.url().isEmpty());
            Assert.assertTrue(event.actionability().isEmpty());
            Assert.assertTrue(event.domSnapshotBefore().isEmpty());
            Assert.assertTrue(event.domSnapshotAfter().isEmpty());
            Assert.assertTrue(event.screenshot().isEmpty());
            String report = FailureTraceReporter.renderTraceJson(info(thrown),
                    "later echo " + privatePath, List.of("attachment " + privatePath));
            Assert.assertFalse(report.contains(privatePath), report);
            Assert.assertFalse(report.contains(privatePath.replace("\\", "\\\\")), report);
        } finally {
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    private static AppiumDriver liveDriver(String id) {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class, Mockito.withSettings()
                .extraInterfaces(SupportsContextSwitching.class).defaultAnswer(Mockito.RETURNS_DEEP_STUBS));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        Mockito.when(driver.getCapabilities()).thenReturn(new ImmutableCapabilities("platformName", "Android"));
        Mockito.when(((SupportsContextSwitching) driver).getContext()).thenReturn("NATIVE_APP", "NATIVE_APP");
        return driver;
    }

    private static TestExecutionInfo info(Throwable throwable) {
        return new TestExecutionInfo("mobile-evidence-id", MobileEvidenceNamespaceTraceTest.class.getName(),
                "failedCapture", "failedCapture", "mobile evidence trace", null, throwable, false);
    }
}
