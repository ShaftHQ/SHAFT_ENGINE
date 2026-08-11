package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileFileActionsContract;
import com.shaft.listeners.internal.TestExecutionInfo;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import testPackage.fixtures.MobileFileSourceContextFixture;

import java.util.List;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public class MobileFileNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
        ReportContext.clear();
    }

    @Test
    public void invalidAndStaleFileActionsShouldEmitOneFailedOwnerEvent() {
        AndroidDriver driver = liveDriver("file-validation");
        MobileFileActionsContract files = new SHAFT.GUI.WebDriver(driver).mobile().files();

        Assert.expectThrows(IllegalArgumentException.class, () -> files.pull(" "));
        assertSingle("pull", "failed");
        Mockito.verify(driver, Mockito.never()).pullFile(Mockito.anyString());
        TraceEventRecorder.clear();

        Mockito.when(driver.getSessionId()).thenReturn(null);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> files.push("/device/source", new byte[]{1}));
        assertSingle("push", "failed");
        Mockito.verify(driver, Mockito.never()).pushFile(Mockito.anyString(), Mockito.any(byte[].class));
    }

    @Test
    public void submittedTextProviderFailureShouldRemainOriginalAndStayOutOfTheRenderedTrace() {
        String submitted = "opaque-file-content-7821";
        AndroidDriver driver = liveDriver("file-provider");
        IllegalStateException providerFailure = new IllegalStateException("Rejected " + submitted);
        Mockito.doThrow(providerFailure).when(driver).pushFile(
                Mockito.eq("/device/message.txt"), Mockito.any(byte[].class));
        MobileFileActionsContract files = new SHAFT.GUI.WebDriver(driver).mobile().files();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> files.pushText("/device/message.txt", submitted));

        Assert.assertSame(thrown, providerFailure);
        assertSingle("push-text", "failed");
        String report = FailureTraceReporter.renderTraceJson(info(thrown), "provider rejected " + submitted, List.of());
        Assert.assertFalse(report.contains(submitted), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
    }

    @Test
    public void pulledTextShouldHaveOneSemanticEventWithoutContentEvidence() {
        String pulled = "private-pulled-content-491";
        AndroidDriver driver = liveDriver("file-pull-text");
        Mockito.when(driver.pullFile("/device/private.txt"))
                .thenReturn(pulled.getBytes(StandardCharsets.UTF_8));
        MobileFileActionsContract files = new SHAFT.GUI.WebDriver(driver).mobile().files();

        Assert.assertEquals(files.pullText("/device/private.txt"), pulled);

        assertSingle("pull-text", "passed");
        Assert.assertFalse(TraceEventRecorder.snapshot().getFirst().toString().contains(pulled));
    }

    @Test
    public void providerAndLocalPublicationFailuresShouldNotExposePaths() throws Exception {
        String devicePath = "/private/opaque-user-7821/token.txt";
        AndroidDriver driver = liveDriver("file-path-secrecy");
        IllegalStateException providerFailure = new IllegalStateException("Rejected " + devicePath);
        Mockito.when(driver.pullFile(devicePath)).thenThrow(providerFailure);
        MobileFileActionsContract files = new SHAFT.GUI.WebDriver(driver).mobile().files();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> files.pull(devicePath));

        Assert.assertSame(thrown, providerFailure);
        String providerReport = FailureTraceReporter.renderTraceJson(info(thrown), "rejected " + devicePath, List.of());
        Assert.assertFalse(providerReport.contains(devicePath), providerReport);
        TraceEventRecorder.clear();
        ReportContext.clear();

        Path directory = Files.createTempDirectory("shaft-private-pull-7821-");
        Path blockingParent = directory.resolve("opaque-local-parent-491");
        Files.writeString(blockingParent, "file", StandardCharsets.UTF_8);
        Path target = blockingParent.resolve("result.txt");
        Mockito.when(driver.pullFile("/device/result.txt")).thenReturn(new byte[]{1});
        try {
            RuntimeException publicationFailure = Assert.expectThrows(RuntimeException.class,
                    () -> files.pullTo("/device/result.txt", target));
            String localReport = FailureTraceReporter.renderTraceJson(
                    info(publicationFailure), "could not publish " + target, List.of());
            Assert.assertFalse(localReport.contains(target.toAbsolutePath().normalize().toString()), localReport);
            Assert.assertFalse(localReport.contains(blockingParent.toString()), localReport);
        } finally {
            Files.deleteIfExists(blockingParent);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void validationFailureShouldRetainItsSafeDiagnostic() {
        AndroidDriver driver = liveDriver("file-validation-diagnostic");
        MobileFileActionsContract files = new SHAFT.GUI.WebDriver(driver).mobile().files();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> files.pushText(" ", "submitted-text"));

        String report = FailureTraceReporter.renderTraceJson(info(thrown), thrown.getMessage(), List.of());
        Assert.assertTrue(report.contains("device file path must not be blank"), report);
        Mockito.verify(driver, Mockito.never()).pushFile(Mockito.anyString(), Mockito.any(byte[].class));
    }

    @Test
    public void earlyFailuresShouldRemoveSubmittedTextAndPathsFromCodeContext() {
        String submittedText = String.join("", "opaque-source", "-text-9941");
        String remotePath = String.join("", "/private/source", "-path-6724");
        String localPath = String.join("", "opaque-local", "-source-8181");
        boolean originalCodeContext = SHAFT.Properties.reporting.traceIncludeCodeContext();
        try {
            SHAFT.Properties.reporting.set().traceIncludeCodeContext(true);
            AndroidDriver pushDriver = liveDriver("file-source-validation");
            MobileFileActionsContract pushFiles = new SHAFT.GUI.WebDriver(pushDriver).mobile().files();

            RuntimeException validationFailure = Assert.expectThrows(RuntimeException.class,
                    () -> MobileFileSourceContextFixture.invalidPushText(pushFiles));
            String validationReport = FailureTraceReporter.renderTraceJson(
                    info(validationFailure), validationFailure.getMessage(), List.of());
            Assert.assertFalse(validationReport.contains(submittedText), validationReport);
            Assert.assertTrue(validationReport.contains("device file path must not be blank"), validationReport);
            TraceEventRecorder.clear();
            ReportContext.clear();

            AppiumDriver unsupportedDriver = liveAppiumDriver("file-source-unsupported");
            MobileFileActionsContract unsupportedFiles = new SHAFT.GUI.WebDriver(unsupportedDriver).mobile().files();
            RuntimeException unsupportedFailure = Assert.expectThrows(RuntimeException.class,
                    () -> MobileFileSourceContextFixture.unsupportedPull(unsupportedFiles));
            String unsupportedReport = FailureTraceReporter.renderTraceJson(
                    info(unsupportedFailure), unsupportedFailure.getMessage(), List.of());
            Assert.assertFalse(unsupportedReport.contains(remotePath), unsupportedReport);
            Assert.assertTrue(unsupportedReport.contains("does not support pulling files"), unsupportedReport);
            TraceEventRecorder.clear();
            ReportContext.clear();

            RuntimeException localFailure = Assert.expectThrows(RuntimeException.class,
                    () -> MobileFileSourceContextFixture.invalidLocalSource(pushFiles));
            String localReport = FailureTraceReporter.renderTraceJson(
                    info(localFailure), localFailure.getMessage(), List.of());
            Assert.assertFalse(localReport.contains(localPath), localReport);
            Assert.assertTrue(localReport.contains("local source must be a regular file"), localReport);
        } finally {
            SHAFT.Properties.reporting.set().traceIncludeCodeContext(originalCodeContext);
        }
    }

    private static AndroidDriver liveDriver(String id) {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        return driver;
    }

    private static AppiumDriver liveAppiumDriver(String id) {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        return driver;
    }

    private static void assertSingle(String operation, String status) {
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "mobile/files");
        Assert.assertEquals(events.getFirst().name(), operation);
        Assert.assertEquals(events.getFirst().status(), status);
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.APPIUM);
        Assert.assertEquals(events.getFirst().locator(), "<device-file>");
    }

    private static TestExecutionInfo info(Throwable throwable) {
        return new TestExecutionInfo("mobile-file-id", MobileFileNamespaceTraceTest.class.getName(),
                "providerFailure", "providerFailure", "mobile file trace", null,
                throwable, false);
    }
}
