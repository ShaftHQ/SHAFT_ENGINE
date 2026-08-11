package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobilePerformanceActionsContract;
import com.shaft.listeners.internal.TestExecutionInfo;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

public class MobilePerformanceNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
        ReportContext.clear();
    }

    @Test
    public void everyOperationShouldEmitOnePayloadFreeAppiumEvent() {
        String applicationId = "private.performance.app.7281";
        String dataType = "private-cpu-type-491";
        String supportedOnlyType = "private-supported-only-type-673";
        String payload = "private-performance-value-881";
        long numericPayload = 947362815L;
        AndroidDriver driver = liveDriver("performance-trace");
        Mockito.when(driver.getSupportedPerformanceDataTypes()).thenReturn(List.of(supportedOnlyType));
        Mockito.when(driver.getPerformanceData(applicationId, dataType, 5)).thenReturn(table(
                row("value", "numeric"), row(payload, numericPayload)));
        MobilePerformanceActionsContract performance = new SHAFT.GUI.WebDriver(driver).mobile().performance();

        performance.supportedTypes();
        performance.sample(applicationId, dataType);
        performance.history();
        performance.clear();

        List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.stream().map(TraceEventRecorder.ActionEvent::name).toList(),
                List.of("supported-types", "sample", "history", "clear"));
        Assert.assertEquals(events.get(0).metadata(), java.util.Map.of("typeCount", "1"));
        Assert.assertEquals(events.get(1).metadata(), java.util.Map.of("columnCount", "2", "rowCount", "1"));
        Assert.assertEquals(events.get(2).metadata(), java.util.Map.of("sampleCount", "1"));
        Assert.assertEquals(events.get(3).metadata(), java.util.Map.of("clearedCount", "1"));
        for (TraceEventRecorder.ActionEvent event : events) {
            Assert.assertEquals(event.category(), "mobile/performance");
            Assert.assertEquals(event.status(), "passed");
            Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
            Assert.assertEquals(event.locator(), "<performance-data>");
            Assert.assertTrue(event.domSnapshotBefore().isEmpty());
            Assert.assertTrue(event.domSnapshotAfter().isEmpty());
            Assert.assertTrue(event.screenshot().isEmpty());
            String rendered = event.toString();
            Assert.assertFalse(rendered.contains(applicationId), rendered);
            Assert.assertFalse(rendered.contains(dataType), rendered);
            Assert.assertFalse(rendered.contains(payload), rendered);
        }
        IllegalStateException laterFailure = new IllegalStateException(
                "Later echo " + applicationId + " " + dataType + " " + supportedOnlyType
                        + " " + payload + " " + numericPayload);
        String laterReport = FailureTraceReporter.renderTraceJson(
                info(laterFailure), laterFailure.getMessage(), List.of());
        Assert.assertFalse(laterReport.contains(applicationId), laterReport);
        Assert.assertFalse(laterReport.contains(dataType), laterReport);
        Assert.assertFalse(laterReport.contains(supportedOnlyType), laterReport);
        Assert.assertFalse(laterReport.contains(payload), laterReport);
        Assert.assertFalse(laterReport.contains(Long.toString(numericPayload)), laterReport);
    }

    @Test
    public void providerFailureShouldPreserveIdentityAndRedactInputsAndThrowablePayload() {
        String applicationId = "private.failed.app.552";
        String dataType = "private-failed-type-914";
        String payload = "private-provider-payload-331";
        AndroidDriver driver = liveDriver("performance-provider-failure");
        IllegalStateException providerFailure = new IllegalStateException(
                "Rejected " + applicationId + " " + dataType + " " + payload);
        Mockito.when(driver.getPerformanceData(applicationId, dataType, 5)).thenThrow(providerFailure);
        MobilePerformanceActionsContract performance = new SHAFT.GUI.WebDriver(driver).mobile().performance();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> performance.sample(applicationId, dataType));

        Assert.assertSame(thrown, providerFailure);
        assertSingleFailed("sample", IllegalStateException.class.getName());
        String report = FailureTraceReporter.renderTraceJson(info(thrown), thrown.getMessage(), List.of());
        Assert.assertFalse(report.contains(applicationId), report);
        Assert.assertFalse(report.contains(dataType), report);
        Assert.assertFalse(report.contains(payload), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
    }

    @Test
    public void validationAndStaleFailuresShouldEmitOneOwnerEventWithoutProviderCalls() {
        AndroidDriver driver = liveDriver("performance-validation-trace");
        MobilePerformanceActionsContract performance = new SHAFT.GUI.WebDriver(driver).mobile().performance();

        Assert.expectThrows(IllegalArgumentException.class, () -> performance.sample(" ", "cpuinfo"));
        assertSingleFailed("sample", IllegalArgumentException.class.getName());
        Mockito.verify(driver, Mockito.never()).getPerformanceData(
                Mockito.anyString(), Mockito.anyString(), Mockito.anyInt());
        TraceEventRecorder.clear();

        Mockito.when(driver.getSessionId()).thenReturn(null);
        Assert.expectThrows(UnsupportedOperationException.class, performance::history);
        assertSingleFailed("history", UnsupportedOperationException.class.getName());
    }

    @Test
    public void publicClearShouldReportTheExactCountAcrossAConcurrentAppend() throws Exception {
        AndroidDriver driver = liveDriver("public-atomic-clear");
        Mockito.when(driver.getPerformanceData("atomic.app", "cpuinfo", 5))
                .thenReturn(table(row("value"), row(1)));
        MobilePerformanceActionsContract performance = new SHAFT.GUI.WebDriver(driver).mobile().performance();
        performance.sample("atomic.app", "cpuinfo");
        TraceEventRecorder.clear();
        Thread clearingThread = Thread.currentThread();
        AtomicInteger clearingThreadLivenessCalls = new AtomicInteger();
        CountDownLatch splitCallGap = new CountDownLatch(1);
        CountDownLatch concurrentAppendFinished = new CountDownLatch(1);
        SessionId live = new SessionId("public-atomic-clear");
        Mockito.doAnswer(ignored -> {
            if (Thread.currentThread() == clearingThread
                    && clearingThreadLivenessCalls.incrementAndGet() == 2) {
                splitCallGap.countDown();
                if (!concurrentAppendFinished.await(10, TimeUnit.SECONDS)) {
                    throw new IllegalStateException("concurrent append timed out");
                }
            }
            return live;
        }).when(driver).getSessionId();

        try (var executor = Executors.newSingleThreadExecutor()) {
            Future<?> appended = executor.submit(() -> {
                splitCallGap.await(300, TimeUnit.MILLISECONDS);
                try {
                    return performance.sample("atomic.app", "cpuinfo");
                } finally {
                    concurrentAppendFinished.countDown();
                }
            });
            performance.clear();
            appended.get(10, TimeUnit.SECONDS);
        }

        TraceEventRecorder.ActionEvent clearEvent = TraceEventRecorder.snapshot().getFirst();
        int clearedCount = Integer.parseInt(clearEvent.metadata().get("clearedCount"));
        Assert.assertEquals(clearedCount + performance.history().size(), 2);
    }

    private static AndroidDriver liveDriver(String id) {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        return driver;
    }

    @SafeVarargs
    private static List<List<Object>> table(List<Object>... rows) {
        return List.of(rows);
    }

    private static List<Object> row(Object... values) {
        return List.of(values);
    }

    private static void assertSingleFailed(String operation, String exceptionType) {
        List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        TraceEventRecorder.ActionEvent event = events.getFirst();
        Assert.assertEquals(event.category(), "mobile/performance");
        Assert.assertEquals(event.name(), operation);
        Assert.assertEquals(event.status(), "failed");
        Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
        Assert.assertEquals(event.locator(), "<performance-data>");
        Assert.assertEquals(event.exceptionType(), exceptionType);
    }

    private static TestExecutionInfo info(Throwable throwable) {
        return new TestExecutionInfo("mobile-performance-id", MobilePerformanceNamespaceTraceTest.class.getName(),
                "providerFailure", "providerFailure", "mobile performance trace", null, throwable, false);
    }
}
