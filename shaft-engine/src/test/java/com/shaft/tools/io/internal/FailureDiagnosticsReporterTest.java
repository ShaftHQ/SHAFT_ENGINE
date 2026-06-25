package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Attachment;
import io.qameta.allure.model.Status;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public class FailureDiagnosticsReporterTest {

    @Test(description = "Failed tests should attach one diagnostics zip with sanitized diagnostics JSON")
    public void failedTestsShouldAttachDiagnosticsZip() throws Exception {
        try {
            SHAFT.Properties.reporting.set()
                    .diagnosticsBundleEnabled(true)
                    .diagnosticsMaxArtifactMb(1)
                    .traceEnabled(true)
                    .traceMode("failure");
            ReportContext.start(info("failingScenario", failure()));
            ReportContext.setStatus(Status.FAILED);
            int before = attachments().size();

            FailureDiagnosticsReporter.attachOnFailure(
                    info("failingScenario", failure()),
                    "Authorization: Bearer raw-token\ncookie: session=raw-cookie\npassword=raw-password",
                    List.of("target/raw-secret-video.mp4"));

            List<Attachment> added = attachments().subList(before, attachments().size());
            Assert.assertEquals(added.size(), 1);
            Assert.assertEquals(added.getFirst().getName(), "shaft-diagnostics");
            Assert.assertEquals(added.getFirst().getType(), "application/zip");

            String json = unzipDiagnostics(FailureDiagnosticsReporter.renderDiagnosticsZip(
                    FailureDiagnosticsReporter.renderDiagnosticsJson(
                            info("failingScenario", failure()),
                            "Authorization: Bearer raw-token\ncookie: session=raw-cookie\npassword=raw-password",
                            List.of("target/raw-secret-video.mp4"))));
            Assert.assertTrue(json.contains("\"schemaVersion\": 1"), json);
            Assert.assertTrue(json.contains("\"className\": \"customer.LoginTest\""), json);
            Assert.assertTrue(json.contains("\"status\": \"failed\""), json);
            Assert.assertTrue(json.contains("\"topProjectFrame\": \"customer.LoginTest.failingScenario(LoginTest.java:27)\""), json);
            Assert.assertTrue(json.contains("\"path\": \"shaft-trace.zip\""), json);
            Assert.assertTrue(json.contains("\"authorization-header\""), json);
            Assert.assertFalse(json.contains("raw-token"), json);
            Assert.assertFalse(json.contains("raw-cookie"), json);
            Assert.assertFalse(json.contains("raw-password"), json);
        } finally {
            ReportContext.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Skipped tests with throwables should not attach diagnostics")
    public void skippedTestsShouldNotAttachDiagnosticsZip() throws Exception {
        try {
            SHAFT.Properties.reporting.set().diagnosticsBundleEnabled(true);
            ReportContext.start(info("skippedScenario", failure()));
            ReportContext.setStatus(Status.SKIPPED);
            int before = attachments().size();

            FailureDiagnosticsReporter.attachOnFailure(info("skippedScenario", failure()), "skip log", List.of());

            Assert.assertEquals(attachments().size(), before);
        } finally {
            ReportContext.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Diagnostics reporting properties should have failure-safe defaults and setters")
    public void diagnosticsReportingPropertiesShouldHaveDefaultsAndSetters() {
        try {
            Assert.assertTrue(SHAFT.Properties.reporting.diagnosticsBundleEnabled());
            Assert.assertEquals(SHAFT.Properties.reporting.diagnosticsMaxArtifactMb(), 50);

            SHAFT.Properties.reporting.set()
                    .diagnosticsBundleEnabled(false)
                    .diagnosticsMaxArtifactMb(3);

            Assert.assertFalse(SHAFT.Properties.reporting.diagnosticsBundleEnabled());
            Assert.assertEquals(SHAFT.Properties.reporting.diagnosticsMaxArtifactMb(), 3);
        } finally {
            Properties.clearForCurrentThread();
        }
    }

    private static TestExecutionInfo info(String methodName, Throwable throwable) throws Exception {
        Method method = FailureDiagnosticsReporterTest.class.getDeclaredMethod("failingScenario");
        return new TestExecutionInfo("id-" + methodName, "customer.LoginTest", methodName, methodName,
                "diagnostics test", method, throwable, false);
    }

    private static RuntimeException failure() {
        RuntimeException throwable = new RuntimeException("boom password=raw-password");
        throwable.setStackTrace(new StackTraceElement[]{
                new StackTraceElement("customer.LoginTest", "failingScenario", "LoginTest.java", 27),
                new StackTraceElement("com.shaft.listeners.internal.ExecutionLifecycleHelper", "attach", "ExecutionLifecycleHelper.java", 1)
        });
        return throwable;
    }

    @SuppressWarnings("unused")
    private static void failingScenario() {
        throw new UnsupportedOperationException("test marker");
    }

    private static String unzipDiagnostics(byte[] zipBytes) throws Exception {
        try (ZipInputStream zip = new ZipInputStream(new java.io.ByteArrayInputStream(zipBytes))) {
            ZipEntry entry;
            while ((entry = zip.getNextEntry()) != null) {
                if ("diagnostics.json".equals(entry.getName())) {
                    return new String(zip.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
                }
            }
        }
        throw new AssertionError("diagnostics.json was not present in shaft-diagnostics.zip");
    }

    private static List<Attachment> attachments() {
        List<Attachment> attachments = new ArrayList<>();
        Allure.getLifecycle().updateTestCase(result -> attachments.addAll(result.getAttachments()));
        return attachments;
    }
}
