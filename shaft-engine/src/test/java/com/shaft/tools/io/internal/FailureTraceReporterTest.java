package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Attachment;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class FailureTraceReporterTest {

    @Test(description = "Failure mode should attach trace artifacts only for failed tests")
    public void failureModeShouldAttachTraceArtifactsOnlyForFailures() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            int beforePassing = attachments().size();

            FailureTraceReporter.attachOnFailure(info("passingScenario", null), "passing log", List.of());

            Assert.assertEquals(attachments().size(), beforePassing,
                    "Passing tests must not attach trace artifacts in failure mode.");

            FailureTraceReporter.attachOnFailure(info("failingScenario", failure()), "token=raw-secret", List.of());

            List<Attachment> added = attachments().subList(beforePassing, attachments().size());
            Assert.assertTrue(added.stream().anyMatch(attachment -> "SHAFT Trace Report".equals(attachment.getName())
                    && "text/html".equals(attachment.getType())));
            Assert.assertTrue(added.stream().anyMatch(attachment -> "shaft-trace".equals(attachment.getName())
                    && "application/json".equals(attachment.getType())));
            Assert.assertTrue(added.stream().anyMatch(attachment -> "shaft-trace".equals(attachment.getName())
                    && "application/zip".equals(attachment.getType())));
        } finally {
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Rendered trace JSON should redact sensitive values and keep source fallback frame")
    public void traceJsonShouldRedactSecretsAndKeepFallbackFrame() throws Exception {
        RuntimeException throwable = failure();
        String json = FailureTraceReporter.renderTraceJson(
                info("failingScenario", throwable),
                "Authorization: Bearer raw-token\ncookie: session=raw-cookie\npassword=raw-password",
                List.of("C:\\secret\\video.mp4"));

        Assert.assertTrue(json.contains("customer.LoginTest.failingScenario(LoginTest.java:27)"));
        Assert.assertTrue(json.contains("********"));
        Assert.assertFalse(json.contains("raw-token"));
        Assert.assertFalse(json.contains("raw-cookie"));
        Assert.assertFalse(json.contains("raw-password"));
    }

    @Test(description = "Trace reporting properties should have failure-safe defaults and setters")
    public void traceReportingPropertiesShouldHaveDefaultsAndSetters() {
        try {
            Assert.assertTrue(SHAFT.Properties.reporting.traceEnabled());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMode(), "failure");
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeCodeContext());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeFullPageSnapshots());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeNativePageSource());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeNetwork());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeConsole());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMaxArtifactMb(), 50);

            SHAFT.Properties.reporting.set()
                    .traceEnabled(false)
                    .traceMode("always")
                    .traceIncludeCodeContext(false)
                    .traceIncludeFullPageSnapshots(false)
                    .traceIncludeNativePageSource(false)
                    .traceIncludeNetwork(false)
                    .traceIncludeConsole(false)
                    .traceMaxArtifactMb(7);

            Assert.assertFalse(SHAFT.Properties.reporting.traceEnabled());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMode(), "always");
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeCodeContext());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeFullPageSnapshots());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeNativePageSource());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeNetwork());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeConsole());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMaxArtifactMb(), 7);
        } finally {
            Properties.clearForCurrentThread();
        }
    }

    private static TestExecutionInfo info(String methodName, Throwable throwable) throws Exception {
        Method method = FailureTraceReporterTest.class.getDeclaredMethod("failingScenario");
        return new TestExecutionInfo("id-" + methodName, "customer.LoginTest", methodName, methodName,
                "trace test", method, throwable, false);
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

    private static List<Attachment> attachments() {
        List<Attachment> attachments = new ArrayList<>();
        Allure.getLifecycle().updateTestCase(result -> attachments.addAll(result.getAttachments()));
        return attachments;
    }
}
