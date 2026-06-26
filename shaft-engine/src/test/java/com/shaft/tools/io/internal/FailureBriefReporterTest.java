package com.shaft.tools.io.internal;

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

public class FailureBriefReporterTest {

    @Test(description = "Failed tests should attach a redacted HTML brief, JSON brief, and attachment manifest")
    public void failedTestsShouldAttachFailureBriefAndManifest() throws Exception {
        try {
            TestExecutionInfo info = info("failingScenario", assertionFailure());
            ReportContext.start(info);
            ReportContext.setStatus(Status.FAILED);
            ReportContext.recordAttachment("Screenshot - failed Assert", "image/png", ".png", "screenshot", 100);
            ReportContext.recordAttachment("SHAFT Trace Report.html", "text/html", ".html", "trace-or-bundle", 200);
            int before = attachments().size();

            FailureBriefReporter.attachOnFailure(
                    info,
                    "Authorization: Bearer raw-token\npassword=raw-password",
                    List.of("target/test_actions.gif"));

            List<Attachment> added = attachments().subList(before, attachments().size());
            Assert.assertEquals(added.size(), 3);
            Assert.assertTrue(added.stream().anyMatch(attachment ->
                    "SHAFT Failure Brief.html".equals(attachment.getName())
                            && "text/html".equals(attachment.getType())));
            Assert.assertTrue(added.stream().anyMatch(attachment ->
                    "shaft-failure-brief.json".equals(attachment.getName())
                            && "application/json".equals(attachment.getType())));
            Assert.assertTrue(added.stream().anyMatch(attachment ->
                    "shaft-attachments-manifest.json".equals(attachment.getName())
                            && "application/json".equals(attachment.getType())));

            String json = FailureBriefReporter.renderBriefJson(
                    info,
                    "Authorization: Bearer raw-token\npassword=raw-password",
                    ReportContext.snapshotAttachments());
            Assert.assertTrue(json.contains("\"schemaVersion\": 1"), json);
            Assert.assertTrue(json.contains("\"status\": \"failed\""), json);
            Assert.assertTrue(json.contains("\"category\": \"assertion\""), json);
            Assert.assertTrue(json.contains("\"topProjectFrame\": \"customer.LoginTest.failingScenario(LoginTest.java:27)\""), json);
            Assert.assertTrue(json.contains("\"Screenshot - failed Assert\""), json);
            Assert.assertTrue(json.contains("\"SHAFT Trace Report.html\""), json);
            Assert.assertTrue(json.contains("\"authorization-header\""), json);
            Assert.assertFalse(json.contains("raw-token"), json);
            Assert.assertFalse(json.contains("raw-password"), json);

            String html = FailureBriefReporter.renderBriefHtml(json);
            Assert.assertTrue(html.contains("SHAFT Failure Brief"), html);
            Assert.assertTrue(html.contains("--shaft-primary"), html);
            Assert.assertTrue(html.contains("status-chip"), html);
            Assert.assertFalse(html.contains("https://"), html);
            Assert.assertFalse(html.contains("cdn."), html);
        } finally {
            ReportContext.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Passed and skipped tests should not attach a failure brief")
    public void nonFailureStatusesShouldNotAttachFailureBrief() throws Exception {
        try {
            TestExecutionInfo passed = info("passingScenario", null);
            ReportContext.start(passed);
            ReportContext.setStatus(Status.PASSED);
            int beforePassed = attachments().size();
            FailureBriefReporter.attachOnFailure(passed, "pass log", List.of());
            Assert.assertEquals(attachments().size(), beforePassed);

            TestExecutionInfo skipped = info("skippedScenario", assertionFailure());
            ReportContext.start(skipped);
            ReportContext.setStatus(Status.SKIPPED);
            int beforeSkipped = attachments().size();
            FailureBriefReporter.attachOnFailure(skipped, "skip log", List.of());
            Assert.assertEquals(attachments().size(), beforeSkipped);
        } finally {
            ReportContext.clear();
            Properties.clearForCurrentThread();
        }
    }

    private static TestExecutionInfo info(String methodName, Throwable throwable) throws Exception {
        Method method = FailureBriefReporterTest.class.getDeclaredMethod("failingScenario");
        return new TestExecutionInfo("id-" + methodName, "customer.LoginTest", methodName, methodName,
                "failure brief test", method, throwable, false);
    }

    private static AssertionError assertionFailure() {
        AssertionError throwable = new AssertionError("Expected dashboard title, but found login page password=raw-password");
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
