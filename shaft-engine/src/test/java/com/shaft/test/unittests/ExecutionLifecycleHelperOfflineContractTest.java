package com.shaft.test.unittests;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.ExecutionLifecycleHelper;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
import io.qameta.allure.TmsLink;
import io.qameta.allure.TmsLinks;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.List;

public class ExecutionLifecycleHelperOfflineContractTest {
    private boolean originalSkipLinkedIssues;
    private String originalVideoScope;

    @BeforeMethod(alwaysRun = true)
    public void rememberFlags() {
        originalSkipLinkedIssues = SHAFT.Properties.flags.skipTestsWithLinkedIssues();
        originalVideoScope = SHAFT.Properties.visuals.videoParamsScope();
    }

    @AfterMethod(alwaysRun = true)
    public void restoreFlags() {
        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(originalSkipLinkedIssues);
        SHAFT.Properties.visuals.set().videoParamsScope(originalVideoScope);
    }

    @Test
    public void missingAnnotationsStayBlank() throws Exception {
        Assert.assertEquals(ExecutionLifecycleHelper.getIssueAnnotationValue(null), "");
        Assert.assertEquals(ExecutionLifecycleHelper.getTmsLinkAnnotationValue(null), "");
        Assert.assertEquals(ExecutionLifecycleHelper.getIssueAnnotationValue(method("plain")), "");
        Assert.assertEquals(ExecutionLifecycleHelper.getTmsLinkAnnotationValue(method("plain")), "");
        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(true);
        Assert.assertEquals(ExecutionLifecycleHelper.getLinkedIssueSkipMessage(null), "");
        Assert.assertEquals(ExecutionLifecycleHelper.getLinkedIssueSkipMessage(method("plain")), "");
    }

    @Test
    public void issueAndTmsAnnotationsKeepTheirValues() throws Exception {
        Assert.assertEquals(ExecutionLifecycleHelper.getIssueAnnotationValue(method("singleIssue")), "SHAFT-1");
        Assert.assertEquals(ExecutionLifecycleHelper.getIssueAnnotationValue(method("manyIssues")), "SHAFT-1, SHAFT-2");
        Assert.assertEquals(ExecutionLifecycleHelper.getTmsLinkAnnotationValue(method("singleTms")), "TMS-1");
        Assert.assertEquals(ExecutionLifecycleHelper.getTmsLinkAnnotationValue(method("manyTms")), "TMS-1, TMS-2");
    }

    @Test
    public void linkedIssueSkipMessageFollowsTheFlag() throws Exception {
        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(false);
        Assert.assertEquals(ExecutionLifecycleHelper.getLinkedIssueSkipMessage(method("singleIssue")), "");

        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(true);
        Assert.assertEquals(
                ExecutionLifecycleHelper.getLinkedIssueSkipMessage(method("singleIssue")),
                "Skipping Test as it's expected to fail due to open issue: [SHAFT-1]");
        Assert.assertEquals(
                ExecutionLifecycleHelper.getLinkedIssueSkipMessage(method("manyIssues")),
                "Skipping Test as it's expected to fail due to open issues: [SHAFT-1 ,SHAFT-2]");
    }

    @Test
    public void testLogTrimsTwoTrailingCharacters() {
        Assert.assertEquals(ExecutionLifecycleHelper.createTestLog(null), "");
        Assert.assertEquals(ExecutionLifecycleHelper.createTestLog(List.of()), "");
        String oneLine = ExecutionLifecycleHelper.createTestLog(List.of("alpha"));
        String twoLines = ExecutionLifecycleHelper.createTestLog(List.of("alpha", "beta"));
        Assert.assertEquals(oneLine.length(), "alpha".length() + System.lineSeparator().length() - 2);
        Assert.assertEquals(twoLines.length(),
                "alpha".length() + "beta".length() + (System.lineSeparator().length() * 2) - 2);
        Assert.assertTrue(oneLine.startsWith("alp"), oneLine);
        Assert.assertTrue(twoLines.startsWith("alpha" + System.lineSeparator() + "be"), twoLines);
        Assert.assertFalse(oneLine.endsWith(System.lineSeparator()));
        Assert.assertFalse(twoLines.endsWith(System.lineSeparator()));
    }

    @Test
    public void nullMetadataIsIgnoredAndABlankMethodUsesTheDisplayName() {
        ExecutionLifecycleHelper.logTestInformation(null);
        ExecutionLifecycleHelper.logFinishedTestInformation(null, "Failed");
        ExecutionLifecycleHelper.logFinishedTestInformation(null, "Failed", new AssertionError("closed"));
        ExecutionLifecycleHelper.appendExecutionSummaryReport(
                null, "closed", ExecutionSummaryReport.StatusIcon.FAILED, ExecutionSummaryReport.Status.FAILED);

        TestExecutionInfo started = info("blankMethod", null, null, "shown-name", "opens the row", null);
        ExecutionLifecycleHelper.logTestInformation(started);
        ExecutionLifecycleHelper.logFinishedTestInformation(
                started, "Failed", new AssertionError("row failed"));
        ExecutionLifecycleHelper.appendExecutionSummaryReport(
                started,
                "row failed",
                ExecutionSummaryReport.StatusIcon.FAILED,
                ExecutionSummaryReport.Status.FAILED);
    }

    @Test
    public void artifactAttachmentSkipsLifecycleHooksAndAttachesAMethodLog() throws Exception {
        SHAFT.Properties.visuals.set().videoParamsScope("DriverSession");
        ExecutionLifecycleHelper.attachTestArtifacts(null);
        ExecutionLifecycleHelper.attachTestArtifacts(info("suiteSetup", "suiteSetup", method("plain"), "suite", "", null));
        ExecutionLifecycleHelper.attachTestArtifacts(info("suiteTeardown", "suiteTeardown", method("plain"), "suite", "", null));
        ExecutionLifecycleHelper.attachTestArtifacts(info("classTeardown", "classTeardown", method("plain"), "class", "", null));

        TestExecutionInfo failed = info("case", "singleIssue", method("singleIssue"), "singleIssue", "linked",
                new AssertionError("artifact"));
        ExecutionLifecycleHelper.attachTestArtifacts(failed);
    }

    private static Method method(String name) throws NoSuchMethodException {
        return ExecutionLifecycleHelperOfflineContractTest.class.getDeclaredMethod(name);
    }

    private static TestExecutionInfo info(String stableId, String methodName, Method method, String displayName,
                                          String description, Throwable throwable) {
        return new TestExecutionInfo(stableId, "offline.Lifecycle", methodName, displayName, description, method,
                throwable, false);
    }

    @Issue("SHAFT-1")
    private void singleIssue() {
    }

    @Issues({@Issue("SHAFT-1"), @Issue("SHAFT-2")})
    private void manyIssues() {
    }

    @TmsLink("TMS-1")
    private void singleTms() {
    }

    @TmsLinks({@TmsLink("TMS-1"), @TmsLink("TMS-2")})
    private void manyTms() {
    }

    private void plain() {
    }
}
