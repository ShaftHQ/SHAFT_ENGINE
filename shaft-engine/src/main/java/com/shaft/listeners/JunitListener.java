package com.shaft.listeners;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.ExecutionCountsTracker;
import com.shaft.listeners.internal.ExecutionLifecycleHelper;
import com.shaft.listeners.internal.JunitListenerHelper;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.IssueReporter;
import com.shaft.tools.io.internal.ProjectStructureManager;
import com.shaft.tools.io.internal.ReportContext;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.ValidationsHelper;
import io.qameta.allure.model.Status;
import lombok.Getter;
import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.engine.support.descriptor.MethodSource;
import org.junit.platform.launcher.LauncherSession;
import org.junit.platform.launcher.LauncherSessionListener;
import org.junit.platform.launcher.TestExecutionListener;
import org.junit.platform.launcher.TestIdentifier;
import org.junit.platform.launcher.TestPlan;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * JUnit Platform launcher listener that connects Jupiter execution to SHAFT reporting.
 */
public class JunitListener implements LauncherSessionListener {
    private static final List<TestIdentifier> passedTests = Collections.synchronizedList(new ArrayList<>());
    private static final List<TestIdentifier> failedTests = Collections.synchronizedList(new ArrayList<>());
    private static final List<TestIdentifier> skippedTests = Collections.synchronizedList(new ArrayList<>());
    private static final ExecutionCountsTracker countsTracker = new ExecutionCountsTracker();
    private static long executionStartTime;
    private static boolean isEngineReady = false;
    @Getter
    private static volatile Boolean isLastFinishedTestOK = true;

    @Override
    public void launcherSessionOpened(LauncherSession session) {
        if (!isEngineReady) {
            session.getLauncher().registerTestExecutionListeners(new TestExecutionListener() {
                @Override
                public void testPlanExecutionStarted(TestPlan testPlan) {
                    executionStartTime = System.currentTimeMillis();
                    passedTests.clear();
                    failedTests.clear();
                    skippedTests.clear();
                    countsTracker.clear();
                    ExecutionLifecycleHelper.engineSetup(ProjectStructureManager.RunType.JUNIT);
                    isEngineReady = true;
                }

                @Override
                public void testPlanExecutionFinished(TestPlan testPlan) {
                    engineTearDown();
                }

                @Override
                public void executionSkipped(TestIdentifier testIdentifier, String reason) {
                    if (testIdentifier.isTest()) {
                        TestExecutionInfo info = toTestExecutionInfo(testIdentifier, new org.opentest4j.TestAbortedException(reason));
                        ReportContext.start(info);
                        ReportContext.setStatus(Status.SKIPPED);
                        afterInvocation(info);
                        onTestSkipped(testIdentifier, reason, info);
                        ReportContext.clear();
                    }
                }

                @Override
                public void executionStarted(TestIdentifier testIdentifier) {
                    if (testIdentifier.isTest()) {
                        TestExecutionInfo info = toTestExecutionInfo(testIdentifier, null);
                        ReportContext.start(info);
                        ReportContext.setStatus(Status.PASSED);
                        JunitListenerHelper.setTestName(testIdentifier);
                        ExecutionLifecycleHelper.logTestInformation(info);
                    }
                }

                @Override
                public void executionFinished(TestIdentifier testIdentifier, TestExecutionResult testExecutionResult) {
                    if (!testIdentifier.isTest()) {
                        return;
                    }
                    Throwable throwable = resolveThrowable(testExecutionResult);
                    TestExecutionInfo info = toTestExecutionInfo(testIdentifier, throwable);
                    ReportContext.update(info);
                    ReportContext.setStatus(toAllureStatus(testExecutionResult));
                    afterInvocation(info);
                    switch (testExecutionResult.getStatus()) {
                        case SUCCESSFUL -> {
                            AssertionError verificationError = ValidationsHelper.getVerificationErrorToForceFail();
                            if (verificationError != null) {
                                ValidationsHelper.resetVerificationStateAfterFailing();
                                TestExecutionInfo failedInfo = toTestExecutionInfo(testIdentifier, verificationError);
                                ReportContext.update(failedInfo);
                                ReportContext.setStatus(Status.FAILED);
                                onTestFailure(testIdentifier, verificationError, failedInfo);
                            } else {
                                onTestSuccess(testIdentifier, info);
                            }
                        }
                        case FAILED, ABORTED -> onTestFailure(testIdentifier, throwable, info);
                    }
                    ReportContext.clear();
                }
            });
        }
    }

    private void engineTearDown() {
        ExecutionCountsTracker.Counts counts = countsTracker.snapshot();
        if (counts.finalPassed() + counts.failed() + counts.skipped() == 0
                && (!passedTests.isEmpty() || !failedTests.isEmpty() || !skippedTests.isEmpty())) {
            counts = legacyCountsSnapshot();
        }
        ExecutionLifecycleHelper.engineTearDown(executionStartTime, counts);
    }

    static void recordRetriedFailure(TestExecutionInfo info) {
        countsTracker.recordFailed(info);
    }

    static void clearRecordedFailures() {
        countsTracker.clearFailures();
    }

    private void afterInvocation(TestExecutionInfo info) {
        ReportManagerHelper.setDiscreteLogging(SHAFT.Properties.reporting.alwaysLogDiscreetly());
        ExecutionLifecycleHelper.attachTestArtifacts(info);
    }

    private void onTestSuccess(TestIdentifier testIdentifier, TestExecutionInfo info) {
        passedTests.add(testIdentifier);
        countsTracker.recordPassed(info);
        isLastFinishedTestOK = true;
        ExecutionLifecycleHelper.logFinishedTestInformation(info, "Passed");
        appendToExecutionSummaryReport(info, "", ExecutionSummaryReport.StatusIcon.PASSED, ExecutionSummaryReport.Status.PASSED);
        IssueReporter.updateIssuesLog(info, true);
    }

    private void onTestFailure(TestIdentifier testIdentifier, Throwable throwable, TestExecutionInfo info) {
        failedTests.add(testIdentifier);
        countsTracker.recordFailed(info);
        isLastFinishedTestOK = false;
        ExecutionLifecycleHelper.logFinishedTestInformation(info, "Failed");
        appendToExecutionSummaryReport(info, throwable == null ? "" : throwable.getMessage(), ExecutionSummaryReport.StatusIcon.FAILED, ExecutionSummaryReport.Status.FAILED);
        IssueReporter.updateIssuesLog(info, false);
    }

    private void onTestSkipped(TestIdentifier testIdentifier, String reason, TestExecutionInfo info) {
        skippedTests.add(testIdentifier);
        countsTracker.recordSkipped(info);
        isLastFinishedTestOK = false;
        ExecutionLifecycleHelper.logFinishedTestInformation(info, "Skipped");
        appendToExecutionSummaryReport(info, reason, ExecutionSummaryReport.StatusIcon.SKIPPED, ExecutionSummaryReport.Status.SKIPPED);
    }

    private void appendToExecutionSummaryReport(TestExecutionInfo info, String errorMessage,
                                                ExecutionSummaryReport.StatusIcon statusIcon,
                                                ExecutionSummaryReport.Status status) {
        ExecutionLifecycleHelper.appendExecutionSummaryReport(info, errorMessage, statusIcon, status);
    }

    private void appendToExecutionSummaryReport(TestIdentifier testIdentifier, String errorMessage,
                                                ExecutionSummaryReport.StatusIcon statusIcon,
                                                ExecutionSummaryReport.Status status) {
        if (testIdentifier.isTest()) {
            appendToExecutionSummaryReport(toTestExecutionInfo(testIdentifier, null), errorMessage, statusIcon, status);
        }
    }

    private static ExecutionCountsTracker.Counts legacyCountsSnapshot() {
        ExecutionCountsTracker tracker = new ExecutionCountsTracker();
        passedTests.forEach(testIdentifier -> tracker.recordPassed(toTestExecutionInfo(testIdentifier, null)));
        failedTests.forEach(testIdentifier -> tracker.recordFailed(toTestExecutionInfo(testIdentifier, new AssertionError())));
        skippedTests.forEach(testIdentifier -> tracker.recordSkipped(toTestExecutionInfo(testIdentifier, null)));
        return tracker.snapshot();
    }

    private static TestExecutionInfo toTestExecutionInfo(TestIdentifier testIdentifier, Throwable throwable) {
        String className = "";
        String methodName = testIdentifier.getDisplayName();
        Method method = null;
        Optional<MethodSource> methodSource = testIdentifier.getSource()
                .filter(MethodSource.class::isInstance)
                .map(MethodSource.class::cast);
        if (methodSource.isPresent()) {
            className = methodSource.get().getClassName();
            methodName = methodSource.get().getMethodName();
            method = resolveMethod(className, methodName);
        }
        String description = testIdentifier.getLegacyReportingName();
        return new TestExecutionInfo(testIdentifier.getUniqueId(), className, methodName,
                testIdentifier.getDisplayName(), description, method, throwable, false);
    }

    private static Method resolveMethod(String className, String methodName) {
        try {
            for (Method method : Class.forName(className).getDeclaredMethods()) {
                if (method.getName().equals(methodName)) {
                    return method;
                }
            }
        } catch (ReflectiveOperationException ignored) {
            // Summary metadata is best-effort when JUnit does not expose a resolvable method.
        }
        return null;
    }

    private static Throwable resolveThrowable(TestExecutionResult result) {
        return result.getThrowable().orElseGet(() -> result.getStatus() == TestExecutionResult.Status.FAILED
                ? new AssertionError("Test Failed")
                : null);
    }

    private static Status toAllureStatus(TestExecutionResult result) {
        return switch (result.getStatus()) {
            case FAILED -> Status.FAILED;
            case ABORTED -> Status.SKIPPED;
            default -> Status.PASSED;
        };
    }
}
