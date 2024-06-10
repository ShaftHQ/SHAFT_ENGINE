package com.shaft.listeners;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.JiraHelper;
import com.shaft.listeners.internal.JunitListenerHelper;
import com.shaft.tools.internal.security.GoogleTink;
import com.shaft.tools.io.internal.AllureManager;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.ProjectStructureManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.launcher.*;

import java.util.ArrayList;
import java.util.List;

public class JunitListener implements LauncherSessionListener {
    private static final List<TestIdentifier> passedTests = new ArrayList<>();
    private static final List<TestIdentifier> failedTests = new ArrayList<>();
    private static final List<TestIdentifier> skippedTests = new ArrayList<>();
    private static long executionStartTime;
    private static boolean isEngineReady = false;

    @Override
    public void launcherSessionOpened(LauncherSession session) {
        if (!isEngineReady) {
            session.getLauncher().registerTestExecutionListeners(new TestExecutionListener() {
                @Override
                public void testPlanExecutionStarted(TestPlan testPlan) {
                    executionStartTime = System.currentTimeMillis();
                    TestNGListener.engineSetup(ProjectStructureManager.RunType.JUNIT);
                    isEngineReady = true;
                }

                @Override
                public void testPlanExecutionFinished(TestPlan testPlan) {
                    engineTearDown();
                }

                @Override
                public void executionSkipped(TestIdentifier testIdentifier, String reason) {
                    afterInvocation();
                    onTestSkipped(testIdentifier, reason);
                }

                @Override
                public void executionStarted(TestIdentifier testIdentifier) {
                    JunitListenerHelper.setTestName(testIdentifier);
                    JunitListenerHelper.logTestInformation(testIdentifier);
                }

                @Override
                public void executionFinished(TestIdentifier testIdentifier, TestExecutionResult testExecutionResult) {
                    afterInvocation();
                    if (testIdentifier.isTest()) {
                        switch (testExecutionResult.getStatus()) {
                            case SUCCESSFUL -> onTestSuccess(testIdentifier);
                            case FAILED, ABORTED -> {
                                Throwable throwable = testExecutionResult.getThrowable().isPresent() ? testExecutionResult.getThrowable().get() : new AssertionError("Test Failed");
                                onTestFailure(testIdentifier, throwable);
                            }
                        }
                    }
                }
            });
        }
    }

    private void engineTearDown() {
        ReportManagerHelper.setDiscreteLogging(true);
        JiraHelper.reportExecutionStatusToJira();
        GoogleTink.encrypt();
        AllureManager.generateAllureReportArchive();
        AllureManager.openAllureReportAfterExecution();
        long executionEndTime = System.currentTimeMillis();
        ExecutionSummaryReport.generateExecutionSummaryReport(passedTests.size(), failedTests.size(), skippedTests.size(), executionStartTime, executionEndTime);
        ReportManagerHelper.logEngineClosure();
    }

    private void afterInvocation() {
        ReportManagerHelper.setDiscreteLogging(SHAFT.Properties.reporting.alwaysLogDiscreetly());
    }

    private void onTestSuccess(TestIdentifier testIdentifier) {
        passedTests.add(testIdentifier);
        appendToExecutionSummaryReport(testIdentifier, "", ExecutionSummaryReport.StatusIcon.PASSED, ExecutionSummaryReport.Status.PASSED);
    }

    private void onTestFailure(TestIdentifier testIdentifier, Throwable throwable) {
        failedTests.add(testIdentifier);
        appendToExecutionSummaryReport(testIdentifier, throwable.getMessage(), ExecutionSummaryReport.StatusIcon.FAILED, ExecutionSummaryReport.Status.FAILED);
    }

    private void onTestSkipped(TestIdentifier testIdentifier, String reason) {
        skippedTests.add(testIdentifier);
        appendToExecutionSummaryReport(testIdentifier, reason, ExecutionSummaryReport.StatusIcon.SKIPPED, ExecutionSummaryReport.Status.SKIPPED);
    }

    private void appendToExecutionSummaryReport(TestIdentifier testIdentifier, String errorMessage, ExecutionSummaryReport.StatusIcon statusIcon, ExecutionSummaryReport.Status status) {
        if (testIdentifier.getType().isTest()) {
            String caseSuite = testIdentifier.getUniqueIdObject().getSegments().get(1).getValue() + "." + testIdentifier.getUniqueIdObject().getLastSegment().getValue();
            String caseName = testIdentifier.getDisplayName();
            String caseDescription = testIdentifier.getLegacyReportingName();
            String statusMessage = statusIcon.getValue() + status.name();
            // Will add empty strings o the tmsLink and issue params until we figure out how to get the values of the annotations using JUnit
            ExecutionSummaryReport.casesDetailsIncrement("", caseSuite, caseName, caseDescription, errorMessage, statusMessage, "");
        }

    }
}
