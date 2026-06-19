package com.shaft.listeners.internal;

import com.shaft.api.RequestBuilder;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.AnimatedGifManager;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.tools.internal.FirestoreRestClient;
import com.shaft.tools.internal.security.GoogleTink;
import com.shaft.tools.io.internal.AllureManager;
import com.shaft.tools.io.internal.ApiPerformanceExecutionReport;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.ProjectStructureManager;
import com.shaft.tools.io.internal.ReportContext;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
import io.qameta.allure.TmsLink;
import io.qameta.allure.TmsLinks;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Shared execution lifecycle behavior used by SHAFT runner adapters.
 */
public final class ExecutionLifecycleHelper {
    private ExecutionLifecycleHelper() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Initializes SHAFT engine services for the selected runner.
     *
     * @param runType current execution runner
     */
    public static void engineSetup(ProjectStructureManager.RunType runType) {
        PropertiesHelper.bootstrapEngine(runType);
    }

    /**
     * Logs the start of a test method.
     *
     * @param info current test metadata
     */
    public static void logTestInformation(TestExecutionInfo info) {
        if (info != null) {
            ReportManagerHelper.logTestInformation(
                    valueOrBlank(info.className()),
                    valueOrBlank(info.methodName()),
                    valueOrBlank(info.description()));
        }
    }

    /**
     * Logs the end of a test method.
     *
     * @param info current test metadata
     * @param status execution status text
     */
    public static void logFinishedTestInformation(TestExecutionInfo info, String status) {
        if (info != null) {
            ReportManagerHelper.logFinishedTestInformation(
                    valueOrBlank(info.className()),
                    valueOrBlank(info.methodName()),
                    valueOrBlank(info.description()),
                    valueOrBlank(status));
        }
    }

    /**
     * Attaches test-scoped SHAFT artifacts and files issue reports.
     *
     * @param info current test metadata
     */
    public static void attachTestArtifacts(TestExecutionInfo info) {
        if (info == null || excludedFromArtifactAttachment(info.methodName())) {
            return;
        }

        List<String> attachments = new ArrayList<>();
        String attachment;
        if (SHAFT.Properties.visuals.videoParamsScope().equals("TestMethod")) {
            RecordManager.attachVideoRecording();
            attachment = RecordManager.getVideoRecordingFilePath();
            if (attachment != null && !attachment.isEmpty()) {
                attachments.add(attachment);
            }
        }
        attachment = AnimatedGifManager.attachAnimatedGif();
        if (attachment != null && !attachment.isEmpty()) {
            attachments.add(attachment);
        }

        String logText = createTestLog(ReportContext.snapshotOutput());
        ReportManagerHelper.attachTestLog(valueOrBlank(info.methodName()), logText);
        JiraHelper.reportBugsToJIRA(attachments, logText, info);
    }

    /**
     * Adds one execution summary row for the provided test.
     *
     * @param info current test metadata
     * @param errorMessage failure or skip message
     * @param statusIcon summary icon
     * @param status summary status
     */
    public static void appendExecutionSummaryReport(TestExecutionInfo info, String errorMessage,
                                                    ExecutionSummaryReport.StatusIcon statusIcon,
                                                    ExecutionSummaryReport.Status status) {
        if (info == null) {
            return;
        }
        String className = valueOrBlank(info.className());
        String methodName = valueOrBlank(info.methodName());
        String caseName = methodName.isBlank() ? valueOrBlank(info.displayName()) : methodName;
        String statusMessage = statusIcon.getValue() + status.name();
        ExecutionSummaryReport.casesDetailsIncrement(
                getTmsLinkAnnotationValue(info.method()),
                className,
                caseName,
                valueOrBlank(info.description()),
                valueOrBlank(errorMessage),
                statusMessage,
                getIssueAnnotationValue(info.method()));
    }

    /**
     * Finalizes shared engine reports and telemetry.
     *
     * @param executionStartTime engine start timestamp
     * @param counts deduplicated execution counts
     */
    public static void engineTearDown(long executionStartTime, ExecutionCountsTracker.Counts counts) {
        ReportManagerHelper.setDiscreteLogging(true);
        long executionEndTime = System.currentTimeMillis();
        Thread.ofVirtual().start(() -> ExecutionSummaryReport.generateExecutionSummaryReport(
                counts.finalPassed(), counts.failed(), counts.skipped(), executionStartTime, executionEndTime));
        Thread.ofVirtual().start(JiraHelper::reportExecutionStatusToJira);
        Thread.ofVirtual().start(GoogleTink::encrypt);
        Thread.ofVirtual().start(() -> FirestoreRestClient.sendTelemetry(
                executionStartTime, executionEndTime, counts.passed(), counts.failed(), counts.skipped(), counts.flaky()));
        ReportManagerHelper.logEngineClosure();
        Thread.ofVirtual().start(() -> {
            Map<String, List<Double>> performanceData = RequestBuilder.getPerformanceData();
            ApiPerformanceExecutionReport.generatePerformanceReport(performanceData, executionStartTime, System.currentTimeMillis());
        });
        AllureManager.openAllureReportAfterExecution();
        AllureManager.generateAllureReportArchive();
    }

    /**
     * Extracts issue annotation values from a reflected method.
     *
     * @param method reflected test method
     * @return issue values, or empty string
     */
    public static String getIssueAnnotationValue(Method method) {
        if (method == null) {
            return "";
        }
        Issue issue = method.getAnnotation(Issue.class);
        Issues issues = method.getAnnotation(Issues.class);
        if (issues != null) {
            return Arrays.toString(issues.value())
                    .replace("[@io.qameta.allure.Issue(\"", "")
                    .replace("@io.qameta.allure.Issue(\"", "")
                    .replace("\")]", "")
                    .replace("\"),", ",");
        } else if (issue != null) {
            return issue.value();
        }
        return "";
    }

    /**
     * Extracts TMS annotation values from a reflected method.
     *
     * @param method reflected test method
     * @return TMS values, or empty string
     */
    public static String getTmsLinkAnnotationValue(Method method) {
        if (method == null) {
            return "";
        }
        TmsLink tmsLink = method.getAnnotation(TmsLink.class);
        TmsLinks tmsLinks = method.getAnnotation(TmsLinks.class);
        if (tmsLinks != null) {
            return Arrays.toString(tmsLinks.value())
                    .replace("[@io.qameta.allure.TmsLink(\"", "")
                    .replace("@io.qameta.allure.TmsLink(\"", "")
                    .replace("\")]", "")
                    .replace("\"),", ",");
        } else if (tmsLink != null) {
            return tmsLink.value();
        }
        return "";
    }

    /**
     * Builds the linked-issue skip message for a test method when enabled.
     *
     * @param method reflected test method
     * @return skip message, or empty string
     */
    public static String getLinkedIssueSkipMessage(Method method) {
        if (method == null || !SHAFT.Properties.flags.skipTestsWithLinkedIssues()) {
            return "";
        }
        Issue issue = method.getAnnotation(Issue.class);
        if (issue != null) {
            return "Skipping Test as it's expected to fail due to open issue: [" + issue.value() + "]";
        }
        Issues issues = method.getAnnotation(Issues.class);
        if (issues != null) {
            StringBuilder issueNames = new StringBuilder();
            Arrays.stream(issues.value()).iterator().forEachRemaining(issueI -> issueNames.append(issueI.value()).append(" ,"));
            return "Skipping Test as it's expected to fail due to open issues: [" + issueNames.substring(0, issueNames.length() - 2) + "]";
        }
        return "";
    }

    /**
     * Creates a single test-log string from runner output entries.
     *
     * @param output report output lines
     * @return aggregated log text
     */
    public static String createTestLog(List<String> output) {
        StringBuilder builder = new StringBuilder();
        if (output != null) {
            for (String each : output) {
                builder.append(each).append(System.lineSeparator());
            }
        }
        String testLog = builder.toString();
        if (testLog.length() >= 2) {
            return testLog.substring(0, builder.length() - 2);
        }
        return testLog;
    }

    private static boolean excludedFromArtifactAttachment(String methodName) {
        return List.of("suiteSetup", "suiteTeardown", "classTeardown").contains(methodName);
    }

    private static String valueOrBlank(String value) {
        return value == null ? "" : value;
    }
}
