package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.HTMLHelper;
import com.shaft.tools.internal.support.ReportHtmlTheme;
import lombok.Getter;

import java.text.DecimalFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

public class ExecutionSummaryReport {
    private static final List<ArrayList<String>> casesDetails = new CopyOnWriteArrayList<>();
    private static final AtomicInteger passedValidations = new AtomicInteger(0);
    private static final AtomicInteger failedValidations = new AtomicInteger(0);
    private static final DateTimeFormatter FILENAME_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss-SSS").withZone(ZoneId.systemDefault());
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy").withZone(ZoneId.systemDefault());
    private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss").withZone(ZoneId.systemDefault());

    public static void casesDetailsIncrement(String tmsLink, String caseSuite, String caseName, String caseDescription, String errorMessage, String status, String issue) {
        ArrayList<String> entry = new ArrayList<>();
        entry.add(tmsLink);
        entry.add(caseSuite);
        if (caseDescription != null && !caseDescription.isEmpty()) {
            entry.add(caseDescription);
        } else {
            entry.add(caseName);
        }
        entry.add(errorMessage);
        entry.add(status);
        entry.add(issue);
        casesDetails.add(entry);
    }

    public static void validationsIncrement(CheckpointStatus status) {
        if (status == CheckpointStatus.PASS) {
            passedValidations.incrementAndGet();
        } else {
            failedValidations.incrementAndGet();
        }
    }

    public static void generateExecutionSummaryReport(int passed, int failed, int skipped, long startTime, long endTime) {
        int total = passed + failed + skipped;

        StringBuilder detailsBuilder = new StringBuilder();
        int caseId = 0;
        for (ArrayList<String> value : casesDetails) {
            detailsBuilder.append(String.format(
                    HTMLHelper.EXECUTION_SUMMARY_DETAILS_FORMAT.getValue(),
                    ++caseId,
                    ReportHtmlTheme.escapeHtml(value.getFirst()),
                    ReportHtmlTheme.escapeHtml(value.get(1)),
                    ReportHtmlTheme.escapeHtml(value.get(2)),
                    ReportHtmlTheme.escapeHtml(value.get(3)),
                    ReportHtmlTheme.statusClass(value.get(4)),
                    value.get(4),
                    ReportHtmlTheme.escapeHtml(value.get(5))));
        }

        var fileActionsSession = FileActions.getInstance(true);

        fileActionsSession.createFolder(SHAFT.Properties.paths.executionSummaryReport());
        fileActionsSession.writeToFile(SHAFT.Properties.paths.executionSummaryReport(),
                "ExecutionSummaryReport_" + FILENAME_FORMATTER.format(Instant.ofEpochMilli(System.currentTimeMillis())) + ".html",
                createReportMessage(passed, failed, skipped, startTime, endTime, detailsBuilder));

        ReportManagerHelper.openExecutionSummaryReportAfterExecution();
        ReportManagerHelper.logExecutionSummary(String.valueOf(total), String.valueOf(passed), String.valueOf(failed), String.valueOf(skipped));
    }

    private static String createReportMessage(int passed, int failed, int skipped, long startTime, long endTime, StringBuilder detailsBuilder) {
        float total = passed + failed + skipped;
        var report = HTMLHelper.EXECUTION_SUMMARY.getValue()
                .replace("${DATE}", DATE_FORMATTER.format(Instant.ofEpochMilli(endTime)))
                .replace("${START_TIME}", TIME_FORMATTER.format(Instant.ofEpochMilli(startTime)))
                .replace("${END_TIME}", TIME_FORMATTER.format(Instant.ofEpochMilli(endTime)))
                .replace("${TOTAL_TIME}", ReportManagerHelper.getExecutionDuration(startTime, endTime))
                .replace("${CASES_TOTAL}", String.valueOf((int) total))
                .replace("${CASES_PASSED}", String.valueOf(passed))
                .replace("${CASES_FAILED}", String.valueOf(failed))
                .replace("${CASES_SKIPPED}", String.valueOf(skipped))
                .replace("${VALIDATION_PASSED}", String.valueOf(passedValidations.get()))
                .replace("${VALIDATION_FAILED}", String.valueOf(failedValidations.get()))
                .replace("${TOTAL_ISSUES}", String.valueOf(ReportManagerHelper.getIssueCounter()))
                .replace("${NO_OPEN_ISSUES_FAILED}", String.valueOf(ReportManagerHelper.getFailedTestsWithoutOpenIssuesCounter()))
                .replace("${OPEN_ISSUES_PASSED}", String.valueOf(ReportManagerHelper.getOpenIssuesForPassedTestsCounter()))
                .replace("${OPEN_ISSUES_FAILED}", String.valueOf(ReportManagerHelper.getOpenIssuesForFailedTestsCounters()))
                .replace("${PASSED_DROPDOWN_OPTION}", StatusIcon.PASSED.getValue() + Status.PASSED.name())
                .replace("${FAILED_DROPDOWN_OPTION}", StatusIcon.FAILED.getValue() + Status.FAILED.name())
                .replace("${SKIPPED_DROPDOWN_OPTION}", StatusIcon.SKIPPED.getValue() + Status.SKIPPED.name())
                .replace("${CASES_DETAILS}", detailsBuilder);
        if (total > 0) {
            report = report
                    .replace("${CASES_PASSED_PERCENTAGE}", String.valueOf(new DecimalFormat("0.00").format((float) passed * 100 / total)))
                    .replace("${CASES_PASSED_PERCENTAGE_PIE}", String.valueOf(passed * 100 / total))
                    .replace("${CASES_FAILED_PERCENTAGE_PIE}", String.valueOf((failed * 100 / total) + (passed * 100 / total)));
        } else {
            report = report.replace("${CASES_PASSED_PERCENTAGE}", "0.00")
                    .replace("${CASES_PASSED_PERCENTAGE_PIE}", "0")
                    .replace("${CASES_FAILED_PERCENTAGE_PIE}", "0");
        }
        int totalValidations = passedValidations.get() + failedValidations.get();
        if (totalValidations > 0) {
            report = report
                    .replace("${VALIDATION_PASSED_PERCENTAGE_PIE}", String.valueOf(passedValidations.get() * 360d / totalValidations))
                    .replace("${VALIDATION_PASSED_PERCENTAGE}", String.valueOf(new DecimalFormat("0.00").format((float) passedValidations.get() * 100 / totalValidations)))
                    .replace("${VALIDATION_TOTAL}", String.valueOf(totalValidations));
        } else {
            report = report
                    .replace("${VALIDATION_PASSED_PERCENTAGE_PIE}", "0")
                    .replace("${VALIDATION_PASSED_PERCENTAGE}", "0.00")
                    .replace("${VALIDATION_TOTAL}", "0");
        }
        return report;
    }

    public enum Status {
        PASSED, FAILED, SKIPPED
    }

    @Getter
    public enum StatusIcon {
        PASSED("&#9989; "), FAILED("&#10060; "), SKIPPED("&#128679; ");

        private final String value;

        StatusIcon(String type) {
            this.value = type;
        }

    }

}
