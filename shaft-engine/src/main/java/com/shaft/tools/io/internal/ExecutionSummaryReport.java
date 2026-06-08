package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.HTMLHelper;
import lombok.Getter;

import java.text.DecimalFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

public class ExecutionSummaryReport {
    private static final ConcurrentHashMap<Integer, ArrayList<?>> casesDetails = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Integer, ArrayList<?>> validations = new ConcurrentHashMap<>();
    private static final String SHAFT_LOGO_URL = "https://github.com/ShaftHQ/SHAFT_ENGINE/raw/main/src/main/resources/images/shaft.png";
    private static final AtomicInteger passedValidations = new AtomicInteger(0);
    private static final AtomicInteger failedValidations = new AtomicInteger(0);
    private static final DateTimeFormatter FILENAME_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss-SSSS-a").withZone(ZoneId.systemDefault());
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
        casesDetails.put(casesDetails.size() + 1, entry);
    }

    public static void validationsIncrement(CheckpointStatus status) {
        ArrayList<String> entry = new ArrayList<>();
        validations.put(validations.size() + 1, entry);

        if (status == CheckpointStatus.PASS) {
            passedValidations.incrementAndGet();
        } else {
            failedValidations.incrementAndGet();
        }
    }

    public static void generateExecutionSummaryReport(int passed, int failed, int skipped, long startTime, long endTime) {
        int total = passed + failed + skipped;

        StringBuilder detailsBuilder = new StringBuilder();
        casesDetails.forEach((key, value) -> detailsBuilder.append(String.format(HTMLHelper.EXECUTION_SUMMARY_DETAILS_FORMAT.getValue(), key, value.getFirst(), value.get(1), value.get(2), value.get(3), value.get(4), value.get(5))));

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
                .replace("${LOGO_URL}", SHAFT_LOGO_URL)
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
            report = report.replace("${CASES_PASSED_PERCENTAGE}", String.valueOf(total))
                    .replace("${CASES_PASSED_PERCENTAGE_PIE}", String.valueOf(total))
                    .replace("${CASES_FAILED_PERCENTAGE_PIE}", String.valueOf(total));
        }
        if (!validations.isEmpty()) {
            report = report
                    .replace("${VALIDATION_PASSED_PERCENTAGE_PIE}", String.valueOf(passedValidations.get() * 360d / validations.size()))
                    .replace("${VALIDATION_PASSED_PERCENTAGE}", String.valueOf(new DecimalFormat("0.00").format((float) passedValidations.get() * 100 / validations.size())))
                    .replace("${VALIDATION_TOTAL}", String.valueOf(validations.size()));
        } else {
            report = report
                    .replace("${VALIDATION_PASSED_PERCENTAGE_PIE}", String.valueOf(0))
                    .replace("${VALIDATION_PASSED_PERCENTAGE}", String.valueOf(0))
                    .replace("${VALIDATION_TOTAL}", String.valueOf(0));
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
