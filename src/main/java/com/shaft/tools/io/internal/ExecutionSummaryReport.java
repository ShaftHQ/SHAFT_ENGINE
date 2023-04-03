package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.HTMLHelper;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;

public class ExecutionSummaryReport {
    private static final HashMap<Integer, ArrayList<?>> casesDetails = new HashMap<>();
    private static final HashMap<Integer, ArrayList<?>> validations = new HashMap<>();
    private static int passedValidations = 0;
    private static int failedValidations = 0;
    private static final String SHAFT_LOGO_URL = "https://github.com/ShaftHQ/SHAFT_ENGINE/raw/main/src/main/resources/images/shaft.png";

    public static void casesDetailsIncrement(String caseSuite, String caseName, String caseDescription,String errorMessage, String status, Boolean hasIssue) {
        ArrayList<String> entry = new ArrayList<>();
        entry.add(caseSuite);
        if (caseDescription != null && !caseDescription.equals("")) {
            entry.add(caseDescription);
        } else {
            entry.add(caseName);
        }
        entry.add(errorMessage);
        entry.add(status);
        if (hasIssue) {
            entry.add("Yes");
        } else {
            entry.add("No");
        }
        casesDetails.put(casesDetails.size() + 1, entry);
    }

    public static void validationsIncrement(CheckpointStatus status) {
        ArrayList<String> entry = new ArrayList<>();
        validations.put(validations.size() + 1, entry);

        if (status == CheckpointStatus.PASS) {
            passedValidations++;
        } else {
            failedValidations++;
        }
    }

    public static void generateExecutionSummaryReport(int passed, int failed, int skipped, long startTime, long endTime) {
        int total = passed + failed + skipped;

        StringBuilder detailsBuilder = new StringBuilder();
        casesDetails.forEach((key, value) -> detailsBuilder.append(String.format(HTMLHelper.EXECUTION_SUMMARY_DETAILS_FORMAT.getValue(), key, value.get(0), value.get(1), value.get(2), value.get(3), value.get(4))));

        SHAFT.CLI.file().writeToFile(System.getProperty("executionSummaryReportFolderPath"),
                "ExecutionSummaryReport_" + new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa").format(System.currentTimeMillis()) + ".html",
                HTMLHelper.EXECUTION_SUMMARY.getValue()
                        .replace("${LOGO_URL}", SHAFT_LOGO_URL)
                        .replace("${DATE}", new SimpleDateFormat("dd/MM/yyyy").format(endTime))
                        .replace("${START_TIME}", new SimpleDateFormat("HH:mm:ss").format(startTime))
                        .replace("${END_TIME}", new SimpleDateFormat("HH:mm:ss").format(endTime))
                        .replace("${TOTAL_TIME}", ReportManagerHelper.getExecutionDuration(startTime, endTime))
                        .replace("${CASES_PASSED_PERCENTAGE}", String.valueOf(new DecimalFormat("0.00").format((float) passed * 100 / total)))
                        .replace("${CASES_PASSED_PERCENTAGE_PIE}", String.valueOf(passed * 100 / total))
                        .replace("${CASES_FAILED_PERCENTAGE_PIE}", String.valueOf((failed * 100 / total) + (passed * 100 / total)))
                        .replace("${CASES_TOTAL}", String.valueOf(total))
                        .replace("${CASES_PASSED}", String.valueOf(passed))
                        .replace("${CASES_FAILED}", String.valueOf(failed))
                        .replace("${CASES_SKIPPED}", String.valueOf(skipped))
                        .replace("${VALIDATION_PASSED_PERCENTAGE_PIE}", String.valueOf(passedValidations * 360d / validations.size()))
                        .replace("${VALIDATION_PASSED_PERCENTAGE}", String.valueOf(new DecimalFormat("0.00").format((float) passedValidations * 100 / validations.size())))
                        .replace("${VALIDATION_TOTAL}", String.valueOf(validations.size()))
                        .replace("${VALIDATION_PASSED}", String.valueOf(passedValidations))
                        .replace("${VALIDATION_FAILED}", String.valueOf(failedValidations))
                        .replace("${TOTAL_ISSUES}", String.valueOf(ReportManagerHelper.getIssueCounter()))
                        .replace("${NO_OPEN_ISSUES_FAILED}", String.valueOf(ReportManagerHelper.getFailedTestsWithoutOpenIssuesCounter()))
                        .replace("${OPEN_ISSUES_PASSED}", String.valueOf(ReportManagerHelper.getOpenIssuesForPassedTestsCounter()))
                        .replace("${OPEN_ISSUES_FAILED}", String.valueOf(ReportManagerHelper.getOpenIssuesForFailedTestsCounters()))
                        .replace("${PASSED_DROPDOWN_OPTION}", StatusIcon.PASSED.getValue() + Status.PASSED.name())
                        .replace("${FAILED_DROPDOWN_OPTION}", StatusIcon.FAILED.getValue() + Status.FAILED.name())
                        .replace("${SKIPPED_DROPDOWN_OPTION}", StatusIcon.SKIPPED.getValue() + Status.SKIPPED.name())
                        .replace("${CASES_DETAILS}", detailsBuilder));

        ReportManagerHelper.openExecutionSummaryReportAfterExecution();
        ReportManagerHelper.logExecutionSummary(String.valueOf(total), String.valueOf(passed), String.valueOf(failed), String.valueOf(skipped));
    }

    public enum Status {
        PASSED, FAILED, SKIPPED
    }

    public enum StatusIcon {
        PASSED("&#9989; "), FAILED("&#10060; "), SKIPPED("&#128679; ");

        private String value;

        StatusIcon(String type) {
            this.value = type;
        }

        public String getValue() {
            return value;
        }
    }

}
