package io.github.shafthq.shaft.tools.io;

import com.shaft.driver.SHAFT;
import io.github.shafthq.shaft.tools.support.HTMLHelper;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;

import java.util.ArrayList;
import java.util.HashMap;

public class ExecutionSummaryReport {

    private static final HashMap<Integer, ArrayList<?>> casesDetails = new HashMap<>();

    public static void casesDetailsIncrement(String caseSuite, String caseName, String caseDescription, String status) {
        ArrayList<String> entry = new ArrayList<>();
        entry.add(caseSuite);
        if (caseDescription != null && !caseDescription.equals("")) {
            entry.add(caseDescription);
        } else {
            entry.add(caseName);
        }
        entry.add(status);
        casesDetails.put(casesDetails.size() + 1, entry);
    }

    public static void generateExecutionSummaryReport(int passed, int failed, int skipped) {
        int total = passed + failed + skipped;

        StringBuilder detailsBuilder = new StringBuilder();
        casesDetails.forEach((key, value) -> detailsBuilder.append(String.format(HTMLHelper.EXECUTION_SUMMARY_DETAILS_FORMAT.getValue(), key, value.get(0), value.get(1), value.get(2))));

        new SHAFT.CLI().file().writeToFile(System.getProperty("executionSummaryReportFolderPath"),
                "ExecutionSummaryReport_" + new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa").format(System.currentTimeMillis()) + ".html",
                HTMLHelper.EXECUTION_SUMMARY.getValue()
                        .replace("${DATE}", new SimpleDateFormat("dd/MM/yyyy").format(System.currentTimeMillis()))
                        .replace("${CASES_PASSED_PERCENTAGE}", String.valueOf(new DecimalFormat("0.00").format((float) passed * 100 / total)))
                        .replace("${CASES_PASSED_PERCENTAGE_PIE}", String.valueOf(passed * 100 / total))
                        .replace("${CASES_FAILED_PERCENTAGE_PIE}", String.valueOf((failed * 100 / total) + (passed * 100 / total)))
                        .replace("${CASES_TOTAL}", String.valueOf(total))
                        .replace("${CASES_PASSED}", String.valueOf(passed))
                        .replace("${CASES_FAILED}", String.valueOf(failed))
                        .replace("${CASES_SKIPPED}", String.valueOf(skipped))
                        .replace("${CASES_DETAILS}", detailsBuilder));

        ReportManagerHelper.logExecutionSummary(String.valueOf(total), String.valueOf(passed), String.valueOf(failed), String.valueOf(skipped));
    }

    public enum ExecutionSummaryReportStatus {
        FAILED,
        SKIPPED
    }

    public enum ExecutionSummaryReportStatusIcon {
        FAILED("&#10060; "), SKIPPED("&#128679; ");

        private String value;

        ExecutionSummaryReportStatusIcon(String type) {
            this.value = type;
        }

        public String getValue() {
            return value;
        }
    }


}
