package io.github.shafthq.shaft.tools.io;

import com.shaft.driver.SHAFT;
import io.github.shafthq.shaft.tools.support.HTMLHelper;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;

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

    public static void generateExecutionSummaryReport(int passed, int failed, int skipped, long startTime, long endTime) {
        int total = passed + failed + skipped;

        StringBuilder detailsBuilder = new StringBuilder();
        casesDetails.forEach((key, value) -> detailsBuilder.append(String.format(HTMLHelper.EXECUTION_SUMMARY_DETAILS_FORMAT.getValue(), key, value.get(0), value.get(1), value.get(2))));

        new SHAFT.CLI().file().writeToFile(System.getProperty("executionSummaryReportFolderPath"),
                "ExecutionSummaryReport_" + new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa").format(System.currentTimeMillis()) + ".html",
                HTMLHelper.EXECUTION_SUMMARY.getValue()
                        .replace("${DATE}", new SimpleDateFormat("dd/MM/yyyy").format(endTime))
                        .replace("${START_TIME}", new SimpleDateFormat("HH:mm:ss").format(startTime))
                        .replace("${END_TIME}", new SimpleDateFormat("HH:mm:ss").format(endTime))
                        .replace("${TOTAL_TIME}", executionDuration(startTime, endTime))
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

    private static String executionDuration(long startTime, long endTime) {
        long durationWithMillis = TimeUnit.MILLISECONDS.toMillis(endTime - startTime);

        String duration = "";
        if (durationWithMillis > 0 && durationWithMillis < 1000) {
            duration = durationWithMillis + " millis";
        } else if (durationWithMillis >= 1000 && durationWithMillis < 60000) {
            duration = String.format("%02d sec, %02d millis",
                    TimeUnit.MILLISECONDS.toSeconds(durationWithMillis),
                    TimeUnit.MILLISECONDS.toMillis(durationWithMillis) - TimeUnit.SECONDS.toMillis(TimeUnit.MILLISECONDS.toSeconds(durationWithMillis))
            );
        } else if (durationWithMillis >= 60000 && durationWithMillis < 3600000) {
            duration = String.format("%02d min, %02d sec",
                    TimeUnit.MILLISECONDS.toMinutes(durationWithMillis),
                    TimeUnit.MILLISECONDS.toSeconds(durationWithMillis) - TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(durationWithMillis))
            );
        } else if (durationWithMillis >= 3600000) {
            duration = String.format("%02d hr, %02d min",
                    TimeUnit.MILLISECONDS.toHours(durationWithMillis),
                    TimeUnit.MILLISECONDS.toMinutes(durationWithMillis) - TimeUnit.HOURS.toMinutes(TimeUnit.MILLISECONDS.toHours(durationWithMillis))
            );
        }
        return duration;
    }

    public enum Status {
        FAILED,
        SKIPPED
    }

    public enum StatusIcon {
        FAILED("&#10060; "), SKIPPED("&#128679; ");

        private String value;

        StatusIcon(String type) {
            this.value = type;
        }

        public String getValue() {
            return value;
        }
    }


}
