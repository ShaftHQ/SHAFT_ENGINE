package io.github.shafthq.shaft.tools.io;

import com.shaft.driver.SHAFT;
import io.github.shafthq.shaft.tools.support.HTMLHelper;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;

public class ExecutionSummaryReport {

    public static void generateExecutionSummaryReport(int passed, int failed, int skipped) {
        int total = passed + failed + skipped;
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
                        .replace("${CASES_SKIPPED}", String.valueOf(skipped)));
    }

}
