package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.PerformanceReportHTMLHelper;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.DoubleSummaryStatistics;
import java.util.List;
import java.util.Map;

public class ApiPerformanceExecutionReport {

    // Fetch properties from the Reporting interface

    // Method to calculate performance data and pass it to PHTMLHelper
    public static void generatePerformanceReport(Map<String, List<Double>> performanceData, long startTime, long endTime) {
        // Check if performance report generation is enabled
        if (!SHAFT.Properties.performance.isEnablePerformanceReport()) {
            System.out.println("Performance report generation is disabled.");
            return;
        }

        var fileActionsSession = FileActions.getInstance(true);

        fileActionsSession.createFolder(SHAFT.Properties.paths.performanceReportPath());
        // Get the custom report path
        //String reportPath = SHAFT.Properties.paths.performanceReport();

        // Calculate execution time
        long executionTime = endTime - startTime;

        // Format the start and end time for display
        String formattedStartTime = formatTimestamp(startTime);
        String formattedEndTime = formatTimestamp(endTime);
        String formattedExecutionTime = formatExecutionTime(executionTime);

        // Generate performance summary statistics for each endpoint
        StringBuilder performanceSummary = new StringBuilder();
        performanceData.forEach((endpoint, times) -> {
            DoubleSummaryStatistics stats = times.stream().mapToDouble(Double::doubleValue).summaryStatistics();
            performanceSummary.append("<tr>");
            performanceSummary.append("<td>").append(endpoint).append("</td>");
            performanceSummary.append("<td>").append(stats.getCount()).append("</td>");
            performanceSummary.append("<td>").append(String.format("%.2f", stats.getMax())).append("</td>");
            performanceSummary.append("<td>").append(String.format("%.2f", stats.getMin())).append("</td>");
            performanceSummary.append("<td>").append(String.format("%.2f", stats.getAverage())).append("</td>");
            performanceSummary.append("</tr>");
        });

        // Generate the final HTML content using the buildHtml method
        String htmlReport = PerformanceReportHTMLHelper.buildHtml(formattedStartTime, formattedEndTime, formattedExecutionTime, performanceSummary.toString());

        // Define the path where the report will be saved
        //String reportPath = SHAFT.Properties.paths.performanceReport() + "/PerformanceReport_" + getTimestamp() + ".html";
        //saveHtmlToFile(reportPath, htmlReport);
        fileActionsSession.writeToFile(SHAFT.Properties.paths.performanceReportPath(), "PerformanceReport_" + getTimestamp() + ".html", htmlReport);
        // Pass the formatted data to PHTMLHelper for HTML generation
        //PHTMLHelper.generateHtmlReport(formattedStartTime, formattedEndTime, formattedExecutionTime, performanceSummary.toString(), reportPath);
    }

    // Helper method to format the timestamp into a readable date
    private static String formatTimestamp(long timestamp) {
        return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date(timestamp));
    }

    // Helper method to format the execution time into minutes and seconds
    private static String formatExecutionTime(long executionTimeMillis) {
        long seconds = (executionTimeMillis / 1000) % 60;
        long minutes = (executionTimeMillis / (1000 * 60)) % 60;
        return String.format("%d minutes, %d seconds", minutes, seconds);
    }

    // Method to generate a timestamp for the report filename
    private static String getTimestamp() {
        return new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa").format(new Date());
    }
}
