package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.PerformanceReportHTMLHelper;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.DoubleSummaryStatistics;
import java.util.List;
import java.util.Map;

/**
 * Utility class for generating HTML-based API performance execution reports.
 * Aggregates per-endpoint response-time statistics (count, min, max, average) collected during
 * a test run and writes a self-contained HTML report file to the path configured by
 * {@code SHAFT.Properties.paths.performanceReportPath()}.
 *
 * <p>Report generation is guarded by the {@code SHAFT.Properties.performance.isEnablePerformanceReport()}
 * flag and is skipped when that flag is {@code false}.
 *
 * <p>This class is not intended for direct use in test code; it is invoked by the SHAFT
 * API performance-tracking infrastructure.
 */
public class ApiPerformanceExecutionReport {
    private static final DateTimeFormatter READABLE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault());
    private static final DateTimeFormatter FILENAME_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss-SSSS-a").withZone(ZoneId.systemDefault());

    // Fetch properties from the Reporting interface

    /**
     * Calculates per-endpoint performance statistics from the supplied data and writes a
     * timestamped HTML report to the configured performance report directory.
     *
     * <p>The method is a no-op when
     * {@code SHAFT.Properties.performance.isEnablePerformanceReport()} returns {@code false}.
     *
     * <p>Example:
     * <pre>{@code
     * Map<String, List<Double>> data = new HashMap<>();
     * data.put("/api/users", Arrays.asList(120.5, 98.3, 135.0));
     * long start = System.currentTimeMillis();
     * // ... run tests ...
     * long end = System.currentTimeMillis();
     * ApiPerformanceExecutionReport.generatePerformanceReport(data, start, end);
     * }</pre>
     *
     * @param performanceData a map where each key is an API endpoint path and each value is a
     *                        list of individual response times in milliseconds
     * @param startTime       the epoch-millisecond timestamp when the test suite began
     * @param endTime         the epoch-millisecond timestamp when the test suite finished
     */
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
        return READABLE_FORMATTER.format(Instant.ofEpochMilli(timestamp));
    }

    // Helper method to format the execution time into minutes and seconds
    private static String formatExecutionTime(long executionTimeMillis) {
        long seconds = (executionTimeMillis / 1000) % 60;
        long minutes = (executionTimeMillis / (1000 * 60)) % 60;
        return String.format("%d minutes, %d seconds", minutes, seconds);
    }

    // Method to generate a timestamp for the report filename
    private static String getTimestamp() {
        return FILENAME_FORMATTER.format(ZonedDateTime.now());
    }
}
