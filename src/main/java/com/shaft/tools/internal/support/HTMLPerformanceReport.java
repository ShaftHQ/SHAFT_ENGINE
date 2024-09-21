package com.shaft.tools.internal.support;

/**
 * @author Kyrillos Nageh
 */

import com.shaft.api.RequestBuilder;

import java.io.FileWriter;
import java.io.IOException;
import java.util.DoubleSummaryStatistics;
import java.util.List;
import java.util.Map;

public class HTMLPerformanceReport {

    // Method to generate the HTML performance report with CSS and Table
    public static void generatePerformanceReport() {
        StringBuilder reportBuilder = new StringBuilder();

        // HTML and CSS structure
        reportBuilder.append("<html><head><style>")
                .append("body {font-family: Arial, sans-serif; margin: 40px;}")
                .append("h1 {color: #333;}")
                .append("table {width: 100%; border-collapse: collapse;}")
                .append("th, td {border: 1px solid #ddd; padding: 8px;}")
                .append("th {background-color: #f2f2f2; text-align: center;}")
                .append("tr:nth-child(even) {background-color: #f9f9f9;}")
                .append("tr:hover {background-color: #f1f1f1;}")
                .append("</style></head><body>");

        reportBuilder.append("<h1>API Performance Report</h1>");
        reportBuilder.append("<table>");
        reportBuilder.append("<tr><th>Endpoint</th><th>Total Requests</th>")
                .append("<th>Min Response Time (ms)</th>")
                .append("<th>Max Response Time (ms)</th>")
                .append("<th>Avg Response Time (ms)</th></tr>");

        // Access the performance data stored in RequestBuilder
        Map<String, List<Double>> performanceData = RequestBuilder.getPerformanceData();

        // Iterate over each endpoint and compute the statistics
        performanceData.forEach((endpoint, times) -> {
            DoubleSummaryStatistics stats = times.stream()
                    .mapToDouble(Double::doubleValue)
                    .summaryStatistics();

            // Add the data as a row in the table
            reportBuilder.append(String.format(
                    "<tr><td>%s</td><td>%d</td><td>%.2f</td><td>%.2f</td><td>%.2f</td></tr>",
                    endpoint,
                    stats.getCount(),    // Number of requests
                    stats.getMin(),      // Minimum response time
                    stats.getMax(),      // Maximum response time
                    stats.getAverage()   // Average response time
            ));
        });

        // Finalize the HTML table and content
        reportBuilder.append("</table>");
        reportBuilder.append("</body></html>");

        // Save the HTML report to a file
        saveReportToFile(reportBuilder.toString(), "src/test/resources/performance_report.html");
    }

    // Helper method to save the report to an HTML file
    private static void saveReportToFile(String reportContent, String fileName) {
        try (FileWriter fileWriter = new FileWriter(fileName)) {
            fileWriter.write(reportContent);
            System.out.println("Report successfully written to " + fileName);
        } catch (IOException e) {
            e.printStackTrace();
            System.err.println("Failed to write the report to the file.");
        }
    }
}
