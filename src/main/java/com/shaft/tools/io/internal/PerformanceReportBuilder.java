package com.shaft.tools.io.internal;

/**
 * @author Kyrillos Nageh
 */

import java.util.DoubleSummaryStatistics;
import java.util.List;
import java.util.Map;

public class PerformanceReportBuilder {

    // Method to build the HTML report content
    public String buildReport(Map<String, List<Double>> performanceData) {
        StringBuilder reportBuilder = new StringBuilder();

        // Adding HTML and CSS styling
        reportBuilder.append("<html><head><style>")
                .append("body {font-family: Arial, sans-serif; margin: 40px;}")
                .append("h1 {color: #333;}")
                .append("table {width: 100%; border-collapse: collapse;}")
                .append("th, td {border: 1px solid #ddd; padding: 8px; text-align: left;}")
                .append("th {background-color: #f2f2f2; text-align: center;}")
                .append("tr:nth-child(even) {background-color: #f9f9f9;}")
                .append("tr:hover {background-color: #f1f1f1;}")
                .append("</style></head><body>");

        // Adding title
        reportBuilder.append("<h1>API Performance Report</h1>");

        // Creating the table with headers
        reportBuilder.append("<table>");
        reportBuilder.append("<tr><th>Endpoint</th><th>Total Requests</th>")
                .append("<th>Min Response Time (ms)</th>")
                .append("<th>Max Response Time (ms)</th>")
                .append("<th>Avg Response Time (ms)</th></tr>");

        // Iterate over performance data and append rows
        performanceData.forEach((endpoint, times) -> {
            DoubleSummaryStatistics stats = times.stream()
                    .mapToDouble(Double::doubleValue)
                    .summaryStatistics();

            // Add each endpoint's performance data as a row
            reportBuilder.append(String.format(
                    "<tr><td>%s</td><td>%d</td><td>%.2f</td><td>%.2f</td><td>%.2f</td></tr>",
                    endpoint,
                    stats.getCount(),    // Total requests
                    stats.getMin(),      // Minimum response time
                    stats.getMax(),      // Maximum response time
                    stats.getAverage()   // Average response time
            ));
        });

        // Closing table and HTML
        reportBuilder.append("</table>");
        reportBuilder.append("</body></html>");

        // Return the complete HTML content
        return reportBuilder.toString();
    }
}
