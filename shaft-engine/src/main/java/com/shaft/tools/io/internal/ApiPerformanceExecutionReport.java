package com.shaft.tools.io.internal;

import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.PerformanceReportHTMLHelper;
import com.shaft.tools.io.ReportManager;
import org.apache.logging.log4j.Level;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.DoubleSummaryStatistics;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.Objects;

/**
 * Utility class for generating HTML-based API performance execution reports.
 * Aggregates per-endpoint response-time statistics collected during
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
    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
    private static final String BUDGET_METRIC = "p95";
    private static final DateTimeFormatter READABLE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault());
    private static final DateTimeFormatter FILENAME_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss-SSS").withZone(ZoneId.systemDefault());

    /**
     * Creates a new API performance execution report helper instance.
     */
    public ApiPerformanceExecutionReport() {
        super();
    }

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
        AssertionError failure = generatePerformanceReportAndGetFailure(performanceData, startTime, endTime);
        if (failure != null) {
            throw failure;
        }
    }

    /**
     * Calculates per-endpoint performance statistics, writes enabled reports, and returns the
     * configured budget failure without throwing it.
     *
     * @param performanceData a map where each key is a normalized API endpoint path and each value
     *                        is a list of individual response times in milliseconds
     * @param startTime       the epoch-millisecond timestamp when the test suite began
     * @param endTime         the epoch-millisecond timestamp when the test suite finished
     * @return assertion error when API performance budgets should fail the run; otherwise {@code null}
     */
    public static AssertionError generatePerformanceReportAndGetFailure(
            Map<String, List<Double>> performanceData, long startTime, long endTime) {
        boolean reportEnabled = SHAFT.Properties.performance.isEnablePerformanceReport();
        Map<String, Double> endpointBudgets = parseEndpointBudgets(SHAFT.Properties.performance.apiEndpointPerformanceBudgets());
        if (!reportEnabled && endpointBudgets.isEmpty()) {
            ReportManager.logDiscrete("Performance report generation is disabled.");
            return null;
        }

        boolean failOnBudgetViolation = SHAFT.Properties.performance.failOnApiPerformanceBudgetViolation();
        List<EndpointStats> endpointStats = buildEndpointStats(performanceData, endpointBudgets, failOnBudgetViolation);
        logBudgetViolations(endpointStats);
        AssertionError failure = budgetFailure(endpointStats);

        if (reportEnabled) {
            writeReports(endpointStats, startTime, endTime, failOnBudgetViolation);
        }
        return failure;
    }

    private static void writeReports(List<EndpointStats> endpointStats, long startTime, long endTime,
                                     boolean failOnBudgetViolation) {
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
        endpointStats.forEach(stats -> performanceSummary.append(stats.toHtmlRow()));

        // Generate the final HTML content using the buildHtml method
        String htmlReport = PerformanceReportHTMLHelper.buildHtml(formattedStartTime, formattedEndTime, formattedExecutionTime, performanceSummary.toString());
        String jsonReport = buildJsonReport(formattedStartTime, formattedEndTime, formattedExecutionTime,
                endpointStats, failOnBudgetViolation);

        // Define the path where the report will be saved
        //String reportPath = SHAFT.Properties.paths.performanceReport() + "/PerformanceReport_" + getTimestamp() + ".html";
        //saveHtmlToFile(reportPath, htmlReport);
        String timestamp = getTimestamp();
        fileActionsSession.writeToFile(SHAFT.Properties.paths.performanceReportPath(), "PerformanceReport_" + timestamp + ".html", htmlReport);
        fileActionsSession.writeToFile(SHAFT.Properties.paths.performanceReportPath(), "PerformanceReport_" + timestamp + ".json", jsonReport);
        // Pass the formatted data to PHTMLHelper for HTML generation
        //PHTMLHelper.generateHtmlReport(formattedStartTime, formattedEndTime, formattedExecutionTime, performanceSummary.toString(), reportPath);
    }

    private static List<EndpointStats> buildEndpointStats(Map<String, List<Double>> performanceData,
                                                          Map<String, Double> endpointBudgets,
                                                          boolean failOnBudgetViolation) {
        if (performanceData == null || performanceData.isEmpty()) {
            return List.of();
        }
        return performanceData.entrySet().stream()
                .filter(entry -> entry.getKey() != null)
                .map(entry -> EndpointStats.from(entry.getKey(), copyAndSort(entry.getValue()),
                        endpointBudgets.get(entry.getKey()), failOnBudgetViolation))
                .sorted(Comparator.comparing(EndpointStats::endpoint))
                .toList();
    }

    private static List<Double> copyAndSort(List<Double> times) {
        if (times == null || times.isEmpty()) {
            return List.of();
        }
        synchronized (times) {
            return times.stream()
                    .filter(Objects::nonNull)
                    .sorted()
                    .toList();
        }
    }

    private static Map<String, Double> parseEndpointBudgets(String configuredBudgets) {
        if (configuredBudgets == null || configuredBudgets.isBlank()) {
            return Map.of();
        }
        Map<String, Double> budgets = new LinkedHashMap<>();
        for (String entry : configuredBudgets.split("[,;\\r\\n]+")) {
            String trimmedEntry = entry.trim();
            if (trimmedEntry.isEmpty()) {
                continue;
            }
            int separator = trimmedEntry.lastIndexOf('=');
            if (separator <= 0 || separator == trimmedEntry.length() - 1) {
                ReportManager.logDiscrete("Ignoring invalid API performance budget entry: \"" + trimmedEntry + "\".", Level.WARN);
                continue;
            }

            String endpoint = trimmedEntry.substring(0, separator).trim();
            String budgetValue = trimmedEntry.substring(separator + 1).trim();
            try {
                double budgetMillis = Double.parseDouble(budgetValue);
                if (endpoint.isBlank() || budgetMillis < 0 || !Double.isFinite(budgetMillis)) {
                    ReportManager.logDiscrete("Ignoring invalid API performance budget entry: \"" + trimmedEntry + "\".", Level.WARN);
                    continue;
                }
                budgets.put(endpoint, budgetMillis);
            } catch (NumberFormatException e) {
                ReportManager.logDiscrete("Ignoring invalid API performance budget entry: \"" + trimmedEntry + "\".", Level.WARN);
            }
        }
        return budgets;
    }

    private static void logBudgetViolations(List<EndpointStats> endpointStats) {
        endpointStats.stream()
                .filter(EndpointStats::hasBudgetViolation)
                .forEach(stats -> ReportManager.logDiscrete(
                        "API performance budget " + stats.budgetStatus().name().toLowerCase(Locale.ROOT)
                                + ": " + stats.budgetViolationMessage() + ".",
                        Level.WARN));
    }

    private static AssertionError budgetFailure(List<EndpointStats> endpointStats) {
        List<String> failures = endpointStats.stream()
                .filter(stats -> stats.budgetStatus() == BudgetStatus.FAIL)
                .map(EndpointStats::budgetViolationMessage)
                .toList();
        if (failures.isEmpty()) {
            return null;
        }
        return new AssertionError("API performance budget violations: " + String.join("; ", failures) + ".");
    }

    private static String buildJsonReport(String formattedStartTime, String formattedEndTime,
                                          String formattedExecutionTime, List<EndpointStats> endpointStats,
                                          boolean failOnBudgetViolation) {
        ObjectNode root = JSON_MAPPER.createObjectNode();
        root.put("startTime", formattedStartTime);
        root.put("endTime", formattedEndTime);
        root.put("executionTime", formattedExecutionTime);
        root.put("budgetMetric", BUDGET_METRIC);
        root.put("failOnBudgetViolation", failOnBudgetViolation);
        var endpoints = root.putArray("endpoints");
        endpointStats.forEach(stats -> stats.appendJson(endpoints.addObject()));
        return root.toPrettyString();
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

    private static String formatMillis(double millis) {
        return String.format(Locale.ROOT, "%.2f", millis);
    }

    private static String htmlEscape(String value) {
        return value.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#39;");
    }

    private record EndpointStats(String endpoint, long requests, double minMillis, double maxMillis,
                                 double averageMillis, double p50Millis, double p90Millis,
                                 double p95Millis, double p99Millis, Double budgetMillis,
                                 BudgetStatus budgetStatus) {
        private static EndpointStats from(String endpoint, List<Double> sortedTimes, Double budgetMillis,
                                          boolean failOnBudgetViolation) {
            DoubleSummaryStatistics stats = sortedTimes.stream().mapToDouble(Double::doubleValue).summaryStatistics();
            long requests = stats.getCount();
            double minMillis = requests == 0 ? 0 : stats.getMin();
            double maxMillis = requests == 0 ? 0 : stats.getMax();
            double averageMillis = requests == 0 ? 0 : stats.getAverage();
            double p50Millis = percentile(sortedTimes, 50);
            double p90Millis = percentile(sortedTimes, 90);
            double p95Millis = percentile(sortedTimes, 95);
            double p99Millis = percentile(sortedTimes, 99);
            BudgetStatus budgetStatus = budgetStatus(p95Millis, budgetMillis, failOnBudgetViolation);
            return new EndpointStats(endpoint, requests, minMillis, maxMillis, averageMillis, p50Millis,
                    p90Millis, p95Millis, p99Millis, budgetMillis, budgetStatus);
        }

        private static double percentile(List<Double> sortedTimes, int percentile) {
            if (sortedTimes.isEmpty()) {
                return 0;
            }
            int index = (int) Math.ceil(percentile / 100d * sortedTimes.size()) - 1;
            return sortedTimes.get(Math.max(0, Math.min(index, sortedTimes.size() - 1)));
        }

        private static BudgetStatus budgetStatus(double p95Millis, Double budgetMillis,
                                                 boolean failOnBudgetViolation) {
            if (budgetMillis == null) {
                return BudgetStatus.NOT_CONFIGURED;
            }
            if (p95Millis <= budgetMillis) {
                return BudgetStatus.PASS;
            }
            return failOnBudgetViolation ? BudgetStatus.FAIL : BudgetStatus.WARN;
        }

        private boolean hasBudgetViolation() {
            return budgetStatus == BudgetStatus.WARN || budgetStatus == BudgetStatus.FAIL;
        }

        private String budgetViolationMessage() {
            return endpoint + " p95 " + formatMillis(p95Millis) + "ms exceeded budget "
                    + formatMillis(budgetMillis) + "ms";
        }

        private String toHtmlRow() {
            return "<tr><td>" + htmlEscape(endpoint) + "</td>"
                    + "<td>" + requests + "</td>"
                    + "<td>" + formatMillis(maxMillis) + "</td>"
                    + "<td>" + formatMillis(minMillis) + "</td>"
                    + "<td>" + formatMillis(averageMillis) + "</td>"
                    + "<td>" + formatMillis(p50Millis) + "</td>"
                    + "<td>" + formatMillis(p90Millis) + "</td>"
                    + "<td>" + formatMillis(p95Millis) + "</td>"
                    + "<td>" + formatMillis(p99Millis) + "</td>"
                    + "<td>" + (budgetMillis == null ? "" : formatMillis(budgetMillis)) + "</td>"
                    + "<td>" + budgetStatus + "</td></tr>";
        }

        private void appendJson(ObjectNode endpointNode) {
            endpointNode.put("endpoint", endpoint);
            endpointNode.put("requests", requests);
            endpointNode.put("minMillis", minMillis);
            endpointNode.put("maxMillis", maxMillis);
            endpointNode.put("averageMillis", averageMillis);
            endpointNode.put("p50Millis", p50Millis);
            endpointNode.put("p90Millis", p90Millis);
            endpointNode.put("p95Millis", p95Millis);
            endpointNode.put("p99Millis", p99Millis);
            if (budgetMillis == null) {
                endpointNode.putNull("budgetMillis");
            } else {
                endpointNode.put("budgetMillis", budgetMillis);
            }
            endpointNode.put("budgetMetric", BUDGET_METRIC);
            endpointNode.put("budgetStatus", budgetStatus.name());
        }
    }

    private enum BudgetStatus {
        PASS, WARN, FAIL, NOT_CONFIGURED
    }
}
