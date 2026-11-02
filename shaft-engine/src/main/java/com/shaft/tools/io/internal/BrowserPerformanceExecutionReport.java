package com.shaft.tools.io.internal;

import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.internal.support.ReportHtmlTheme;
import org.apache.logging.log4j.Level;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.DoubleSummaryStatistics;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Aggregates Playwright browser action and page-load timings into performance budget reports.
 */
public final class BrowserPerformanceExecutionReport {
    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
    private static final String BUDGET_METRIC = "p95";
    private static final DateTimeFormatter READABLE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault());
    private static final DateTimeFormatter FILENAME_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss-SSS").withZone(ZoneId.systemDefault());
    private static final Map<String, List<Double>> BROWSER_ACTION_DATA = new ConcurrentHashMap<>();
    private static final Map<String, List<Double>> PAGE_LOAD_DATA = new ConcurrentHashMap<>();

    private BrowserPerformanceExecutionReport() {
        throw new IllegalStateException("Utility class");
    }

    public static Map<String, List<Double>> getBrowserActionPerformanceData() {
        return BROWSER_ACTION_DATA;
    }

    public static Map<String, List<Double>> getPageLoadPerformanceData() {
        return PAGE_LOAD_DATA;
    }

    public static void reset() {
        BROWSER_ACTION_DATA.clear();
        PAGE_LOAD_DATA.clear();
    }

    public static void recordBrowserAction(String actionName, long durationNanos) {
        record(BROWSER_ACTION_DATA, actionName, durationNanos);
    }

    public static void recordPageLoad(String pageName, long durationNanos) {
        record(PAGE_LOAD_DATA, pageName, durationNanos);
    }

    public static boolean shouldRecord() {
        return SHAFT.Properties.performance.isEnablePerformanceReport()
                || !isBlank(SHAFT.Properties.performance.browserActionPerformanceBudgets())
                || !isBlank(SHAFT.Properties.performance.pageLoadPerformanceBudgets());
    }

    public static void generatePerformanceReport(long startTime, long endTime) {
        AssertionError failure = generatePerformanceReportAndGetFailure(startTime, endTime);
        if (failure != null) {
            throw failure;
        }
    }

    public static AssertionError generatePerformanceReportAndGetFailure(long startTime, long endTime) {
        Map<String, Double> actionBudgets = parseBudgets(
                SHAFT.Properties.performance.browserActionPerformanceBudgets(),
                "browser action");
        Map<String, Double> pageLoadBudgets = parseBudgets(
                SHAFT.Properties.performance.pageLoadPerformanceBudgets(),
                "page-load");
        boolean hasData = !BROWSER_ACTION_DATA.isEmpty() || !PAGE_LOAD_DATA.isEmpty();
        if (!hasData && actionBudgets.isEmpty() && pageLoadBudgets.isEmpty()) {
            return null;
        }

        boolean failOnBudgetViolation = SHAFT.Properties.performance.failOnBrowserPerformanceBudgetViolation();
        List<MetricStats> actionStats = buildStats(BROWSER_ACTION_DATA, actionBudgets, failOnBudgetViolation);
        List<MetricStats> pageLoadStats = buildStats(PAGE_LOAD_DATA, pageLoadBudgets, failOnBudgetViolation);
        logBudgetViolations("browser action", actionStats);
        logBudgetViolations("page-load", pageLoadStats);
        AssertionError failure = budgetFailure(actionStats, pageLoadStats);

        if (SHAFT.Properties.performance.isEnablePerformanceReport()) {
            writeReports(actionStats, pageLoadStats, startTime, endTime, failOnBudgetViolation);
        }
        return failure;
    }

    private static void record(Map<String, List<Double>> data, String key, long durationNanos) {
        if (!shouldRecord() || isBlank(key)) {
            return;
        }
        double durationMillis = Math.max(0, durationNanos) / 1_000_000d;
        data.computeIfAbsent(key, ignored -> Collections.synchronizedList(new ArrayList<>()))
                .add(durationMillis);
    }

    private static List<MetricStats> buildStats(Map<String, List<Double>> data, Map<String, Double> budgets,
                                                boolean failOnBudgetViolation) {
        return data.entrySet().stream()
                .filter(entry -> !isBlank(entry.getKey()))
                .map(entry -> MetricStats.from(entry.getKey(), copyAndSort(entry.getValue()),
                        budgetFor(budgets, entry.getKey()), failOnBudgetViolation))
                .sorted(Comparator.comparing(MetricStats::metric))
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

    private static Double budgetFor(Map<String, Double> budgets, String key) {
        Double exact = budgets.get(key);
        return exact == null ? budgets.get("*") : exact;
    }

    private static Map<String, Double> parseBudgets(String configuredBudgets, String budgetType) {
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
                logInvalidBudget(trimmedEntry, budgetType);
                continue;
            }

            String metric = trimmedEntry.substring(0, separator).trim();
            String budgetValue = trimmedEntry.substring(separator + 1).trim();
            try {
                double budgetMillis = Double.parseDouble(budgetValue);
                if (metric.isBlank() || budgetMillis < 0 || !Double.isFinite(budgetMillis)) {
                    logInvalidBudget(trimmedEntry, budgetType);
                    continue;
                }
                budgets.put(metric, budgetMillis);
            } catch (NumberFormatException e) {
                logInvalidBudget(trimmedEntry, budgetType);
            }
        }
        return budgets;
    }

    private static void logInvalidBudget(String entry, String budgetType) {
        ReportManager.logDiscrete("Ignoring invalid " + budgetType + " performance budget entry: \"" + entry + "\".", Level.WARN);
    }

    private static void logBudgetViolations(String budgetType, List<MetricStats> stats) {
        stats.stream()
                .filter(MetricStats::hasBudgetViolation)
                .forEach(metricStats -> ReportManager.logDiscrete(
                        budgetType + " performance budget " + metricStats.budgetStatus().name().toLowerCase(Locale.ROOT)
                                + ": " + metricStats.budgetViolationMessage() + ".",
                        Level.WARN));
    }

    private static AssertionError budgetFailure(List<MetricStats> actionStats, List<MetricStats> pageLoadStats) {
        List<String> failures = new ArrayList<>();
        actionStats.stream()
                .filter(stats -> stats.budgetStatus() == BudgetStatus.FAIL)
                .map(MetricStats::budgetViolationMessage)
                .forEach(failures::add);
        pageLoadStats.stream()
                .filter(stats -> stats.budgetStatus() == BudgetStatus.FAIL)
                .map(MetricStats::budgetViolationMessage)
                .forEach(failures::add);
        if (failures.isEmpty()) {
            return null;
        }
        return new AssertionError("Browser performance budget violations: " + String.join("; ", failures) + ".");
    }

    private static void writeReports(List<MetricStats> actionStats, List<MetricStats> pageLoadStats,
                                     long startTime, long endTime, boolean failOnBudgetViolation) {
        FileActions fileActionsSession = FileActions.getInstance(true);
        fileActionsSession.createFolder(SHAFT.Properties.paths.performanceReportPath());

        String formattedStartTime = formatTimestamp(startTime);
        String formattedEndTime = formatTimestamp(endTime);
        String formattedExecutionTime = formatExecutionTime(endTime - startTime);
        String htmlReport = buildHtmlReport(formattedStartTime, formattedEndTime, formattedExecutionTime,
                actionStats, pageLoadStats);
        String jsonReport = buildJsonReport(formattedStartTime, formattedEndTime, formattedExecutionTime,
                actionStats, pageLoadStats, failOnBudgetViolation);
        String timestamp = getTimestamp();
        fileActionsSession.writeToFile(SHAFT.Properties.paths.performanceReportPath(), "BrowserPerformanceReport_" + timestamp + ".html", htmlReport);
        fileActionsSession.writeToFile(SHAFT.Properties.paths.performanceReportPath(), "BrowserPerformanceReport_" + timestamp + ".json", jsonReport);
    }

    private static String buildJsonReport(String formattedStartTime, String formattedEndTime,
                                          String formattedExecutionTime, List<MetricStats> actionStats,
                                          List<MetricStats> pageLoadStats, boolean failOnBudgetViolation) {
        ObjectNode root = JSON_MAPPER.createObjectNode();
        root.put("reportType", "browser");
        root.put("startTime", formattedStartTime);
        root.put("endTime", formattedEndTime);
        root.put("executionTime", formattedExecutionTime);
        root.put("budgetMetric", BUDGET_METRIC);
        root.put("failOnBudgetViolation", failOnBudgetViolation);
        appendStats(root.putArray("browserActions"), actionStats);
        appendStats(root.putArray("pageLoads"), pageLoadStats);
        return root.toPrettyString();
    }

    private static void appendStats(ArrayNode target, List<MetricStats> stats) {
        stats.forEach(metricStats -> metricStats.appendJson(target.addObject()));
    }

    private static String buildHtmlReport(String formattedStartTime, String formattedEndTime,
                                          String formattedExecutionTime, List<MetricStats> actionStats,
                                          List<MetricStats> pageLoadStats) {
        return "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\">"
                + "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                + "<title>SHAFT Browser Performance Report</title><style>"
                + ReportHtmlTheme.style()
                + "</style></head><body><div class=\"report-shell\">"
                + "<header class=\"report-header\"><div class=\"report-header-inner\">"
                + "<span class=\"brand-mark\">S</span><div><h1>SHAFT Browser Performance Report</h1>"
                + "<p class=\"subtitle\">" + htmlEscape(formattedStartTime) + " - "
                + htmlEscape(formattedEndTime) + " · " + htmlEscape(formattedExecutionTime)
                + "</p></div></div></header><main class=\"report-main\">"
                + "<section class=\"panel\"><h2>Summary</h2><div class=\"metric-grid\">"
                + "<div class=\"metric-card\"><div class=\"metric-label\">Browser Actions</div><div class=\"metric-value\">"
                + actionStats.size() + "</div></div>"
                + "<div class=\"metric-card\"><div class=\"metric-label\">Page Loads</div><div class=\"metric-value\">"
                + pageLoadStats.size() + "</div></div>"
                + "</div></section>"
                + table("Browser Actions", actionStats)
                + table("Page Loads", pageLoadStats)
                + "</main></div></body></html>";
    }

    private static String table(String title, List<MetricStats> stats) {
        StringBuilder rows = new StringBuilder();
        stats.forEach(metricStats -> rows.append(metricStats.toHtmlRow()));
        return "<section class=\"panel\"><h2>" + htmlEscape(title) + "</h2><div class=\"table-wrap\"><table><thead><tr>"
                + "<th>Metric</th><th>Samples</th><th>Max (ms)</th><th>Min (ms)</th><th>Average (ms)</th>"
                + "<th>P50 (ms)</th><th>P90 (ms)</th><th>P95 (ms)</th><th>P99 (ms)</th>"
                + "<th>Budget (ms)</th><th>Budget Status</th></tr></thead><tbody>"
                + rows
                + "</tbody></table></div></section>";
    }

    private static String formatTimestamp(long timestamp) {
        return READABLE_FORMATTER.format(Instant.ofEpochMilli(timestamp));
    }

    private static String formatExecutionTime(long executionTimeMillis) {
        long seconds = (executionTimeMillis / 1000) % 60;
        long minutes = (executionTimeMillis / (1000 * 60)) % 60;
        return String.format("%d minutes, %d seconds", minutes, seconds);
    }

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

    private static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }

    private record MetricStats(String metric, long samples, double minMillis, double maxMillis,
                               double averageMillis, double p50Millis, double p90Millis,
                               double p95Millis, double p99Millis, Double budgetMillis,
                               BudgetStatus budgetStatus) {
        private static MetricStats from(String metric, List<Double> sortedTimes, Double budgetMillis,
                                        boolean failOnBudgetViolation) {
            DoubleSummaryStatistics stats = sortedTimes.stream().mapToDouble(Double::doubleValue).summaryStatistics();
            long samples = stats.getCount();
            double minMillis = samples == 0 ? 0 : stats.getMin();
            double maxMillis = samples == 0 ? 0 : stats.getMax();
            double averageMillis = samples == 0 ? 0 : stats.getAverage();
            double p50Millis = percentile(sortedTimes, 50);
            double p90Millis = percentile(sortedTimes, 90);
            double p95Millis = percentile(sortedTimes, 95);
            double p99Millis = percentile(sortedTimes, 99);
            BudgetStatus budgetStatus = budgetStatus(p95Millis, budgetMillis, failOnBudgetViolation);
            return new MetricStats(metric, samples, minMillis, maxMillis, averageMillis, p50Millis,
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
            return metric + " p95 " + formatMillis(p95Millis) + "ms exceeded budget "
                    + formatMillis(budgetMillis) + "ms";
        }

        private String toHtmlRow() {
            return "<tr><td>" + htmlEscape(metric) + "</td>"
                    + "<td>" + samples + "</td>"
                    + "<td>" + formatMillis(maxMillis) + "</td>"
                    + "<td>" + formatMillis(minMillis) + "</td>"
                    + "<td>" + formatMillis(averageMillis) + "</td>"
                    + "<td>" + formatMillis(p50Millis) + "</td>"
                    + "<td>" + formatMillis(p90Millis) + "</td>"
                    + "<td>" + formatMillis(p95Millis) + "</td>"
                    + "<td>" + formatMillis(p99Millis) + "</td>"
                    + "<td>" + (budgetMillis == null ? "" : formatMillis(budgetMillis)) + "</td>"
                    + "<td><span class=\"status-chip " + ReportHtmlTheme.statusClass(budgetStatus.name()) + "\">"
                    + budgetStatus + "</span></td></tr>";
        }

        private void appendJson(ObjectNode node) {
            node.put("metric", metric);
            node.put("samples", samples);
            node.put("minMillis", minMillis);
            node.put("maxMillis", maxMillis);
            node.put("averageMillis", averageMillis);
            node.put("p50Millis", p50Millis);
            node.put("p90Millis", p90Millis);
            node.put("p95Millis", p95Millis);
            node.put("p99Millis", p99Millis);
            if (budgetMillis == null) {
                node.putNull("budgetMillis");
            } else {
                node.put("budgetMillis", budgetMillis);
            }
            node.put("budgetMetric", BUDGET_METRIC);
            node.put("budgetStatus", budgetStatus.name());
        }
    }

    private enum BudgetStatus {
        PASS, WARN, FAIL, NOT_CONFIGURED
    }
}
