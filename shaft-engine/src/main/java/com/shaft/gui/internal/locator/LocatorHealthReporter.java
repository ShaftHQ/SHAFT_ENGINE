package com.shaft.gui.internal.locator;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;

import java.io.File;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

/**
 * Collects run-level locator timing and flakiness signals when enabled.
 */
public final class LocatorHealthReporter {
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final ConcurrentMap<String, LocatorStats> LOCATORS = new ConcurrentHashMap<>();
    private static final DateTimeFormatter FILENAME_FORMATTER =
            DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss-SSSS-a").withZone(ZoneId.systemDefault());
    private static final Pattern SECRET_ASSIGNMENT = Pattern.compile(
            "(?i)(password|passwd|secret|token|api[-_]?key|authorization|cookie)\\s*[:=]\\s*[^\\s,;]+");
    private static final Pattern LONG_TOKEN = Pattern.compile("\\b[a-zA-Z0-9_-]{32,}\\b");

    private LocatorHealthReporter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns whether locator health collection is enabled for the current thread.
     *
     * @return {@code true} when collection is enabled
     */
    public static boolean isEnabled() {
        return SHAFT.Properties.reporting.locatorHealthReportEnabled();
    }

    /**
     * Clears all collected run-level locator health data.
     */
    public static void reset() {
        LOCATORS.clear();
    }

    /**
     * Records one completed locator lookup.
     *
     * @param locator locator used for the lookup
     * @param elapsedMillis elapsed lookup time in milliseconds
     * @param pollingAttempts number of polling attempts performed
     * @param foundElementCount number of elements found by the locator
     * @param timedOut whether the lookup timed out
     * @param staleRetries number of stale-element retries observed
     */
    public static void recordLookup(
            By locator,
            long elapsedMillis,
            int pollingAttempts,
            int foundElementCount,
            boolean timedOut,
            int staleRetries) {
        if (!isEnabled() || locator == null) {
            return;
        }
        statsFor(locator).recordLookup(elapsedMillis, pollingAttempts, foundElementCount, timedOut, staleRetries);
    }

    /**
     * Records one SHAFT Heal recovery attempt for the original locator.
     *
     * @param originalLocator locator that needed recovery
     * @param accepted whether the recovery was accepted
     */
    public static void recordHealingAttempt(By originalLocator, boolean accepted) {
        if (!isEnabled() || originalLocator == null) {
            return;
        }
        statsFor(originalLocator).recordHealingAttempt(accepted);
    }

    /**
     * Writes and attaches the end-of-run report, then returns a build failure when configured.
     *
     * @return assertion error when warnings should fail the run; otherwise {@code null}
     */
    public static AssertionError reportAndGetFailure() {
        if (!isEnabled() || LOCATORS.isEmpty()) {
            reset();
            return null;
        }
        String json = buildSummaryJson();
        String html = buildSummaryHtml();
        writeReports(json, html);
        ReportManager.log(buildSummaryText());
        ReportManagerHelper.attach("json", "Locator Health Report", json);
        ReportManagerHelper.attach("html", "Locator Health Report", html);
        AssertionError failure = warningFailure();
        reset();
        return failure;
    }

    static String buildSummaryJson() {
        ObjectNode root = MAPPER.createObjectNode();
        ArrayNode locators = root.putArray("locators");
        sortedStats().forEach(stats -> stats.appendJson(locators.addObject()));
        root.put("warningCount", warningCount());
        return root.toPrettyString();
    }

    static String buildSummaryHtml() {
        StringBuilder rows = new StringBuilder();
        sortedStats().forEach(stats -> rows.append(stats.toHtmlRow()));
        return """
                <!doctype html>
                <html lang="en">
                <head>
                  <meta charset="utf-8">
                  <title>SHAFT Locator Health Report</title>
                  <style>
                    body{font-family:Arial,sans-serif;margin:24px;color:#1f2933}
                    table{border-collapse:collapse;width:100%%}
                    th,td{border:1px solid #d9e2ec;padding:8px;text-align:left}
                    th{background:#f0f4f8}
                    .warn{color:#b42318;font-weight:700}
                  </style>
                </head>
                <body>
                  <h1>SHAFT Locator Health Report</h1>
                  <p>Warnings: <span class="warn">%d</span></p>
                  <table>
                    <thead><tr><th>Locator</th><th>Lookups</th><th>Avg ms</th><th>P95 ms</th><th>Polls</th><th>Timeouts</th><th>Stale</th><th>Multiple</th><th>Slow</th><th>Healing</th></tr></thead>
                    <tbody>%s</tbody>
                  </table>
                </body>
                </html>
                """.formatted(warningCount(), rows);
    }

    private static LocatorStats statsFor(By locator) {
        String locatorText = safe(locator);
        return LOCATORS.computeIfAbsent(locatorText, LocatorStats::new);
    }

    private static String buildSummaryText() {
        StringBuilder summary = new StringBuilder("Locator health summary");
        sortedStats().forEach(stats -> summary.append(System.lineSeparator())
                .append("- ").append(stats.locator)
                .append(": lookups=").append(stats.lookupCount.get())
                .append(", avgMs=").append(String.format(Locale.ROOT, "%.1f", stats.averageLookupMillis()))
                .append(", p95Ms=").append(stats.percentileMillis(95))
                .append(", warnings=").append(stats.warningCount()));
        return summary.toString();
    }

    private static AssertionError warningFailure() {
        int warnings = warningCount();
        if (warnings > 0 && SHAFT.Properties.reporting.failOnLocatorHealthWarnings()) {
            return new AssertionError("Locator health warnings were found: " + warnings + ".");
        }
        return null;
    }

    private static int warningCount() {
        return LOCATORS.values().stream().mapToInt(LocatorStats::warningCount).sum();
    }

    private static List<LocatorStats> sortedStats() {
        return LOCATORS.values().stream()
                .sorted(Comparator.comparing(stats -> stats.locator))
                .toList();
    }

    private static void writeReports(String json, String html) {
        String folder = reportFolder();
        String timestamp = FILENAME_FORMATTER.format(Instant.now());
        FileActions fileActions = FileActions.getInstance(true);
        fileActions.createFolder(folder);
        fileActions.writeToFile(folder, "LocatorHealthReport_" + timestamp + ".json", json);
        fileActions.writeToFile(folder, "LocatorHealthReport_" + timestamp + ".html", html);
    }

    private static String reportFolder() {
        String base = SHAFT.Properties.paths.executionSummaryReport();
        String separator = File.separator;
        if (base.endsWith("/") || base.endsWith("\\")) {
            return base + "locator-health" + separator;
        }
        return base + separator + "locator-health" + separator;
    }

    private static String safe(By locator) {
        return safe(locator == null ? "" : locator.toString());
    }

    private static String safe(String value) {
        String sanitized = value == null ? "" : value;
        sanitized = SECRET_ASSIGNMENT.matcher(sanitized).replaceAll("$1=[REDACTED]");
        sanitized = LONG_TOKEN.matcher(sanitized).replaceAll("[REDACTED]");
        return sanitized.replaceAll("[\\p{Cntrl}&&[^\\r\\n\\t]]", "").trim();
    }

    private static String htmlEscape(String value) {
        return value.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#39;");
    }

    private static final class LocatorStats {
        private final String locator;
        private final ConcurrentLinkedQueue<Long> lookupMillis = new ConcurrentLinkedQueue<>();
        private final AtomicInteger lookupCount = new AtomicInteger();
        private final AtomicInteger pollingAttempts = new AtomicInteger();
        private final AtomicInteger timeoutCount = new AtomicInteger();
        private final AtomicInteger staleElementRetries = new AtomicInteger();
        private final AtomicInteger multipleElementMatches = new AtomicInteger();
        private final AtomicInteger slowLookups = new AtomicInteger();
        private final AtomicInteger healingAttempts = new AtomicInteger();
        private final AtomicInteger acceptedHealings = new AtomicInteger();
        private final AtomicInteger rejectedHealings = new AtomicInteger();

        private LocatorStats(String locator) {
            this.locator = locator;
        }

        private void recordLookup(
                long elapsedMillis,
                int pollingAttempts,
                int foundElementCount,
                boolean timedOut,
                int staleRetries) {
            long safeElapsedMillis = Math.max(0, elapsedMillis);
            lookupMillis.add(safeElapsedMillis);
            lookupCount.incrementAndGet();
            this.pollingAttempts.addAndGet(Math.max(0, pollingAttempts));
            if (timedOut) {
                timeoutCount.incrementAndGet();
            }
            if (foundElementCount > 1) {
                multipleElementMatches.incrementAndGet();
            }
            if (staleRetries > 0) {
                staleElementRetries.addAndGet(staleRetries);
            }
            if (safeElapsedMillis >= SHAFT.Properties.reporting.slowLocatorThresholdMillis()) {
                slowLookups.incrementAndGet();
                ReportManager.logDiscrete("Slow locator lookup: \"" + locator + "\" took "
                        + safeElapsedMillis + "ms.", Level.WARN);
            }
        }

        private void recordHealingAttempt(boolean accepted) {
            healingAttempts.incrementAndGet();
            if (accepted) {
                acceptedHealings.incrementAndGet();
            } else {
                rejectedHealings.incrementAndGet();
            }
        }

        private double averageLookupMillis() {
            return lookupMillis.stream().mapToLong(Long::longValue).average().orElse(0);
        }

        private long percentileMillis(int percentile) {
            List<Long> sorted = lookupMillis.stream().sorted().toList();
            if (sorted.isEmpty()) {
                return 0;
            }
            int index = (int) Math.ceil(percentile / 100.0 * sorted.size()) - 1;
            return sorted.get(Math.max(0, Math.min(index, sorted.size() - 1)));
        }

        private int warningCount() {
            return timeoutCount.get()
                    + staleElementRetries.get()
                    + multipleElementMatches.get()
                    + slowLookups.get()
                    + rejectedHealings.get();
        }

        private void appendJson(ObjectNode locatorNode) {
            locatorNode.put("locator", locator);
            locatorNode.put("lookupCount", lookupCount.get());
            locatorNode.put("averageLookupMillis", averageLookupMillis());
            locatorNode.put("p95LookupMillis", percentileMillis(95));
            locatorNode.put("pollingAttempts", pollingAttempts.get());
            locatorNode.put("timeoutCount", timeoutCount.get());
            locatorNode.put("staleElementRetries", staleElementRetries.get());
            locatorNode.put("multipleElementMatches", multipleElementMatches.get());
            locatorNode.put("slowLookups", slowLookups.get());
            locatorNode.put("healingAttempts", healingAttempts.get());
            locatorNode.put("acceptedHealings", acceptedHealings.get());
            locatorNode.put("rejectedHealings", rejectedHealings.get());
            locatorNode.put("warningCount", warningCount());
        }

        private String toHtmlRow() {
            return "<tr><td>" + htmlEscape(locator) + "</td><td>" + lookupCount.get()
                    + "</td><td>" + String.format(Locale.ROOT, "%.1f", averageLookupMillis())
                    + "</td><td>" + percentileMillis(95)
                    + "</td><td>" + pollingAttempts.get()
                    + "</td><td>" + timeoutCount.get()
                    + "</td><td>" + staleElementRetries.get()
                    + "</td><td>" + multipleElementMatches.get()
                    + "</td><td>" + slowLookups.get()
                    + "</td><td>" + acceptedHealings.get() + "/" + rejectedHealings.get()
                    + "</td></tr>";
        }
    }
}
