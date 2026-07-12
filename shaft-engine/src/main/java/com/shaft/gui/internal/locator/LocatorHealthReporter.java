package com.shaft.gui.internal.locator;

import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.tools.internal.support.ReportHtmlTheme;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;

import java.io.File;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

/**
 * Collects run-level locator timing and flakiness signals when enabled.
 */
public final class LocatorHealthReporter {
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final ConcurrentMap<String, LocatorStats> LOCATORS = new ConcurrentHashMap<>();
    private static final DateTimeFormatter FILENAME_FORMATTER =
            DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss-SSS").withZone(ZoneId.systemDefault());
    private static final Pattern SECRET_ASSIGNMENT = Pattern.compile(
            "(?i)(password|passwd|secret|token|api[-_]?key|authorization|cookie)\\s*[:=]\\s*[^\\s,;]+");
    private static final Pattern LONG_TOKEN = Pattern.compile("\\b[a-zA-Z0-9_-]{32,}\\b");
    private static final Pattern XPATH_INDEX = Pattern.compile("\\[[0-9]+]");
    private static final Pattern GENERATED_ID = Pattern.compile(
            "(?i)(^|.*[#:\\s_-])(react-select-\\d+|ember\\d+|[a-f0-9]{8,}|[a-z]+[_-]?\\d{4,}).*");

    private LocatorHealthReporter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns whether locator health collection is enabled for the current thread.
     *
     * @return {@code true} when collection is enabled
     */
    public static boolean isEnabled() {
        return SHAFT.Properties.reporting.locatorHealthReportEnabled()
                || SHAFT.Properties.reporting.locatorHealthEnabled();
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
        recordHealingAttempt(originalLocator, accepted, null, -1);
    }

    /**
     * Records one SHAFT Heal recovery attempt for the original locator.
     *
     * @param originalLocator locator that needed recovery
     * @param accepted whether the recovery was accepted
     * @param selectedLocator accepted replacement locator
     * @param confidence provider confidence from {@code 0.0} to {@code 1.0}, or negative when unknown
     */
    public static void recordHealingAttempt(By originalLocator, boolean accepted, By selectedLocator, double confidence) {
        if (!isEnabled() || originalLocator == null) {
            return;
        }
        statsFor(originalLocator).recordHealingAttempt(accepted, selectedLocator, confidence);
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
        if (SHAFT.Properties.reporting.locatorHealthAttachDashboard()) {
            ReportManagerHelper.attach("html", "Locator Health Report", html);
        }
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

    /**
     * Returns the current locator-health JSON snapshot without resetting collected data.
     *
     * @return current locator-health summary JSON
     */
    public static String currentSummaryJson() {
        return buildSummaryJson();
    }

    static String buildSummaryHtml() {
        StringBuilder rows = new StringBuilder();
        sortedStats().forEach(stats -> rows.append(stats.toHtmlRow()));
        return """
                <!doctype html>
                <html lang="en">
                <head>
                  <meta charset="utf-8">
                  <meta name="viewport" content="width=device-width, initial-scale=1">
                  <title>SHAFT Locator Health Report</title>
                  <style>
                """ + ReportHtmlTheme.style() + """
                  </style>
                </head>
                <body>
                  <div class="report-shell">
                    <header class="report-header">
                      <div class="report-header-inner">
                        <span class="brand-mark">S</span>
                        <div>
                          <h1>SHAFT Locator Health Report</h1>
                          <p class="subtitle">Locator stability, timing, and healing signals</p>
                        </div>
                      </div>
                    </header>
                    <main class="report-main">
                      <section class="panel">
                        <h2>Summary</h2>
                        <div class="metric-grid">
                          <div class="metric-card"><div class="metric-label">Warnings</div><div class="metric-value"><span class="status-chip %s">%d</span></div></div>
                        </div>
                      </section>
                      <section class="panel">
                        <h2>Locator Details</h2>
                        <div class="table-wrap">
                          <table>
                            <thead><tr><th>Locator</th><th>Score</th><th>Recommendation</th><th>Lookups</th><th>Avg ms</th><th>P95 ms</th><th>Polls</th><th>Timeouts</th><th>Stale</th><th>Multiple</th><th>Slow</th><th>Healing</th></tr></thead>
                            <tbody>%s</tbody>
                          </table>
                        </div>
                      </section>
                    </main>
                  </div>
                </body>
                </html>
                """.formatted(warningCount() == 0 ? "passed" : "warn", warningCount(), rows);
    }

    private static LocatorStats statsFor(By locator) {
        String locatorText = safe(locator);
        return LOCATORS.computeIfAbsent(locatorText, LocatorStats::new);
    }

    private static String buildSummaryText() {
        StringBuilder summary = new StringBuilder("Locator health summary");
        sortedStats().forEach(stats -> summary.append(System.lineSeparator())
                .append("- ").append(stats.locator)
                .append(": score=").append(stats.healthScore())
                .append(", lookups=").append(stats.lookupCount.get())
                .append(", avgMs=").append(String.format(Locale.ROOT, "%.1f", stats.averageLookupMillis()))
                .append(", p95Ms=").append(stats.percentileMillis(95))
                .append(", warnings=").append(stats.warningCount())
                .append(stats.recommendations().isEmpty()
                        ? ""
                        : ", recommendation=" + stats.recommendations().getFirst()));
        return summary.toString();
    }

    private static AssertionError warningFailure() {
        int failBelowScore = SHAFT.Properties.reporting.locatorHealthFailBelowScore();
        if (failBelowScore >= 0) {
            boolean failingScore = LOCATORS.values().stream()
                    .anyMatch(stats -> stats.healthScore() < failBelowScore);
            if (failingScore) {
                return new AssertionError("Locator health score below configured threshold: " + failBelowScore + ".");
            }
        }
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
                .sorted(Comparator.comparingInt(LocatorStats::healthScore).thenComparing(stats -> stats.locator))
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
        private final AtomicInteger uniqueElementMatches = new AtomicInteger();
        private final AtomicInteger noElementMatches = new AtomicInteger();
        private final AtomicInteger timeoutCount = new AtomicInteger();
        private final AtomicInteger staleElementRetries = new AtomicInteger();
        private final AtomicInteger multipleElementMatches = new AtomicInteger();
        private final AtomicInteger slowLookups = new AtomicInteger();
        private final AtomicInteger healingAttempts = new AtomicInteger();
        private final AtomicInteger acceptedHealings = new AtomicInteger();
        private final AtomicInteger rejectedHealings = new AtomicInteger();
        private final AtomicReference<String> lastHealedLocator = new AtomicReference<>("");
        private final AtomicReference<Double> lastHealingConfidence = new AtomicReference<>();

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
            if (foundElementCount == 0) {
                noElementMatches.incrementAndGet();
            } else if (foundElementCount == 1) {
                uniqueElementMatches.incrementAndGet();
            }
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

        private void recordHealingAttempt(boolean accepted, By selectedLocator, double confidence) {
            healingAttempts.incrementAndGet();
            if (accepted) {
                acceptedHealings.incrementAndGet();
                if (selectedLocator != null) {
                    lastHealedLocator.set(safe(selectedLocator));
                }
                if (confidence >= 0) {
                    lastHealingConfidence.set(Math.max(0, Math.min(1, confidence)));
                }
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
                    + rejectedHealings.get()
                    + (healthScore() < SHAFT.Properties.reporting.locatorHealthWarnBelowScore() ? 1 : 0);
        }

        private int healthScore() {
            int score = 100;
            score -= Math.min(35, noElementMatches.get() * 20);
            score -= Math.min(25, multipleElementMatches.get() * 15);
            score -= Math.min(20, staleElementRetries.get() * 10);
            score -= Math.min(15, slowLookups.get() * 10);
            score -= Math.min(20, rejectedHealings.get() * 10);
            score -= Math.min(40, selectorSmells().size() * 10);
            return Math.max(0, score);
        }

        private double rate(int value) {
            int total = lookupCount.get();
            return total == 0 ? 0 : (double) value / total;
        }

        private List<String> selectorSmells() {
            List<String> smells = new ArrayList<>();
            String normalized = locator.toLowerCase(Locale.ROOT);
            if (normalized.startsWith("by.xpath: /")) {
                smells.add("absolute XPath");
            }
            if (normalized.startsWith("by.xpath:") && XPATH_INDEX.matcher(locator).find()) {
                smells.add("index-heavy XPath");
            }
            if ((normalized.startsWith("by.id:") || normalized.startsWith("by.cssselector:"))
                    && GENERATED_ID.matcher(locator).matches()) {
                smells.add("generated ID");
            }
            if (normalized.startsWith("by.xpath:")
                    && (normalized.contains("text()") || normalized.contains("normalize-space()"))
                    && !normalized.contains("@")) {
                smells.add("text-only selector");
            }
            if (normalized.startsWith("by.cssselector:") && cssDepth() >= 5) {
                smells.add("deep CSS chain");
            }
            return smells;
        }

        private int cssDepth() {
            String css = locator.substring("By.cssSelector:".length()).trim();
            if (css.isBlank()) {
                return 0;
            }
            return css.split("\\s*>\\s*|\\s+").length;
        }

        private List<String> recommendations() {
            Set<String> recommendations = new LinkedHashSet<>();
            for (String smell : selectorSmells()) {
                switch (smell) {
                    case "absolute XPath" ->
                            recommendations.add("Prefer role/name, stable id/name, or data-testid over absolute XPath.");
                    case "index-heavy XPath" ->
                            recommendations.add("Avoid volatile XPath indexes; target a stable attribute or accessible name.");
                    case "generated ID" ->
                            recommendations.add("Replace generated IDs with data-testid or a stable name.");
                    case "text-only selector" ->
                            recommendations.add("Pair text with role, data-testid, or stable container context.");
                    case "deep CSS chain" ->
                            recommendations.add("Simplify deep CSS chains and add data-testid to the target element.");
                    default -> {
                    }
                }
            }
            if (noElementMatches.get() > 0) {
                recommendations.add("Add data-testid or use a stable id/name so the locator resolves consistently.");
            }
            if (multipleElementMatches.get() > 0) {
                recommendations.add("Make the locator unique with role/name, stable id/name, or data-testid.");
            }
            if (staleElementRetries.get() > 0) {
                recommendations.add("Re-query dynamic elements immediately before interaction.");
            }
            if (slowLookups.get() > 0) {
                recommendations.add("Simplify the selector or use a stable attribute to reduce lookup time.");
            }
            if (!lastHealedLocator.get().isBlank()) {
                recommendations.add("Review the accepted healed locator: " + lastHealedLocator.get() + ".");
            }
            return List.copyOf(recommendations);
        }

        private void appendJson(ObjectNode locatorNode) {
            locatorNode.put("locator", locator);
            locatorNode.put("healthScore", healthScore());
            locatorNode.put("lookupCount", lookupCount.get());
            locatorNode.put("averageLookupMillis", averageLookupMillis());
            locatorNode.put("p95LookupMillis", percentileMillis(95));
            locatorNode.put("pollingAttempts", pollingAttempts.get());
            locatorNode.put("uniqueMatchRate", rate(uniqueElementMatches.get()));
            locatorNode.put("noMatchRate", rate(noElementMatches.get()));
            locatorNode.put("multiMatchRate", rate(multipleElementMatches.get()));
            locatorNode.put("staleRate", rate(staleElementRetries.get()));
            locatorNode.put("timeoutCount", timeoutCount.get());
            locatorNode.put("noMatchCount", noElementMatches.get());
            locatorNode.put("staleElementRetries", staleElementRetries.get());
            locatorNode.put("multipleElementMatches", multipleElementMatches.get());
            locatorNode.put("slowLookups", slowLookups.get());
            locatorNode.put("healingAttempts", healingAttempts.get());
            locatorNode.put("acceptedHealings", acceptedHealings.get());
            locatorNode.put("rejectedHealings", rejectedHealings.get());
            if (!lastHealedLocator.get().isBlank()) {
                locatorNode.put("lastHealedLocator", lastHealedLocator.get());
            }
            Double confidence = lastHealingConfidence.get();
            if (confidence != null) {
                locatorNode.put("lastHealingConfidence", confidence);
            }
            ArrayNode smells = locatorNode.putArray("selectorSmells");
            selectorSmells().forEach(smells::add);
            ArrayNode recommendationNodes = locatorNode.putArray("recommendations");
            recommendations().forEach(recommendationNodes::add);
            locatorNode.put("warningCount", warningCount());
        }

        private String toHtmlRow() {
            List<String> recommendations = recommendations();
            String riskClass = healthScore() < SHAFT.Properties.reporting.locatorHealthWarnBelowScore()
                    ? " class=\"warn\""
                    : "";
            return "<tr" + riskClass + "><td>" + htmlEscape(locator) + "</td><td>" + healthScore()
                    + "</td><td>" + htmlEscape(recommendations.isEmpty() ? "" : recommendations.getFirst())
                    + "</td><td>" + lookupCount.get()
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
