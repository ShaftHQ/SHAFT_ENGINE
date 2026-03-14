package com.shaft.validation.accessibility;

import com.deque.html.axecore.results.Results;
import com.deque.html.axecore.results.Rule;
import com.deque.html.axecore.selenium.AxeBuilder;
import io.qameta.allure.Allure;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Internal helper that drives WCAG-based accessibility analysis using the
 * <a href="https://github.com/dequelabs/axe-core">axe-core</a> engine via
 * {@link com.deque.html.axecore.selenium.AxeBuilder}.
 *
 * <p>The class exposes static methods that run an axe scan against a live
 * {@link WebDriver} session, persist the results as JSON and an interactive HTML
 * report, and attach the report to the current Allure test run as an attachment.
 *
 * <p>Typical usage:
 * <pre>{@code
 * // Quick fire-and-forget scan with default configuration
 * AccessibilityHelper.analyzePageAccessibility(driver, "HomePage");
 *
 * // Scan and obtain a structured result object for in-test assertions
 * AccessibilityResult result = AccessibilityHelper.analyzePageAccessibilityAndSave(
 *         driver, "CheckoutPage", true);
 * System.out.println("Accessibility score: " + result.getAccessibilityScore() + "%");
 * }</pre>
 *
 * <p><strong>Thread safety:</strong> {@link #DATE_FORMAT} and
 * {@link #DISPLAY_DATE_FORMAT} are immutable {@link DateTimeFormatter} instances, so formatting
 * operations are safe to call from parallel test threads. However, concurrent scans
 * of the same {@code pageName} within the same second may produce file-name collisions
 * in the shared reports directory; use distinct page names or unique report directories
 * when running parallel scans on the same page.
 *
 * @see AccessibilityConfig
 * @see AccessibilityResult
 */
public class AccessibilityHelper {
    private static final Logger logger = LogManager.getLogger(AccessibilityHelper.class);
    private static final DateTimeFormatter DATE_FORMAT =
            DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss");
    private static final DateTimeFormatter DISPLAY_DATE_FORMAT =
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    // DOM stability wait settings
    private static final int DOM_STABLE_MILLIS = 1500; // Wait until DOM stable
    private static final int DOM_TIMEOUT_SECONDS = 15; // Maximum wait time
    private static final int DOM_CHECK_INTERVAL_MILLIS = 500; // Sleep interval between checks
    private static final int PAGE_LOAD_TIMEOUT_SECONDS = 30;

    /**
     * This is a utility class and cannot be instantiated.
     *
     * @throws IllegalStateException always
     */
    private AccessibilityHelper() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Mutable configuration bean for an accessibility scan with fluent-builder setters.
     *
     * <p>All setters follow a fluent (builder) pattern and return {@code this},
     * allowing convenient one-liner configuration:
     * <pre>{@code
     * AccessibilityConfig config = new AccessibilityConfig()
     *         .setTags(List.of("wcag21aa"))
     *         .setContext("main")
     *         .setReportsDir("target/a11y-reports/")
     *         .setIncludePasses(false);
     * AccessibilityHelper.analyzePageAccessibility(driver, "Home", config);
     * }</pre>
     *
     * <p>Default values:
     * <ul>
     *   <li>{@code tags} – {@code ["wcag2a","wcag2aa","wcag21a","wcag21aa","best-practice"]}</li>
     *   <li>{@code reportsDir} – {@code "accessibility-reports/"}</li>
     *   <li>{@code includePasses} – {@code true}</li>
     *   <li>{@code context} – {@code "body, header, main, footer"}</li>
     * </ul>
     */
    public static class AccessibilityConfig {
        private List<String> tags = new ArrayList<>(List.of("wcag2a", "wcag2aa", "wcag21a", "wcag21aa", "best-practice"));
        private String reportsDir = "accessibility-reports/";
        private boolean includePasses = true;
        private String context = "body, header, main, footer";

        /**
         * Creates a new {@code AccessibilityConfig} with default settings.
         * Use the fluent setters to customise the configuration after construction.
         */
        public AccessibilityConfig() {
        }

        /**
         * Returns the axe-core WCAG tag filters applied during the scan.
         *
         * @return mutable list of axe rule-set tag identifiers (e.g. {@code "wcag21aa"})
         */
        public List<String> getTags() { return tags; }

        /**
         * Replaces the axe-core tag filters for this scan.
         *
         * <pre>{@code config.setTags(List.of("wcag21aa", "best-practice")); }</pre>
         *
         * @param tags non-null list of axe rule-set tag identifiers
         * @return this instance for method chaining
         * @throws NullPointerException if {@code tags} is {@code null}
         */
        public AccessibilityConfig setTags(List<String> tags) {
            this.tags = Objects.requireNonNull(tags, "tags must not be null");
            return this;
        }

        /**
         * Returns the file-system directory where JSON and HTML reports will be written.
         *
         * @return directory path string (includes trailing {@code /})
         */
        public String getReportsDir() { return reportsDir; }

        /**
         * Sets the output directory for generated accessibility reports.
         *
         * <pre>{@code config.setReportsDir("target/accessibility/"); }</pre>
         *
         * @param reportsDir path to the output directory; created automatically if absent
         * @return this instance for method chaining
         */
        public AccessibilityConfig setReportsDir(String reportsDir) { this.reportsDir = reportsDir; return this; }

        /**
         * Returns whether passing rules are included in the generated report.
         *
         * @return {@code true} if passing rules appear in the report; {@code false} otherwise
         */
        public boolean isIncludePasses() { return includePasses; }

        /**
         * Controls whether passing axe rules are written into the report.
         *
         * <pre>{@code config.setIncludePasses(false); // omit passes for a leaner report }</pre>
         *
         * @param includePasses {@code true} to include passing rules; {@code false} to suppress them
         * @return this instance for method chaining
         */
        public AccessibilityConfig setIncludePasses(boolean includePasses) { this.includePasses = includePasses; return this; }

        /**
         * Returns the CSS selector string used to scope the axe scan to specific page regions.
         *
         * @return CSS selector string (e.g. {@code "body, header, main, footer"})
         */
        public String getContext() { return context; }

        /**
         * Restricts the axe scan to the DOM subtrees matched by the given CSS selector.
         *
         * <pre>{@code config.setContext("#content, nav"); }</pre>
         *
         * @param context CSS selector identifying the elements to include in the scan
         * @return this instance for method chaining
         */
        public AccessibilityConfig setContext(String context) { this.context = context; return this; }
    }


    /**
     * Runs a WCAG accessibility scan against the currently loaded page using the
     * default {@link AccessibilityConfig} and writes both a JSON data file and an
     * interactive HTML report to disk.  The HTML report is also attached to the
     * current Allure test step automatically.
     *
     * <pre>{@code
     * AccessibilityHelper.analyzePageAccessibility(driver, "HomePage");
     * }</pre>
     *
     * @param driver   active {@link WebDriver} instance pointing at the page to scan;
     *                 must not be {@code null}
     * @param pageName human-readable label used in the report file name and Allure
     *                 attachment title; must not be blank
     */
    public static void analyzePageAccessibility(WebDriver driver, String pageName) {
        analyzePageAccessibility(driver, pageName, new AccessibilityConfig());
    }

    /**
     * Runs a WCAG accessibility scan using the supplied {@link AccessibilityConfig}
     * and writes both a JSON data file and an interactive HTML report to disk.
     * The HTML report is also attached to the current Allure test step automatically.
     *
     * <p>The method blocks until {@code document.readyState === "complete"} and the
     * DOM has been stable for {@value #DOM_STABLE_MILLIS} ms before invoking axe.
     *
     * <pre>{@code
     * AccessibilityConfig config = new AccessibilityConfig()
     *         .setTags(List.of("wcag21aa"))
     *         .setReportsDir("target/a11y/");
     * AccessibilityHelper.analyzePageAccessibility(driver, "CheckoutPage", config);
     * }</pre>
     *
     * @param driver   active {@link WebDriver} instance pointing at the page to scan;
     *                 must not be {@code null}
     * @param pageName human-readable label used in the report file name and Allure
     *                 attachment title; must not be blank
     * @param config   scan configuration; must not be {@code null}
     * @throws IllegalArgumentException if {@code driver}, {@code pageName}, or
     *                                  {@code config} fails validation
     */
    public static void analyzePageAccessibility(WebDriver driver, String pageName, AccessibilityConfig config) {

        if (driver == null) throw new IllegalArgumentException("WebDriver cannot be null");
        if (pageName == null || pageName.trim().isEmpty()) throw new IllegalArgumentException("Page name cannot be empty");
        if (config == null) throw new IllegalArgumentException("Configuration cannot be null");

        try {
            if (driver.getWindowHandle() == null) {
                logger.error("WebDriver is invalid or closed for page: {}", pageName);
                return;
            }

            if (driver instanceof RemoteWebDriver remoteDriver) {
                Capabilities caps = remoteDriver.getCapabilities();
                logger.info("Running accessibility scan on browser: {} {}, URL: {}",
                        caps.getBrowserName(), caps.getBrowserVersion(), driver.getCurrentUrl());
            } else {
                logger.info("Driver type: {}", driver.getClass().getSimpleName());
            }

            new WebDriverWait(driver, Duration.ofSeconds(PAGE_LOAD_TIMEOUT_SECONDS))
                    .until(webDriver -> ((JavascriptExecutor) webDriver)
                            .executeScript("return document.readyState").equals("complete"));

            logger.info("Page fully loaded, starting accessibility scan for: {}", pageName);

            waitForDomStability(driver, DOM_STABLE_MILLIS, DOM_TIMEOUT_SECONDS);

            AxeBuilder axeBuilder = new AxeBuilder()
                    .withTags(config.getTags())
                    .include(config.getContext());
            axeBuilder.setOptions("{ \"resultTypes\": [\"violations\", \"incomplete\", \"inapplicable\", \"passes\"] }");

            Results results = axeBuilder.analyze(driver);
            JSONObject responseJSON = convertResultsToJSON(results);

            String timestamp = DATE_FORMAT.format(ZonedDateTime.now());
            Files.createDirectories(Paths.get(config.getReportsDir()));

            String jsonReportPath = config.getReportsDir() + "AccessibilityJSON_" + pageName + "_" + timestamp + ".json";
            String htmlReportPath = config.getReportsDir() + "AccessibilityReport_" + pageName + "_" + timestamp + ".html";

            writeJsonResults(jsonReportPath, responseJSON);
            generateEnhancedHTMLReport(responseJSON, pageName, htmlReportPath, config,driver);

            logger.info("Accessibility report generated at: {}", htmlReportPath);
            logger.info("JSON report generated at: {}", jsonReportPath);
            printDetailedSummary(responseJSON, pageName);

            try {
                Path reportPath = Paths.get(htmlReportPath);
                if (Files.exists(reportPath)) {
                    Path allureDir = Paths.get("allure-results", "accessibility");
                    Files.createDirectories(allureDir);
                    try {
                        Path target = allureDir.resolve(reportPath.getFileName());
                        Files.copy(reportPath, target, StandardCopyOption.REPLACE_EXISTING);
                    } catch (IOException ioEx) {
                        logger.warn("Could not copy report to Allure results: {}", ioEx.getMessage());
                    }
                    try (FileInputStream fis = new FileInputStream(reportPath.toFile())) {
                        Allure.addAttachment("Accessibility Report - " + pageName, "text/html", fis, ".html");
                    }
                    logger.info("Accessibility report copied to Allure results and attached for page: {}", pageName);
                } else {
                    logger.warn("Accessibility HTML report not found for Allure attachment: {}", htmlReportPath);
                }
            } catch (Exception e) {
                logger.error("Failed to copy/attach Accessibility report to Allure: {}", e.getMessage(), e);
            }
        } catch (Exception e) {
            logger.error("Accessibility scan failed for page: {}", pageName, e);
            try {
                String fallbackPath = "accessibility-reports/FailedReport_" + pageName + ".txt";
                Files.createDirectories(Paths.get("accessibility-reports/"));
                Files.writeString(Paths.get(fallbackPath),
                        "Accessibility scan failed for " + pageName + ":\n" + e.getMessage(),
                        StandardCharsets.UTF_8);
                try (FileInputStream fis = new FileInputStream(fallbackPath)) {
                    Allure.addAttachment("Accessibility Report - " + pageName,
                            "text/plain", fis, ".txt");
                }
            } catch (Exception inner) {
                logger.error("Also failed to save fallback report: {}", inner.getMessage());
            }
        }

    }

    /**
     * Convenience overload that runs an accessibility scan scoped to a specific CSS
     * context selector using default settings for all other configuration values.
     *
     * <pre>{@code
     * AccessibilityHelper.analyzePageAccessibility(driver, "Checkout", "#checkout-form");
     * }</pre>
     *
     * @param driver   active {@link WebDriver} instance; must not be {@code null}
     * @param pageName label used for the report file name and Allure attachment title
     * @param context  CSS selector restricting the scan scope (e.g. {@code "main, footer"})
     */
    public static void analyzePageAccessibility(WebDriver driver, String pageName, String context) {
        AccessibilityConfig config = new AccessibilityConfig();
        config.setContext(context);
        analyzePageAccessibility(driver, pageName, config);
    }

    /**
     * Determines whether the currently loaded page has any axe-detected WCAG violations
     * using the default rule set.
     *
     * <pre>{@code
     * if (AccessibilityHelper.hasCriticalViolations(driver, "SearchResults")) {
     *     throw new AssertionError("Accessibility violations detected");
     * }
     * }</pre>
     *
     * @param driver   active {@link WebDriver} instance; must not be {@code null}
     * @param pageName label used for log and error messages
     * @return {@code true} if at least one axe violation was found; {@code false} otherwise
     * @throws RuntimeException if the axe analysis itself fails
     */
    public static boolean hasCriticalViolations(WebDriver driver, String pageName) {
        try {
            Results results = new AxeBuilder().analyze(driver);
            return !results.getViolations().isEmpty();
        } catch (Exception e) {
            logger.error("Failed to check critical violations for page: {}", pageName, e);
            throw new RuntimeException("Failed to check critical violations", e);
        }
    }

    /**
     * Returns a breakdown of axe violations grouped by their WCAG conformance level
     * (e.g. {@code "WCAG 2.1 AA"}, {@code "Best Practice"}).
     *
     * <p>The map value represents the total number of <em>affected elements</em>
     * (nodes), not the number of distinct rules.
     *
     * <pre>{@code
     * Map<String, Integer> counts = AccessibilityHelper.getViolationsByType(driver);
     * counts.forEach((type, count) ->
     *         System.out.printf("%s: %d affected element(s)%n", type, count));
     * }</pre>
     *
     * @param driver active {@link WebDriver} instance; must not be {@code null}
     * @return map of WCAG level label to affected-element count; never {@code null}
     * @throws RuntimeException if the axe analysis fails
     */
    public static Map<String, Integer> getViolationsByType(WebDriver driver) {
        try {
            Results results = new AxeBuilder().analyze(driver);
            return results.getViolations().stream()
                    .collect(Collectors.groupingBy(
                            rule -> getWcagModelFromTags(rule.getTags()),
                            Collectors.summingInt(rule -> rule.getNodes().size())
                    ));
        } catch (Exception e) {
            logger.error("Failed to get violations by type", e);
            throw new RuntimeException("Failed to get violations by type", e);
        }
    }

    /**
     * Performs a quick axe accessibility check and returns whether the page has
     * zero violations.
     *
     * <pre>{@code
     * assertTrue(AccessibilityHelper.isAccessible(driver),
     *         "Page should have no accessibility violations");
     * }</pre>
     *
     * @param driver active {@link WebDriver} instance; must not be {@code null}
     * @return {@code true} if no axe violations were found; {@code false} if at least
     *         one violation exists or if the analysis could not be completed
     */
    public static boolean isAccessible(WebDriver driver) {
        try {
            Results results = new AxeBuilder().analyze(driver);
            return results.getViolations().isEmpty();
        } catch (Exception e) {
            logger.error("Failed to perform quick accessibility check", e);
            return false;
        }
    }

    /**
     * Runs an accessibility scan with default configuration, optionally persisting
     * reports to disk, and returns a structured {@link AccessibilityResult}.
     *
     * <pre>{@code
     * AccessibilityResult result =
     *         AccessibilityHelper.analyzePageAccessibilityAndSave(driver, "Checkout", true);
     * assertFalse(result.hasViolations(), "No accessibility violations expected");
     * }</pre>
     *
     * @param driver     active {@link WebDriver} instance; must not be {@code null}
     * @param pageName   label for the report and Allure attachment; must not be blank
     * @param saveReport {@code true} to write JSON/HTML reports and attach them to Allure
     * @return an {@link AccessibilityResult} containing violation details and score
     * @throws RuntimeException if the axe analysis fails
     */
    public static AccessibilityResult analyzePageAccessibilityAndSave(WebDriver driver, String pageName, boolean saveReport) {
        return analyzePageAccessibilityAndSave(driver, pageName, new AccessibilityConfig(), saveReport);
    }

    /**
     * Full-control overload: runs an accessibility scan with the supplied
     * {@link AccessibilityConfig}, optionally saves reports, and returns a
     * structured {@link AccessibilityResult} suitable for programmatic assertions.
     *
     * <p>The accessibility score is calculated as:
     * <pre>{@code score = (passesCount / (passesCount + violationsCount)) * 100 }</pre>
     * Incomplete checks are excluded from the score calculation.
     *
     * <pre>{@code
     * AccessibilityConfig config = new AccessibilityConfig()
     *         .setTags(List.of("wcag21aa"))
     *         .setContext("#content");
     * AccessibilityResult result =
     *         AccessibilityHelper.analyzePageAccessibilityAndSave(driver, "Home", config, true);
     * System.out.printf("Score: %.1f%% (%d violation(s))%n",
     *         result.getAccessibilityScore(), result.getViolationsCount());
     * }</pre>
     *
     * @param driver     active {@link WebDriver} instance; must not be {@code null}
     * @param pageName   label for the report and Allure attachment; must not be blank
     * @param config     scan configuration; a default instance is used when {@code null}
     * @param saveReport {@code true} to write JSON/HTML reports and attach them to Allure
     * @return an {@link AccessibilityResult} containing violation details and score
     * @throws IllegalArgumentException if the supplied context selector matches no
     *                                  elements on the page
     * @throws RuntimeException         if the axe analysis fails for any other reason
     */
    public static AccessibilityResult analyzePageAccessibilityAndSave(WebDriver driver, String pageName, AccessibilityConfig config, boolean saveReport) {
        if (driver == null) throw new IllegalArgumentException("WebDriver cannot be null");
        if (pageName == null || pageName.trim().isEmpty()) throw new IllegalArgumentException("Page name cannot be empty");
        if (config == null) config = new AccessibilityConfig();

        try {
            logger.info("Running accessibility analysis (returning result) for: {}", pageName);

            AxeBuilder axeBuilder = new AxeBuilder()
                    .withTags(config.getTags())
                    .include(config.getContext());
            if (config.isIncludePasses()) {
                axeBuilder.setOptions("{ \"resultTypes\": [\"violations\", \"incomplete\", \"inapplicable\", \"passes\"] }");
            }

            Results results = axeBuilder.analyze(driver);

            // --- Handle context not found or empty results ---
            if ((results.getViolations() == null || results.getViolations().isEmpty())
                    && (results.getIncomplete() == null || results.getIncomplete().isEmpty())
                    && (results.getPasses() == null || results.getPasses().isEmpty())
                    && (results.getInapplicable() == null || results.getInapplicable().isEmpty())) {

                throw new IllegalArgumentException(
                        "Accessibility analysis failed: the provided context selector '"
                                + config.getContext() + "' was not found or contains no elements on the page '"
                                + pageName + "'.");
            }

            // Ensure lists are not null
            if (results.getViolations() == null) results.setViolations(new ArrayList<>());
            if (results.getIncomplete() == null) results.setIncomplete(new ArrayList<>());
            if (results.getPasses() == null) results.setPasses(new ArrayList<>());
            if (results.getInapplicable() == null) results.setInapplicable(new ArrayList<>());

            JSONObject responseJSON = convertResultsToJSON(results);

            // --- ✅ Calculate accessibility score ---
            int violationsCount = results.getViolations().size();
            int passesCount = results.getPasses().size();
            int totalChecks = violationsCount + passesCount;
            double accessibilityScore = totalChecks > 0 ? ((double) passesCount / totalChecks) * 100.0 : 0.0;

            // Prepare result object
            AccessibilityResult result = new AccessibilityResult()
                    .setPageName(pageName)
                    .setViolations(results.getViolations())
                    .setViolationsCount(violationsCount)
                    .setPassesCount(passesCount)
                    .setScore(accessibilityScore)
                    .setTimestamp(LocalDateTime.now().toString());

            if (saveReport) {
                // Prepare paths
                String timestamp = DATE_FORMAT.format(ZonedDateTime.now());
                String reportsDir = config.getReportsDir();
                Files.createDirectories(Paths.get(reportsDir));

                String jsonPath = reportsDir + "AccessibilityJSON_" + pageName + "_" + timestamp + ".json";
                String htmlPath = reportsDir + "AccessibilityReport_" + pageName + "_" + timestamp + ".html";

                // Save JSON and HTML
                writeJsonResults(jsonPath, responseJSON);
                generateEnhancedHTMLReport(responseJSON, pageName, htmlPath, config,driver);

                // Attach HTML to Allure
                try (FileInputStream fis = new FileInputStream(htmlPath)) {
                    Allure.addAttachment("Accessibility Report - " + pageName, "text/html", fis, ".html");
                }
                logger.info("HTML + JSON Accessibility reports saved and attached for: {}", pageName);
            }

            logger.info("✅ Accessibility score for page '{}' = {}%", pageName, accessibilityScore);
            return result;

        } catch (Exception e) {
            logger.error("Accessibility analysis failed for page: {}", pageName, e);

            if (saveReport) {
                try {
                    // Create fallback minimal report
                    String fallbackDir = (config != null && config.getReportsDir() != null)
                            ? config.getReportsDir()
                            : "accessibility-reports/";
                    Files.createDirectories(Paths.get(fallbackDir));

                    String fallbackPath = fallbackDir + "FailedReport_" + pageName + "_" +
                            DATE_FORMAT.format(ZonedDateTime.now()) + ".txt";

                    String message = "Accessibility analysis failed for page: " + pageName +
                            "\nError Message: " + e.getMessage() +
                            "\nCause: " + (e.getCause() != null ? e.getCause().toString() : "N/A");

                    Files.writeString(Paths.get(fallbackPath), message, StandardCharsets.UTF_8);

                    // Attach to Allure
                    try (FileInputStream fis = new FileInputStream(fallbackPath)) {
                        Allure.addAttachment("Accessibility Failure Report - " + pageName,
                                "text/plain", fis, ".txt");
                    }

                    logger.info("⚠️ Fallback accessibility report generated: {}", fallbackPath);
                } catch (Exception inner) {
                    logger.error("Also failed to generate fallback report for {}: {}", pageName, inner.getMessage());
                }
            }

            // ✅ Fail the test clearly
            throw new RuntimeException("Accessibility analysis failed for page '" + pageName + "'. Check the Accessibility report for more details.", e);
        }
    }


    /**
     * Value object returned by
     * {@link AccessibilityHelper#analyzePageAccessibilityAndSave(WebDriver, String, boolean)}
     * and its overloads, carrying the aggregated outcome of a single axe accessibility scan.
     *
     * <p>Instances are constructed internally by {@code AccessibilityHelper} and exposed
     * to test code for programmatic assertions:
     * <pre>{@code
     * AccessibilityResult result =
     *         AccessibilityHelper.analyzePageAccessibilityAndSave(driver, "Home", true);
     * assertFalse(result.hasViolations(), "Expected no WCAG violations");
     * System.out.printf("Score: %.1f%% – %d violation(s)%n",
     *         result.getAccessibilityScore(), result.getViolationsCount());
     * }</pre>
     */
    public static class AccessibilityResult {
        private String pageName;
        private List<Rule> violations;
        private List<Rule> passes; // new field
        private int violationsCount;
        private String timestamp;
        private int passCount;
        private double accessibilityScore;

        /**
         * Creates a new empty {@code AccessibilityResult}.
         * Instances are normally produced by {@link AccessibilityHelper} and should not
         * be constructed directly in test code.
         */
        public AccessibilityResult() {
        }

        // getters and setters
        /**
         * Returns the overall accessibility score as a percentage in the range
         * {@code [0.0, 100.0]}.
         *
         * <p>The score is {@code (passes / (passes + violations)) × 100}.
         * A score of {@code 100.0} means no violations were detected.
         *
         * @return accessibility score percentage; {@code 0.0} when no checks were run
         */
        public double getAccessibilityScore() {
            return accessibilityScore;
        }

        /**
         * Returns the page name supplied when the scan was initiated.
         *
         * @return page label; never {@code null}
         */
        public String getPageName() {
            return pageName;
        }

        /**
         * Sets the page name for this result.
         *
         * @param pageName human-readable label for the scanned page
         * @return this instance for method chaining
         */
        public AccessibilityResult setPageName(String pageName) {
            this.pageName = pageName;
            return this;
        }

        /**
         * Returns the list of axe {@link Rule} objects representing detected violations.
         *
         * @return list of violations; may be {@code null} if not yet populated
         */
        public List<Rule> getViolations() {
            return violations;
        }

        /**
         * Sets the violations list and synchronises {@link #violationsCount} accordingly.
         *
         * @param violations list of axe {@link Rule} violations; may be {@code null}
         * @return this instance for method chaining
         */
        public AccessibilityResult setViolations(List<Rule> violations) {
            this.violations = violations;
            this.violationsCount = violations != null ? violations.size() : 0;
            return this;
        }

        /**
         * Returns the total number of axe violations found during the scan.
         *
         * @return non-negative violation count
         */
        public int getViolationsCount() {
            return violationsCount;
        }

        /**
         * Explicitly sets the violations count.  Prefer {@link #setViolations(List)}
         * which derives this value automatically from the list size.
         *
         * @param violationsCount non-negative count of violations
         * @return this instance for method chaining
         */
        public AccessibilityResult setViolationsCount(int violationsCount) {
            this.violationsCount = violationsCount;
            return this;
        }

        /**
         * Sets the number of rules that the scanned page passed.
         *
         * @param passCount non-negative count of passing rules
         * @return this instance for method chaining
         */
        public AccessibilityResult setPassesCount(int passCount) {
            this.passCount = passCount;
            return this;
        }

        /**
         * Sets the pre-computed accessibility score for this result.
         *
         * @param score accessibility score in the range {@code [0.0, 100.0]}
         * @return this instance for method chaining
         */
        public AccessibilityResult setScore(double score) {
            this.accessibilityScore = score;
            return this;
        }


        // --- New passes field ---
        /**
         * Returns the list of axe {@link Rule} objects that the page passed.
         *
         * @return list of passing rules; may be {@code null} if not yet populated
         */
        public List<Rule> getPasses() {
            return passes;
        }

        /**
         * Sets the list of passing axe rules for this result.
         *
         * @param passes list of axe {@link Rule} objects that passed; may be {@code null}
         * @return this instance for method chaining
         */
        public AccessibilityResult setPasses(List<Rule> passes) {
            this.passes = passes;
            return this;
        }

        /**
         * Returns the number of axe rules the page passed.
         * If the {@link #passes} list has been populated it is derived from that list;
         * otherwise the count stored by {@link #setPassesCount(int)} is returned.
         *
         * @return non-negative pass count
         */
        public int getPassCount() {
            return passes != null ? passes.size() : passCount;
        }

        /**
         * Returns the ISO-8601 timestamp captured when the scan completed.
         *
         * @return timestamp string (e.g. {@code "2024-06-01T14:30:00.123456789"}); may be
         *         {@code null} if not yet set
         */
        public String getTimestamp() {
            return timestamp;
        }

        /**
         * Sets the scan timestamp.
         *
         * @param timestamp ISO-8601 date-time string representing when the scan completed
         * @return this instance for method chaining
         */
        public AccessibilityResult setTimestamp(String timestamp) {
            this.timestamp = timestamp;
            return this;
        }

        /**
         * Returns {@code true} when at least one WCAG violation was detected.
         *
         * <pre>{@code
         * assertFalse(result.hasViolations(), "Page should be violation-free");
         * }</pre>
         *
         * @return {@code true} if {@link #getViolationsCount()} {@code > 0}
         */
        public boolean hasViolations() {
            return violationsCount > 0;
        }

        @Override
        public String toString() {
            return "AccessibilityResult{" +
                    "pageName='" + pageName + '\'' +
                    ", violationsCount=" + violationsCount +
                    ", passesCount=" + getPassCount() +
                    ", timestamp='" + timestamp + '\'' +
                    '}';
        }
    }

    private static JSONObject convertResultsToJSON(Results results) {
        JSONObject json = new JSONObject();
        try {
            json.put("violations", convertRules(results.getViolations(), "Violation"));
            json.put("incomplete", convertRules(results.getIncomplete(), "Needs Review"));
            json.put("inapplicable", convertRules(results.getInapplicable(), "Not Applicable"));
            json.put("passes", convertRules(results.getPasses(), "Passed"));
            json.put("totalViolations", results.getViolations().size());
            json.put("totalIncomplete", results.getIncomplete().size());
            json.put("totalInapplicable", results.getInapplicable().size());
            json.put("totalPassed", results.getPasses().size());
        } catch (JSONException e) {
            throw new RuntimeException("Error creating JSON from results", e);
        }
        return json;
    }

    private static JSONArray convertRules(List<Rule> rules, String category) {
        JSONArray arr = new JSONArray();

        if (rules == null || rules.isEmpty()) {
            return arr;
        }

        for (Rule rule : rules) {
            JSONObject obj = new JSONObject();
            try {
                obj.put("id", rule.getId());
                obj.put("category", category);
                obj.put("impact", rule.getImpact() != null ? rule.getImpact() : "none");
                obj.put("description", rule.getDescription());
                obj.put("helpUrl", rule.getHelpUrl());
                obj.put("wcagModel", getWcagModelFromTags(rule.getTags()));
                obj.put("tags", new JSONArray(rule.getTags()));
                JSONArray nodesArray = new JSONArray();
                rule.getNodes().forEach(node -> {
                    JSONObject nodeObj = new JSONObject();
                    try {
                        nodeObj.put("html", node.getHtml());
                        nodeObj.put("target", node.getTarget());
                        nodeObj.put("failureSummary", node.getFailureSummary());
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                    nodesArray.put(nodeObj);
                });
                obj.put("nodes", nodesArray);
                arr.put(obj);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        }
        return arr;
    }

    private static String getWcagModelFromTags(List<String> tags) {
        if (tags == null || tags.isEmpty()) return "Unknown";
        for (String tag : tags) {
            if (tag.contains("wcag2a")) return "WCAG 2.0 A";
            if (tag.contains("wcag2aa")) return "WCAG 2.0 AA";
            if (tag.contains("wcag21a")) return "WCAG 2.1 A";
            if (tag.contains("wcag21aa")) return "WCAG 2.1 AA";
            if (tag.contains("wcag22a")) return "WCAG 2.2 A";
            if (tag.contains("wcag22aa")) return "WCAG 2.2 AA";
            if (tag.contains("best-practice")) return "Best Practice";
        }
        return "Other";
    }

    private static void writeJsonResults(String filePath, JSONObject jsonData) throws IOException {
        Path path = Paths.get(filePath);
        Files.createDirectories(path.getParent());
        try {
            Files.writeString(path, jsonData.toString(4), StandardCharsets.UTF_8);
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }

    private static void printDetailedSummary(JSONObject json, String pageName) throws JSONException {
        logger.info("\n📊 Accessibility Summary for {}", pageName);
        logger.info("==========================================");
        logger.info(" Violations: {}", json.getInt("totalViolations"));
        logger.info(" Incomplete: {}", json.getInt("totalIncomplete"));
        logger.info(" Passed: {}", json.getInt("totalPassed"));
        logger.info(" Inapplicable: {}", json.getInt("totalInapplicable"));
        logger.info("==========================================");
        if (json.getInt("totalViolations") == 0) {
            logger.info("🎉 Great! No accessibility violations found.");
        }
    }

    /**
     * Attaches the most recently generated HTML accessibility report for the given
     * page to the current Allure test step.
     *
     * <p>The method looks for the latest report file matching
     * {@code AccessibilityReport_<pageName>*} inside the default
     * {@code "accessibility-reports/"} directory.
     *
     * <pre>{@code
     * AccessibilityHelper.attachReportToAllure("HomePage");
     * }</pre>
     *
     * @param pageName label used to locate the report file; must match the value
     *                 passed when the report was originally generated
     */
    public static void attachReportToAllure(String pageName) {
        try {
            Path report = getLatestReportPath(pageName);
            if (report != null && Files.exists(report)) {
                Path allureDir = Path.of("allure-results", "accessibility");
                Files.createDirectories(allureDir);
                Path target = allureDir.resolve(report.getFileName());
                Files.copy(report, target, StandardCopyOption.REPLACE_EXISTING);
                try (FileInputStream fis = new FileInputStream(target.toFile())) {
                    Allure.addAttachment("Accessibility Report - " + pageName, "text/html", fis, ".html");
                }
            }
        } catch (Exception e) {
            logger.warn("Failed to attach accessibility report to Allure: " + e.getMessage());
        }
    }

    /**
     * Generates a filtered HTML report containing only the violations present in
     * the supplied {@link AccessibilityResult} and attaches it to the current
     * Allure test step.
     *
     * <p>This is useful when a full report already exists but you want a focused
     * attachment highlighting only the failing rules.
     *
     * <pre>{@code
     * AccessibilityResult result =
     *         AccessibilityHelper.analyzePageAccessibilityAndSave(driver, "Cart", true);
     * if (result.hasViolations()) {
     *     AccessibilityHelper.attachFilteredReportToAllure("Cart", result, driver);
     * }
     * }</pre>
     *
     * @param pageName label used in the Allure attachment title and report file name
     * @param result   {@link AccessibilityResult} whose violations will be rendered
     * @param driver   active {@link WebDriver} instance used to read browser metadata
     *                 for the report header
     */
    public static void attachFilteredReportToAllure(String pageName, AccessibilityResult result, WebDriver driver) {
        try {
            String filteredReportPath = "allure-results/accessibility/FilteredReport_" + pageName + ".html";
            generateFilteredHTMLReport(result, pageName, filteredReportPath,driver);
            Path path = Path.of(filteredReportPath);
            if (Files.exists(path)) {
                try (FileInputStream fis = new FileInputStream(path.toFile())) {
                    Allure.addAttachment("Filtered Accessibility Report - " + pageName, "text/html", fis, ".html");
                }
            }
        } catch (Exception e) {
            logger.warn("Failed to attach filtered accessibility report to Allure: " + e.getMessage());
        }
    }

    /**
     * Generates a minimal HTML accessibility report that lists only the violations
     * contained in the supplied {@link AccessibilityResult} and writes it to the
     * given file path.
     *
     * <p>The output is suitable for attaching directly to an Allure report via
     * {@link #attachFilteredReportToAllure(String, AccessibilityResult, WebDriver)}.
     *
     * <pre>{@code
     * AccessibilityHelper.generateFilteredHTMLReport(
     *         result, "Checkout", "target/a11y/filtered.html", driver);
     * }</pre>
     *
     * @param result     {@link AccessibilityResult} providing the violation data
     * @param pageName   page label rendered in the report heading
     * @param reportPath file-system path where the HTML file will be written
     * @param driver     active {@link WebDriver} used to read browser metadata for
     *                   the report header
     * @throws IOException   if the report file cannot be created or written
     * @throws JSONException if serialising the violation data fails
     */
    public static void generateFilteredHTMLReport(AccessibilityResult result, String pageName, String reportPath, WebDriver driver) throws IOException, JSONException {
        List<Rule> violations = result.getViolations() != null ? result.getViolations() : Collections.emptyList();
        JSONObject json = new JSONObject();
        json.put("violations", convertRules(violations, "Violation"));
        json.put("totalViolations", violations.size());
        json.put("totalIncomplete", 0);
        json.put("totalPassed", 0);
        json.put("totalInapplicable", 0);

        AccessibilityConfig config = new AccessibilityConfig(); // or pass default
        generateEnhancedHTMLReport(json, pageName, reportPath, config,driver);
    }

    /**
     * Finds the most recently modified HTML report file for the given page inside
     * the {@code "accessibility-reports/"} directory.
     *
     * <pre>{@code
     * Path latestReport = AccessibilityHelper.getLatestReportPath("HomePage");
     * if (latestReport != null) {
     *     System.out.println("Latest report: " + latestReport);
     * }
     * }</pre>
     *
     * @param pageName label used to filter report files by their name prefix
     * @return the {@link Path} of the newest matching report, or {@code null} if
     *         the reports directory does not exist or contains no matching files
     * @throws IOException if reading the directory listing fails
     */
    public static Path getLatestReportPath(String pageName) throws IOException {
        Path reportsDir = Paths.get("accessibility-reports");
        if (!Files.exists(reportsDir)) return null;

        try (var stream = Files.list(reportsDir)) {
            return stream
                    .filter(p -> p.getFileName().toString().startsWith("AccessibilityReport_" + pageName))
                    .max(Comparator.comparingLong(p -> p.toFile().lastModified()))
                    .orElse(null);
        }
    }

    private static void generateEnhancedHTMLReport(JSONObject json, String pageName, String reportPath, AccessibilityConfig config, WebDriver driver)
            throws IOException, JSONException {
        int totalViolations = json.optInt("totalViolations", 0);
        int totalIncomplete = json.optInt("totalIncomplete", 0);  // keep for user info
        int totalPassed = json.optInt("totalPassed", 0);
        int totalInapplicable = json.optInt("totalInapplicable", 0);
        int totalChecks = totalViolations + totalPassed; // ✅ ignore incomplete for score
        double score = totalChecks == 0 ? 100.0 : ((double) totalPassed / totalChecks) * 100.0;

        String formattedScore = String.format(Locale.US, "%.1f", score);
        String dateTime = DISPLAY_DATE_FORMAT.format(ZonedDateTime.now());
        String browserInfo = "Unknown Browser";
        String browserVersion = "N/A";
        String platform = "N/A";

        if (driver != null) {
            try {
                Capabilities caps = ((RemoteWebDriver) driver).getCapabilities();
                browserInfo = caps.getBrowserName();
                browserVersion = caps.getBrowserVersion();
                platform = (caps.getPlatformName() != null) ? caps.getPlatformName().toString() : "Unknown";
            } catch (Exception e) {
                logger.warn("Could not fetch browser capabilities for report metadata: {}", e.getMessage());
            }
        }
        StringBuilder html = new StringBuilder();
        html.append("<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'>");
        html.append("<meta name='viewport' content='width=device-width, initial-scale=1'>");
        html.append("<title>Accessibility Report - ").append(escapeHtml(pageName)).append("</title>");
        html.append("<link href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css' rel='stylesheet'>");
        html.append("<script src='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js'></script>");
        html.append("<script src='https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js'></script>");
        html.append("<style>");
        html.append("body{background:#f8fafc;font-family:'Segoe UI',sans-serif;padding:20px;}");
        html.append("h2{font-weight:600;margin-bottom:10px;}");
        html.append(".score-wrap{position:relative;width:200px;height:200px;margin:20px auto;}");
        html.append(".score-text{position:absolute;left:50%;top:50%;transform:translate(-50%,-50%);font-size:2.5rem;font-weight:700;color:#111;}");
        html.append(".summary-boxes{display:flex;justify-content:center;gap:40px;flex-wrap:wrap;margin-top:25px;}");
        html.append(".summary-item{min-width:120px;text-align:center;padding:15px 20px;border-radius:10px;box-shadow:0 2px 5px rgba(0,0,0,0.1);}");
        html.append(".summary-item .label{font-weight:600;margin-bottom:6px;}");
        html.append(".summary-item .value{font-size:1.5rem;font-weight:700;}");
        html.append(".text-danger{color:#dc3545!important;}");
        html.append(".text-warning{color:#ffc107!important;}");
        html.append(".text-success{color:#198754!important;}");
        html.append(".text-secondary{color:#6c757d!important;}");
        html.append(".accordion-button{text-transform:capitalize;font-weight:500;}");
        html.append("table{font-size:0.9rem;}th,td{vertical-align:middle!important;}");
        html.append(".filter-select{width:100%;font-size:0.85rem;}");
        html.append("</style></head><body><div class='container'>");

        html.append("<div class='text-center mb-4'>");
        html.append("<h2>Accessibility Report</h2>");
        html.append("<div class='text-muted'>").append(escapeHtml(pageName)).append(" • ").append(dateTime).append("</div>");
        html.append("</div>");

        html.append("<div style='margin-top:8px; font-size:14px; color:#555;'>")
                .append("<strong>Browser:</strong> ").append(browserInfo)
                .append(" ").append(browserVersion)
                .append(" | <strong>Platform:</strong> ").append(platform)
                .append(" | <strong>Generated:</strong> ").append(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")))
                .append("</div>");
        html.append("</div>");

        html.append("<div class='mb-4'>");
        html.append("<div class='card p-3 text-center mb-3'><div class='score-wrap'>");
        html.append("<canvas id='gauge'></canvas>");
        html.append("<div class='score-text'>").append(formattedScore).append("%</div>");
        html.append("</div><div class='text-muted small mt-2'>Overall Accessibility Score</div></div>");
        html.append("<div class='card p-3'><div class='row text-center'>");
        html.append("<div class='col'><div class='text-danger fw-bold'>❌ Violations</div><div class='h4'>").append(totalViolations).append("</div></div>");
        html.append("<div class='col'><div class='text-warning fw-bold'>⚠️ Needs Review</div><div class='h4'>").append(totalIncomplete).append("</div></div>");
        html.append("<div class='col'><div class='text-success fw-bold'>✅ Passed</div><div class='h4'>").append(totalPassed).append("</div></div>");
        html.append("<div class='col'><div class='text-secondary fw-bold'>🚫 Inapplicable</div><div class='h4'>").append(totalInapplicable).append("</div></div>");
        html.append("</div></div></div>");

        html.append("<div class='accordion' id='accessibilityAccordion'>");
        html.append(buildAccordionSection("Violations", json.optJSONArray("violations"), "danger", 1));
        html.append(buildAccordionSection("Needs Review", json.optJSONArray("incomplete"), "warning", 2));
        if (config.isIncludePasses())
            html.append(buildAccordionSection("Passed Rules", json.optJSONArray("passes"), "success", 3));
        html.append(buildAccordionSection("Inapplicable", json.optJSONArray("inapplicable"), "secondary", 4));
        html.append("</div>");

        html.append("<div class='text-muted small mt-2'>");
        html.append("Overall Accessibility Score (ignoring incomplete checks): ").append(formattedScore).append("%");
        if (totalIncomplete > 0) {
            html.append(" • ⚠️ ").append(totalIncomplete).append(" check(s) incomplete — manual review may be needed");
        }
        html.append("</div>");
        html.append("<div class='text-center text-muted small mt-4'>Generated ").append(dateTime)
                .append(" by SHAFT ENGINE AccessibilityHelper</div>");

        html.append("<script>");
        html.append("(function(){const ctx=document.getElementById('gauge');if(!ctx)return;");
        html.append("const score=").append(formattedScore).append(";");
        html.append("function color(v){if(v>=90)return '#0cce6b';if(v>=50)return '#ffa400';return '#ff4e42';}");
        html.append("new Chart(ctx,{type:'doughnut',data:{datasets:[{data:[score,100-score],backgroundColor:[color(score),'#e9ecef'],borderWidth:0}]},");
        html.append("options:{rotation:0,circumference:360,cutout:'70%',plugins:{legend:{display:false},tooltip:{enabled:false}}}});})();");
        html.append("</script>");

        html.append("<script>");
        html.append("document.querySelectorAll('.filter-select').forEach(sel=>{sel.addEventListener('change',()=>{\n");
        html.append("const table=sel.closest('table');const colIndex=sel.dataset.col;\n");
        html.append("const val=sel.value.toLowerCase();\n");
        html.append("table.querySelectorAll('tbody tr').forEach(row=>{\n");
        html.append("const cell=row.cells[colIndex];if(!cell)return;\n");
        html.append("const txt=cell.textContent.toLowerCase();\n");
        html.append("row.style.display=(val===''||txt.includes(val))?'':'none';});});});");
        html.append("</script>");

        html.append("</div></body></html>");
        Files.writeString(Paths.get(reportPath), html.toString(), StandardCharsets.UTF_8);
    }

    private static String buildAccordionSection(String title, JSONArray arr, String color, int index) throws JSONException {
        if (arr == null) arr = new JSONArray();
        String collapseId = "collapse" + index;
        String cardHeader = title + " (" + arr.length() + ")";

        Set<String> impacts = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        Set<String> wcagModels = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);

        for (int i = 0; i < arr.length(); i++) {
            JSONObject rule = arr.getJSONObject(i);
            impacts.add(rule.optString("impact", "none"));
            wcagModels.add(rule.optString("wcagModel", "Unknown"));
        }

        StringBuilder sb = new StringBuilder();
        sb.append("<div class='accordion-item'>");
        sb.append("<h2 class='accordion-header'>");
        sb.append("<button class='accordion-button collapsed text-").append(color)
                .append("' type='button' data-bs-toggle='collapse' data-bs-target='#")
                .append(collapseId).append("'>").append(cardHeader).append("</button></h2>");
        sb.append("<div id='").append(collapseId)
                .append("' class='accordion-collapse collapse' data-bs-parent='#accessibilityAccordion'>");
        sb.append("<div class='accordion-body'>");

        if (arr.length() == 0) {
            sb.append("<div class='text-muted'>No items found.</div>");
        } else {
            sb.append("<div class='table-responsive'><table class='table table-bordered table-sm align-middle'>");
            sb.append("<thead><tr>");
            sb.append("<th>Rule ID</th>");
            sb.append("<th>Description</th>");
            sb.append("<th>Impact<br><select class='filter-select' data-col='2'><option value=''>All</option>");
            for (String impact : impacts) {
                sb.append("<option>").append(escapeHtml(impact)).append("</option>");
            }
            sb.append("</select></th>");
            sb.append("<th>WCAG Model<br><select class='filter-select' data-col='3'><option value=''>All</option>");
            for (String model : wcagModels) {
                sb.append("<option>").append(escapeHtml(model)).append("</option>");
            }
            sb.append("</select></th>");
            sb.append("<th>Affected Elements</th><th>Help</th>");
            sb.append("</tr></thead><tbody>");

            for (int i = 0; i < arr.length(); i++) {
                JSONObject rule = arr.getJSONObject(i);
                sb.append("<tr><td>").append(escapeHtml(rule.optString("id"))).append("</td>");
                sb.append("<td>").append(escapeHtml(rule.optString("description"))).append("</td>");
                sb.append("<td>").append(escapeHtml(rule.optString("impact", "none"))).append("</td>");
                sb.append("<td>").append(escapeHtml(rule.optString("wcagModel"))).append("</td>");
                sb.append("<td>");
                JSONArray nodes = rule.optJSONArray("nodes");
                if (nodes != null) {
                    for (int j = 0; j < nodes.length(); j++) {
                        JSONObject node = nodes.getJSONObject(j);
                        sb.append("<code>").append(escapeHtml(node.optString("html"))).append("</code><br>");
                    }
                }
                sb.append("</td>");
                sb.append("<td><a href='").append(rule.optString("helpUrl", "#"))
                        .append("' target='_blank'>Link</a></td></tr>");
            }
            sb.append("</tbody></table></div>");
        }
        sb.append("</div></div></div>");
        return sb.toString();
    }

    private static String escapeHtml(String text) {
        if (text == null) return "";
        return text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#39;");
    }

    private static void waitForDomStability(WebDriver driver, int stableMillis, int timeoutSeconds) {
        JavascriptExecutor js = (JavascriptExecutor) driver;
        long lastLength = ((Number) js.executeScript("return document.getElementsByTagName('*').length")).longValue();
        long startTime = System.currentTimeMillis();
        long lastChangeTime = startTime;

        while (System.currentTimeMillis() - startTime < timeoutSeconds * 1000L) {
            long currentLength = ((Number) js.executeScript("return document.getElementsByTagName('*').length")).longValue();
            if (currentLength != lastLength) {
                lastLength = currentLength;
                lastChangeTime = System.currentTimeMillis();
            } else if (System.currentTimeMillis() - lastChangeTime >= stableMillis) {
                break;
            }
            try {
                Thread.sleep(DOM_CHECK_INTERVAL_MILLIS);
            } catch (InterruptedException ignored) {}
        }
    }

}