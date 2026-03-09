package com.shaft.validation.accessibility;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.openqa.selenium.WebDriver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import static com.shaft.validation.accessibility.AccessibilityHelper.attachFilteredReportToAllure;
import static com.shaft.validation.accessibility.AccessibilityHelper.attachReportToAllure;

/**
 * Provides a fluent API for running automated accessibility audits against web pages using the
 * <a href="https://github.com/dequelabs/axe-core">axe-core</a> engine via
 * {@link AccessibilityHelper}.
 *
 * <p>Typical usage:
 * <pre>{@code
 * driver.accessibility()
 *       .analyzePage("Home")
 *       .assertNoCriticalViolations("Home")
 *       .backToBrowser();
 * }</pre>
 *
 * <p>Analysis results are cached in a {@link java.util.concurrent.ConcurrentHashMap}
 * keyed by a composite of page name, {@code AccessibilityConfig} instance identity
 * (via {@code System.identityHashCode}), and the {@code saveReport} flag.
 * Successive calls with the <em>same config instance</em> and the same page name will
 * reuse the cached result. Note that two separately constructed {@code AccessibilityConfig}
 * objects with identical settings will produce different cache keys because
 * {@code AccessibilityConfig} does not override {@code equals}/{@code hashCode}.
 *
 * <p><b>Thread safety:</b> Each {@code AccessibilityActions} instance is tied to a single
 * {@link com.shaft.driver.SHAFT.GUI.WebDriver} instance and must not be shared across threads.
 * The internal cache ({@link java.util.concurrent.ConcurrentHashMap}) is safe for concurrent
 * reads, but the analysis itself (browser interaction) is not.
 *
 * @see AccessibilityHelper
 */
public class AccessibilityActions {
    private final SHAFT.GUI.WebDriver driver;
    private final BrowserActions browserActions;

    /**
     * Thread-safe cache keyed by a composed string (pageName + configHash + saveReport flag).
     * Using a composed key avoids collisions between different configs / saveReport options.
     */
    private final Map<String, AccessibilityHelper.AccessibilityResult> cachedResults = new ConcurrentHashMap<>();

    /**
     * Constructs an {@code AccessibilityActions} instance backed by the supplied raw
     * {@link WebDriver} and the caller's {@link BrowserActions} context.
     *
     * <p>The raw driver is wrapped in a {@link com.shaft.driver.SHAFT.GUI.WebDriver} so that
     * SHAFT's assertion and verification helpers are available internally.
     *
     * @param rawDriver      the underlying Selenium {@link WebDriver} used to run axe-core scripts
     * @param browserActions the {@link BrowserActions} instance to return to when
     *                       {@link #backToBrowser()} is called
     */
    public AccessibilityActions(WebDriver rawDriver, BrowserActions browserActions) {
        this.driver = new SHAFT.GUI.WebDriver(rawDriver);
        this.browserActions = browserActions;
    }

    private static final Logger logger = LogManager.getLogger(AccessibilityActions.class);

    /* ==========================
        ANALYSIS METHODS
       ========================== */

    /**
     * Runs an accessibility audit on the current page, saves the HTML report to disk, and
     * attaches it to the Allure report under the given {@code pageName}.
     *
     * <p>The result is cached so that subsequent assertion calls for the same page reuse it
     * without triggering another axe run.
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility().analyzePage("Checkout");
     * }</pre>
     *
     * @param pageName a human-readable label used to name the saved report file and the
     *                 Allure attachment (e.g. {@code "HomePage"})
     * @return this {@code AccessibilityActions} instance for method chaining
     */
    public AccessibilityActions analyzePage(String pageName) {
        // Always save report for explicit analysis call
        String key = buildCacheKey(pageName, null, true);
        cacheResult(key, AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName, true));
        attachReportToAllure(pageName);
        return this;
    }

    /**
     * Runs an accessibility audit on the current page using a custom {@link AccessibilityHelper.AccessibilityConfig},
     * saves the HTML report to disk, and attaches it to the Allure report.
     *
     * <p>Use this overload when you need to restrict the audit to specific WCAG tags, rules, or
     * impact levels that differ from the default configuration.
     *
     * <p>Example:
     * <pre>{@code
     * AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
     *         .setTags(List.of("wcag2a", "wcag2aa"));
     * driver.accessibility().analyzePage("Checkout", config);
     * }</pre>
     *
     * @param pageName a human-readable label used to name the saved report file and Allure attachment
     * @param config   custom axe-core configuration controlling which rules / tags are included
     * @return this {@code AccessibilityActions} instance for method chaining
     */
    public AccessibilityActions analyzePage(String pageName, AccessibilityHelper.AccessibilityConfig config) {
        // Always save report for explicit analysis call, even with custom config
        String key = buildCacheKey(pageName, config, true);
        cacheResult(key, AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName, config, true));
        attachReportToAllure(pageName);
        return this;
    }

    /**
     * Analyzes the current page and returns the {@link AccessibilityHelper.AccessibilityResult}
     * for programmatic inspection. The report is saved to disk and the result is cached.
     *
     * <p>Subsequent calls with the same {@code pageName} return the cached result without
     * running the audit again.
     *
     * <p>Example:
     * <pre>{@code
     * AccessibilityHelper.AccessibilityResult result =
     *         driver.accessibility().analyzeAndReturn("ProductPage");
     * int violationCount = result.getViolations().size();
     * }</pre>
     *
     * @param pageName a human-readable label for the page being audited
     * @return the {@link AccessibilityHelper.AccessibilityResult} containing violations, passes,
     *         and the overall accessibility score
     */
    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName) {
        return analyzeAndReturn(pageName, null, true);
    }

    /**
     * Analyzes the current page, optionally saving the HTML report to disk, and returns the
     * {@link AccessibilityHelper.AccessibilityResult} for programmatic inspection.
     *
     * <p>Example:
     * <pre>{@code
     * // Analyze without persisting a report file
     * AccessibilityHelper.AccessibilityResult result =
     *         driver.accessibility().analyzeAndReturn("SearchPage", false);
     * }</pre>
     *
     * @param pageName   a human-readable label for the page being audited
     * @param saveReport {@code true} to persist the HTML report file on disk; {@code false} to
     *                   skip file creation (useful for assertion-only checks)
     * @return the {@link AccessibilityHelper.AccessibilityResult} for the audited page
     */
    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName, boolean saveReport) {
        return analyzeAndReturn(pageName, null, saveReport);
    }

    /**
     * Analyzes the current page using a custom {@link AccessibilityHelper.AccessibilityConfig},
     * saves the report, and returns the result. The result is cached for the given
     * {@code pageName} + {@code config} combination.
     *
     * <p>Example:
     * <pre>{@code
     * AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
     *         .setTags(List.of("best-practice"));
     * AccessibilityHelper.AccessibilityResult result =
     *         driver.accessibility().analyzeAndReturn("CartPage", config);
     * }</pre>
     *
     * @param pageName a human-readable label for the page being audited
     * @param config   custom axe-core configuration; {@code null} uses the default configuration
     * @return the {@link AccessibilityHelper.AccessibilityResult} for the audited page
     */
    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName, AccessibilityHelper.AccessibilityConfig config) {
        return analyzeAndReturn(pageName, config, true);
    }

    /**
     * Analyzes the current page using the given configuration and {@code saveReport} flag, then
     * returns the {@link AccessibilityHelper.AccessibilityResult}.
     *
     * <p>This is the canonical overload that all other {@code analyzeAndReturn} variants delegate to.
     * Results are memoized: the first call computes and caches the result; subsequent calls with
     * an identical key return the cached value immediately.
     *
     * <p>Example:
     * <pre>{@code
     * AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
     *         .setTags(List.of("wcag2a"));
     * AccessibilityHelper.AccessibilityResult result =
     *         driver.accessibility().analyzeAndReturn("LoginPage", config, false);
     * }</pre>
     *
     * @param pageName   a human-readable label for the page being audited
     * @param config     custom axe-core configuration; {@code null} applies the default configuration
     * @param saveReport {@code true} to persist the HTML report file; {@code false} to skip it
     * @return the {@link AccessibilityHelper.AccessibilityResult} for the audited page
     */
    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName, AccessibilityHelper.AccessibilityConfig config, boolean saveReport) {
        String key = buildCacheKey(pageName, config, saveReport);
        return cachedResults.computeIfAbsent(key, k ->
                AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName, config, saveReport));
    }

    /**
     * Analyzes the current page and attaches a filtered Allure report that excludes the
     * specified rule IDs, without permanently modifying the cached result.
     *
     * <p>This is useful when certain known violations should be suppressed from the report
     * for a particular context (e.g. third-party widgets) while keeping the full cached result
     * intact for other callers.
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility()
     *       .analyzeWithIgnoredRules("Dashboard", List.of("color-contrast", "label"));
     * }</pre>
     *
     * @param pageName      a human-readable label for the page being audited; used to retrieve or
     *                      create the cached result and to name the Allure attachment
     * @param ignoredRuleIds axe-core rule IDs (e.g. {@code "color-contrast"}, {@code "label"})
     *                       to exclude from the attached report; the underlying cached violations
     *                       list is restored after the attachment is created
     * @return this {@code AccessibilityActions} instance for method chaining
     */
    public AccessibilityActions analyzeWithIgnoredRules(String pageName, List<String> ignoredRuleIds) {
        // Use saved result if available; prefer saved report so attachments include reports
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, true);

        // Defensive copy of violations contents so we can safely filter for reporting and then restore.
        List<com.deque.html.axecore.results.Rule> originalViolations = new ArrayList<>(result.getViolations());
        try {
            result.getViolations().removeIf(rule -> ignoredRuleIds.contains(rule.getId()));
            attachFilteredReportToAllure(pageName, result, driver.getDriver());
        } finally {
            // restore original violations so cached result remains unchanged for other callers
            result.getViolations().clear();
            result.getViolations().addAll(originalViolations);
        }

        return this;
    }

    /* ==========================
        ASSERTION METHODS
       ========================== */

    /**
     * Asserts (hard assertion) that the current page has no accessibility violations with an
     * impact level of {@code "critical"}.
     *
     * <p>The audit result is fetched from the cache or computed on demand. Failure causes the
     * test to stop immediately (hard assertion semantics).
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility()
     *       .analyzePage("PaymentPage")
     *       .assertNoCriticalViolations("PaymentPage");
     * }</pre>
     *
     * @param pageName a human-readable label identifying the page to assert against; must match
     *                 the label used in a preceding {@link #analyzePage(String)} or
     *                 {@link #analyzeAndReturn(String)} call so the cache is hit
     * @return this {@code AccessibilityActions} instance for method chaining
     */
    public AccessibilityActions assertNoCriticalViolations(String pageName) {
        // saveReport=true so report is saved for assertion
        boolean noViolations = analyzeAndReturn(pageName, true).getViolations().stream()
                .noneMatch(rule -> "critical".equalsIgnoreCase(rule.getImpact()));

        driver.assertThat()
                .object(noViolations)
                .isTrue()
                .withCustomReportMessage("Assert no critical accessibility violations found on page: " + pageName)
                .perform();
        return this;
    }

    /**
     * Verifies (soft assertion) that the current page has no accessibility violations with an
     * impact level of {@code "critical"}.
     *
     * <p>Unlike {@link #assertNoCriticalViolations(String)}, a soft assertion failure is
     * recorded but does not stop test execution immediately; all failures are reported at the
     * end of the test.
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility()
     *       .analyzePage("CartPage")
     *       .verifyNoCriticalViolations("CartPage");
     * }</pre>
     *
     * @param pageName a human-readable label identifying the page to verify; must match the label
     *                 used in a preceding analysis call so the cache is hit
     * @return this {@code AccessibilityActions} instance for method chaining
     */
    public AccessibilityActions verifyNoCriticalViolations(String pageName) {
        // saveReport=true so report is saved for verification
        boolean noViolations = analyzeAndReturn(pageName, true).getViolations().stream()
                .noneMatch(rule -> "critical".equalsIgnoreCase(rule.getImpact()));

        driver.verifyThat()
                .object(noViolations)
                .isTrue()
                .withCustomReportMessage("Verify no critical accessibility violations found on page: " + pageName)
                .perform();
        return this;
    }

    /**
     * Asserts (hard assertion) that the current page is fully accessible according to
     * {@link AccessibilityHelper#isAccessible(WebDriver)}'s default criteria (zero violations).
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility().assertIsAccessible();
     * }</pre>
     *
     * @return this {@code AccessibilityActions} instance for method chaining
     */
    public AccessibilityActions assertIsAccessible() {
        boolean accessible = AccessibilityHelper.isAccessible(driver.getDriver());
        driver.assertThat()
                .object(accessible)
                .isTrue()
                .withCustomReportMessage("Assert the page is accessible")
                .perform();
        return this;
    }

    /**
     * Verifies (soft assertion) that the current page is fully accessible according to
     * {@link AccessibilityHelper#isAccessible(WebDriver)}'s default criteria (zero violations).
     *
     * <p>Unlike {@link #assertIsAccessible()}, a failure is recorded but does not halt the test.
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility().verifyIsAccessible();
     * }</pre>
     *
     * @return this {@code AccessibilityActions} instance for method chaining
     */
    public AccessibilityActions verifyIsAccessible() {
        boolean accessible = AccessibilityHelper.isAccessible(driver.getDriver());
        driver.verifyThat()
                .object(accessible)
                .isTrue()
                .withCustomReportMessage("Verify the page is accessible")
                .perform();
        return this;
    }

    /**
     * Asserts (hard assertion) that the current page has no accessibility violations matching
     * any of the supplied impact levels.
     *
     * <p>A filtered Allure attachment is generated showing only the violations that matched the
     * requested impact levels, making failures easier to triage.
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility()
     *       .assertNoViolationsByImpact("ProfilePage", "critical", "serious");
     * }</pre>
     *
     * @param pageName     a human-readable label for the page being checked; used for caching and
     *                     the assertion failure message
     * @param impactLevels one or more axe-core impact levels to check (case-insensitive):
     *                     {@code "minor"}, {@code "moderate"}, {@code "serious"}, {@code "critical"}
     * @return this {@code AccessibilityActions} instance for method chaining
     */
    public AccessibilityActions assertNoViolationsByImpact(String pageName, String... impactLevels) {
        Set<String> impacts = Arrays.stream(impactLevels)
                .map(String::toLowerCase)
                .collect(Collectors.toSet());

        // Use saved result for better reporting
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, false);
        boolean noViolations = result.getViolations().stream()
                .noneMatch(rule -> impacts.contains(rule.getImpact().toLowerCase()));

        attachFilteredReportToAllure(pageName, result, driver.getDriver());

        driver.assertThat()
                .object(noViolations)
                .isTrue()
                .withCustomReportMessage("No " + String.join("/", impactLevels) + " violations found on page: " + pageName)
                .perform();
        return this;
    }

    /**
     * Attaches a filtered Allure report for the given page and throws an {@link AssertionError}
     * if any accessibility violations exist.
     *
     * <p>This method is a strict gate: any violation — regardless of impact level — causes an
     * immediate hard failure. Use {@link #assertNoViolationsByImpact(String, String...)} when
     * you need to filter by severity.
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility()
     *       .analyzePage("LandingPage")
     *       .failIfViolationsExist("LandingPage");
     * }</pre>
     *
     * @param pageName a human-readable label for the page being checked; must match a previously
     *                 analyzed page so the cached result is reused
     * @return this {@code AccessibilityActions} instance for method chaining
     * @throws AssertionError if one or more accessibility violations are present on the page
     */
    public AccessibilityActions failIfViolationsExist(String pageName) {
        // saveReport=false: base analysis already done, no need to save another report
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, false);
        attachFilteredReportToAllure(pageName, result, driver.getDriver());
        if (!result.getViolations().isEmpty()) {
            throw new AssertionError("Accessibility violations found on page '" + pageName + "'");
        }
        return this;
    }

    /* ==========================
        ASSERT ACCESSIBILITY SCORE
       ========================== */

    /**
     * Asserts that the accessibility score for the given page is at least
     * {@code minimumPercentage}. The report is saved to disk.
     *
     * <p>The score is computed by {@link AccessibilityHelper.AccessibilityResult#getAccessibilityScore()}
     * and represents the ratio of passing rules to total rules as a percentage (0–100).
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility()
     *       .assertAccessibilityScoreAtLeast("HomePage", 90.0);
     * }</pre>
     *
     * @param pageName          a human-readable label for the page being checked
     * @param minimumPercentage the minimum acceptable accessibility score (inclusive), in the
     *                          range {@code 0.0}–{@code 100.0}
     * @return this {@code AccessibilityActions} instance for method chaining
     * @throws IllegalArgumentException if {@code minimumPercentage} is outside {@code [0, 100]}
     * @throws AssertionError           if the computed score is below {@code minimumPercentage}
     */
    public AccessibilityActions assertAccessibilityScoreAtLeast(String pageName, double minimumPercentage) {
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, true);
        return validateAccessibilityScore(pageName, minimumPercentage, result);
    }

    /**
     * Asserts that the accessibility score for the given page is at least
     * {@code minimumPercentage}, with control over whether the HTML report is saved to disk.
     *
     * <p>Example:
     * <pre>{@code
     * // Assert score without persisting a report file
     * driver.accessibility()
     *       .assertAccessibilityScoreAtLeast("ResultsPage", 85.0, false);
     * }</pre>
     *
     * @param pageName          a human-readable label for the page being checked
     * @param minimumPercentage the minimum acceptable accessibility score (inclusive), in the
     *                          range {@code 0.0}–{@code 100.0}
     * @param saveReport        {@code true} to persist the HTML report file; {@code false} to skip it
     * @return this {@code AccessibilityActions} instance for method chaining
     * @throws IllegalArgumentException if {@code minimumPercentage} is outside {@code [0, 100]}
     * @throws AssertionError           if the computed score is below {@code minimumPercentage}
     */
    public AccessibilityActions assertAccessibilityScoreAtLeast(String pageName, double minimumPercentage, boolean saveReport) {
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, saveReport);
        return validateAccessibilityScore(pageName, minimumPercentage, result);
    }

    /**
     * Asserts that the accessibility score for the given page is at least
     * {@code minimumPercentage}, using a custom {@link AccessibilityHelper.AccessibilityConfig}.
     * The report is saved to disk.
     *
     * <p>Example:
     * <pre>{@code
     * AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
     *         .setTags(List.of("wcag2a"));
     * driver.accessibility()
     *       .assertAccessibilityScoreAtLeast("ProfilePage", 95.0, config);
     * }</pre>
     *
     * @param pageName          a human-readable label for the page being checked
     * @param minimumPercentage the minimum acceptable accessibility score (inclusive), in the
     *                          range {@code 0.0}–{@code 100.0}
     * @param config            custom axe-core configuration applied during the audit
     * @return this {@code AccessibilityActions} instance for method chaining
     * @throws IllegalArgumentException if {@code minimumPercentage} is outside {@code [0, 100]}
     * @throws AssertionError           if the computed score is below {@code minimumPercentage}
     */
    public AccessibilityActions assertAccessibilityScoreAtLeast(String pageName, double minimumPercentage, AccessibilityHelper.AccessibilityConfig config) {
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, config, true);
        return validateAccessibilityScore(pageName, minimumPercentage, result);
    }

    /**
     * Asserts that the accessibility score for the given page is at least
     * {@code minimumPercentage}, using a custom {@link AccessibilityHelper.AccessibilityConfig}
     * and explicit control over report persistence.
     *
     * <p>This is the most flexible overload and is the one all other
     * {@code assertAccessibilityScoreAtLeast} variants ultimately delegate to.
     *
     * <p>Example:
     * <pre>{@code
     * AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
     *         .setTags(List.of("wcag2aa"));
     * driver.accessibility()
     *       .assertAccessibilityScoreAtLeast("SettingsPage", 80.0, config, true);
     * }</pre>
     *
     * @param pageName          a human-readable label for the page being checked
     * @param minimumPercentage the minimum acceptable accessibility score (inclusive), in the
     *                          range {@code 0.0}–{@code 100.0}
     * @param config            custom axe-core configuration applied during the audit;
     *                          {@code null} uses the default configuration
     * @param saveReport        {@code true} to persist the HTML report file; {@code false} to skip it
     * @return this {@code AccessibilityActions} instance for method chaining
     * @throws IllegalArgumentException if {@code minimumPercentage} is outside {@code [0, 100]}
     * @throws AssertionError           if the computed score is below {@code minimumPercentage}
     */
    public AccessibilityActions assertAccessibilityScoreAtLeast(String pageName, double minimumPercentage,
                                                                AccessibilityHelper.AccessibilityConfig config,
                                                                boolean saveReport) {
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, config, saveReport);
        return validateAccessibilityScore(pageName, minimumPercentage, result);
    }

    /* ==========================
        PRIVATE HELPERS
       ========================== */

    private AccessibilityActions validateAccessibilityScore(String pageName,
                                                            double minimumPercentage,
                                                            AccessibilityHelper.AccessibilityResult result) {

        if (minimumPercentage < 0.0 || minimumPercentage > 100.0) {
            throw new IllegalArgumentException("Invalid minimumPercentage: " + minimumPercentage
                    + "%. It must be between 0 and 100.");
        }

        double score = result.getAccessibilityScore();
        double roundedScore = Math.round(score * 100.0) / 100.0;

        if (roundedScore < minimumPercentage) {
            throw new AssertionError("Accessibility score for page '" + pageName
                    + "' is below the minimum required " + minimumPercentage + "%. Actual: "
                    + roundedScore + "%");
        }

        logger.info("Accessibility score for page '" + pageName + "' is " + roundedScore
                + "%, meets the minimum requirement of " + minimumPercentage + "%");

        return this;
    }

    /**
     * Returns the {@link BrowserActions} instance that was used to create this
     * {@code AccessibilityActions}, enabling fluent chaining back into browser-level operations.
     *
     * <p>Example:
     * <pre>{@code
     * driver.accessibility()
     *       .analyzePage("Home")
     *       .assertNoCriticalViolations("Home")
     *       .backToBrowser()
     *       .navigateToURL("https://example.com/about");
     * }</pre>
     *
     * @return the originating {@link BrowserActions} instance
     */
    public BrowserActions backToBrowser() {
        return browserActions;
    }

    private void cacheResult(String key, AccessibilityHelper.AccessibilityResult result) {
        cachedResults.put(key, result);
    }

    /**
     * Build a unique cache key based on the page name, optional config and the saveReport flag.
     * This avoids collisions where the same page is analyzed with different configs or saveReport settings.
     *
     * Note: this uses config.hashCode() — ensure your AccessibilityConfig implementation defines
     * a stable hashCode()/equals() if you expect caching to work across process runs.
     */
    private String buildCacheKey(String pageName, AccessibilityHelper.AccessibilityConfig config, boolean saveReport) {
        String configPart = (config == null) ? "default" : Integer.toHexString(config.hashCode());
        return pageName + "::" + configPart + "::" + saveReport;
    }
}
