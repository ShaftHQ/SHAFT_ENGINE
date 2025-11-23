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

public class AccessibilityActions {
    private final SHAFT.GUI.WebDriver driver;
    private final BrowserActions browserActions;

    /**
     * Thread-safe cache keyed by a composed string (pageName + configHash + saveReport flag).
     * Using a composed key avoids collisions between different configs / saveReport options.
     */
    private final Map<String, AccessibilityHelper.AccessibilityResult> cachedResults = new ConcurrentHashMap<>();

    public AccessibilityActions(WebDriver rawDriver, BrowserActions browserActions) {
        this.driver = new SHAFT.GUI.WebDriver(rawDriver);
        this.browserActions = browserActions;
    }

    private static final Logger logger = LogManager.getLogger(AccessibilityActions.class);

    /* ==========================
        ANALYSIS METHODS
       ========================== */

    public AccessibilityActions analyzePage(String pageName) {
        // Always save report for explicit analysis call
        String key = buildCacheKey(pageName, null, true);
        cacheResult(key, AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName, true));
        attachReportToAllure(pageName);
        return this;
    }

    public AccessibilityActions analyzePage(String pageName, AccessibilityHelper.AccessibilityConfig config) {
        // Always save report for explicit analysis call, even with custom config
        String key = buildCacheKey(pageName, config, true);
        cacheResult(key, AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName, config, true));
        attachReportToAllure(pageName);
        return this;
    }

    /**
     * Analyze and return a result for the current pageName. This method uses a cache key that
     * includes pageName, config (if any) and the saveReport flag — so different combinations won't collide.
     * If an entry exists in the cache for that exact combination, it will be returned.
     */
    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName) {
        return analyzeAndReturn(pageName, null, true);
    }

    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName, boolean saveReport) {
        return analyzeAndReturn(pageName, null, saveReport);
    }

    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName, AccessibilityHelper.AccessibilityConfig config) {
        return analyzeAndReturn(pageName, config, true);
    }

    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName, AccessibilityHelper.AccessibilityConfig config, boolean saveReport) {
        String key = buildCacheKey(pageName, config, saveReport);
        return cachedResults.computeIfAbsent(key, k ->
                AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName, config, saveReport));
    }

    /**
     * Analyze a page then attach a filtered report to Allure without permanently mutating the cached result.
     * Previously this method removed violations directly on the cached object; now we temporarily filter,
     * attach, and restore the original list to avoid side-effects for other callers.
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

    public AccessibilityActions assertIsAccessible() {
        boolean accessible = AccessibilityHelper.isAccessible(driver.getDriver());
        driver.assertThat()
                .object(accessible)
                .isTrue()
                .withCustomReportMessage("Assert the page is accessible")
                .perform();
        return this;
    }

    public AccessibilityActions verifyIsAccessible() {
        boolean accessible = AccessibilityHelper.isAccessible(driver.getDriver());
        driver.verifyThat()
                .object(accessible)
                .isTrue()
                .withCustomReportMessage("Verify the page is accessible")
                .perform();
        return this;
    }

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

    public AccessibilityActions assertAccessibilityScoreAtLeast(String pageName, double minimumPercentage) {
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, true);
        return validateAccessibilityScore(pageName, minimumPercentage, result);
    }

    public AccessibilityActions assertAccessibilityScoreAtLeast(String pageName, double minimumPercentage, boolean saveReport) {
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, saveReport);
        return validateAccessibilityScore(pageName, minimumPercentage, result);
    }

    public AccessibilityActions assertAccessibilityScoreAtLeast(String pageName, double minimumPercentage, AccessibilityHelper.AccessibilityConfig config) {
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, config, true);
        return validateAccessibilityScore(pageName, minimumPercentage, result);
    }

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
