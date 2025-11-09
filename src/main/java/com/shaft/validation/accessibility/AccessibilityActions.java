package com.shaft.validation.accessibility;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import org.openqa.selenium.WebDriver;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import static com.shaft.validation.accessibility.AccessibilityHelper.attachFilteredReportToAllure;
import static com.shaft.validation.accessibility.AccessibilityHelper.attachReportToAllure;

public class AccessibilityActions {
    private final SHAFT.GUI.WebDriver driver;
    private final BrowserActions browserActions;

    // Use ConcurrentHashMap to make caching thread-safe
    private final Map<String, AccessibilityHelper.AccessibilityResult> cachedResults = new ConcurrentHashMap<>();

    public AccessibilityActions(WebDriver rawDriver, BrowserActions browserActions) {
        this.driver = new SHAFT.GUI.WebDriver(rawDriver);
        this.browserActions = browserActions;
    }

    Logger logger = Logger.getLogger(getClass().getName());

    /* ==========================
        ANALYSIS METHODS
       ========================== */

    public AccessibilityActions analyzePage(String pageName) {
        // Always save report for explicit analysis call
        cacheResult(pageName, AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName,true));
        attachReportToAllure(pageName);
        return this;
    }

    public AccessibilityActions analyzePage(String pageName, AccessibilityHelper.AccessibilityConfig config) {
        // Always save report for explicit analysis call, even with custom config
        cacheResult(pageName, AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName, config,true));
        attachReportToAllure(pageName);
        return this;
    }

    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName, boolean saveReport) {
        // saveReport flag determines whether a report is actually written
        return cachedResults.computeIfAbsent(pageName,
                k -> AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName, saveReport));
    }

    public AccessibilityHelper.AccessibilityResult analyzeAndReturn(String pageName, AccessibilityHelper.AccessibilityConfig config) {
        // NOTE: always saves report (true) here – could be refactored to accept a saveReport flag if needed
        return cachedResults.computeIfAbsent(pageName,
                k -> AccessibilityHelper.analyzePageAccessibilityAndSave(driver.getDriver(), pageName, config,true));
    }

    public AccessibilityActions analyzeWithIgnoredRules(String pageName, List<String> ignoredRuleIds) {
        // For filtered analysis, do not save the base report again (saveReport=false)
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName,false);
        result.getViolations().removeIf(rule -> ignoredRuleIds.contains(rule.getId()));
        attachFilteredReportToAllure(pageName, result,driver.getDriver());
        return this;
    }

    /* ==========================
        ASSERTION METHODS
       ========================== */

    public AccessibilityActions assertNoCriticalViolations(String pageName) {
        // saveReport=true so report is saved for assertion
        boolean noViolations = !analyzeAndReturn(pageName,true).getViolations().stream()
                .anyMatch(rule -> "critical".equalsIgnoreCase(rule.getImpact()));
        driver.assertThat()
                .object(noViolations)
                .isTrue()
                .withCustomReportMessage("Assert no critical accessibility violations found on page: " + pageName)
                .perform();
        return this;
    }

    public AccessibilityActions verifyNoCriticalViolations(String pageName) {
        // saveReport=true so report is saved for verification
        boolean noViolations = !analyzeAndReturn(pageName,true).getViolations().stream()
                .anyMatch(rule -> "critical".equalsIgnoreCase(rule.getImpact()));
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

        // saveReport=false because we are just filtering for this assertion
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName,false);
        boolean noViolations = result.getViolations().stream()
                .noneMatch(rule -> impacts.contains(rule.getImpact().toLowerCase()));

        attachFilteredReportToAllure(pageName, result,driver.getDriver());

        driver.assertThat()
                .object(noViolations)
                .isTrue()
                .withCustomReportMessage("No " + String.join("/", impactLevels) + " violations found on page: " + pageName)
                .perform();
        return this;
    }

    public AccessibilityActions failIfViolationsExist(String pageName) {
        // saveReport=false: base analysis already done, no need to save another report
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName,false);
        attachFilteredReportToAllure(pageName, result, driver.getDriver());
        if (!result.getViolations().isEmpty()) {
            throw new AssertionError("Accessibility violations found on page '" + pageName + "'");
        }
        return this;
    }

    public AccessibilityActions assertAccessibilityScoreAtLeast(String pageName, double minimumPercentage) {
        if (minimumPercentage < 0.0 || minimumPercentage > 100.0) {
            throw new IllegalArgumentException("Invalid minimumPercentage: " + minimumPercentage
                    + "%. It must be between 0 and 100.");
        }

        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, true);
        double score = result.getAccessibilityScore();
        double roundedScore = Math.round(score * 100.0) / 100.0;

        if (roundedScore < minimumPercentage) {
            throw new AssertionError("Accessibility score for page '" + pageName
                    + "' is below the minimum required " + minimumPercentage + "%. Actual: "
                    + roundedScore + "%");
        }

        // If it passes, you can log success if needed
        logger.info("Accessibility score for page '" + pageName + "' is " + roundedScore + "%, meets the minimum requirement of " + minimumPercentage + "%");

        return this;
    }

    public AccessibilityActions assertAccessibilityScoreAtLeast(String pageName, double minimumPercentage, AccessibilityHelper.AccessibilityConfig config) {
        if (minimumPercentage < 0.0 || minimumPercentage > 100.0) {
            throw new IllegalArgumentException("Invalid minimumPercentage: " + minimumPercentage
                    + "%. It must be between 0 and 100.");
        }
        AccessibilityHelper.AccessibilityResult result = analyzeAndReturn(pageName, config);
        double score = result.getAccessibilityScore();
        double roundedScore = Math.round(score * 100.0) / 100.0;

        if (roundedScore < minimumPercentage) {
            throw new AssertionError("Accessibility score for page '" + pageName
                    + "' is below the minimum required " + minimumPercentage + "%. Actual: "
                    + roundedScore + "%");
        }

        // If it passes, you can log success if needed
        logger.info("Accessibility score for page '" + pageName + "' is " + roundedScore + "%, meets the minimum requirement of " + minimumPercentage + "%");
        return this;
    }


    /* ==========================
        HELPER METHODS
       ========================== */

    public BrowserActions backToBrowser() {
        return browserActions;
    }

    private void cacheResult(String pageName, AccessibilityHelper.AccessibilityResult result) {
        cachedResults.put(pageName, result);
    }

}
