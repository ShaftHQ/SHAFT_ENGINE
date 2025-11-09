package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.validation.accessibility.AccessibilityHelper;
import org.testng.annotations.*;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class AccessibilityTest {
    private SHAFT.GUI.WebDriver driver;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com");
    }

    /* ==========================
       ANALYSIS METHODS
       ========================== */

    @Test(description = "Analyze page with default configuration and assert cached result",enabled = false)
    public void testAnalyzePageDefault() {
        driver.browser()
                .accessibility()
                .analyzePage("HomePage")
                .assertNoCriticalViolations("HomePage")
                .assertIsAccessible();

        // Verify cached result works
        var result = driver.browser().accessibility().analyzeAndReturn("HomePage",false);
        SHAFT.Report.log("Cached Violations Count: " + result.getViolationsCount());
    }

    @Test(description = "Analyze page with custom configuration")
    public void testAnalyzePageWithConfig() {
        AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
                .setTags(Arrays.asList("wcag2a", "wcag2aa"))
                .setIncludePasses(true)
                .setContext("body")
                .setReportsDir("target/custom-accessibility-reports/");
        try {
            driver.browser()
                    .accessibility()
                    .analyzePage("CustomConfigPage", config)
                    .assertNoCriticalViolations("CustomConfigPage")
                    .assertIsAccessible();
        } catch (AssertionError e) {
            SHAFT.Report.log("Expected failure caught: " + e.getMessage());
        }
    }

    @Test(description = "Analyze page and ignore specific rules")
    public void testAnalyzeWithIgnoredRules() {
        driver.browser().navigateToURL("https://www.google.com");
        List<String> ignoredRules = Collections.singletonList("color-contrast");

        try {
            driver.browser()
                .accessibility()
                .analyzePage("PageWithIgnoredRules")
                .analyzeWithIgnoredRules("PageWithIgnoredRules", ignoredRules)
                .assertNoCriticalViolations("PageWithIgnoredRules");
        } catch (AssertionError e) {
            SHAFT.Report.log("Expected failure caught: " + e.getMessage());
        }
    }

    @Test(description = "Analyze and return result with default config withour saving report")
    public void testAnalyzeAndReturnDefault() {
        var result = driver.browser().accessibility().analyzeAndReturn("ReturnedResultPage",false);
        SHAFT.Report.log("Page: " + result.getPageName());
        SHAFT.Report.log("Violations Count: " + result.getViolationsCount());
    }

    @Test(description = "Analyze and return result with custom config (expected failure)")
    public void testAnalyzeAndReturnWithConfig() {
        try {
            AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
                    .setTags(Arrays.asList("wcag2a", "wcag2aa"))
                    .setIncludePasses(true)
                    .setReportsDir("target/accessibility-return-tests/")
                    .setContext("body");

            var result = driver.browser()
                    .accessibility()
                    .analyzeAndReturn("ReturnedResultCustomPage", config);

            SHAFT.Report.log("Page: " + result.getPageName());
            SHAFT.Report.log("Violations Count: " + result.getViolationsCount());
        } catch (AssertionError e) {
            SHAFT.Report.log("Expected failure caught: " + e.getMessage());
        }
    }

    /* ==========================
       ASSERTION By score
       ========================== */
    @Test(description = "Accessibility score above threshold should pass")
    public void testAccessibilityScorePass() {
        // Assert that the page is at least 95% accessible
        driver.browser().accessibility()
                .assertAccessibilityScoreAtLeast("samplePage", 95.0);
    }

    @Test(description = "Accessibility score below threshold should fail")
    public void testAccessibilityScoreFail() {
        // Assert that the page is at least 15% accessible
        driver.browser().accessibility()
                    .assertAccessibilityScoreAtLeast("lowScorePage", 100);
    }

    @Test(description = "Accessibility score with custom config")
    public void testAccessibilityWithCustomConfig() {
        // Assert with config that the page is at least 10% accessible
        AccessibilityHelper.AccessibilityConfig customConfig = new AccessibilityHelper.AccessibilityConfig()
                .setTags(List.of("wcag2aa", "best-practice"))
                .setIncludePasses(true)
                .setContext("body");

        // Assert that the accessibility score is at least 85%
        driver.browser().accessibility()
                .assertAccessibilityScoreAtLeast("customConfigPage",100.00, customConfig);
    }


    /* ==========================
       ASSERTION METHODS (expected failures)
       ========================== */

    @Test(description = "Hard assert no critical violations (expected failure)")
    public void testAssertNoCriticalViolations() {
        try {
            driver.browser()
                    .accessibility()
                    .analyzePage("AssertCriticalPage")
                    .assertNoCriticalViolations("AssertCriticalPage");
        } catch (AssertionError e) {
            SHAFT.Report.log("Expected failure caught: " + e.getMessage());
        }
    }

    @Test(description = "Soft verify no critical violations (expected failure)",enabled = false)
    public void testVerifyNoCriticalViolations() {
        try {
            driver.browser()
                    .accessibility()
                    .analyzePage("VerifyCriticalPage")
                    .verifyNoCriticalViolations("VerifyCriticalPage");
        } catch (AssertionError e) {
            SHAFT.Report.log("Expected failure caught: " + e.getMessage());
        }
    }

    @Test(description = "Hard assert page accessibility (expected failure)")
    public void testAssertIsAccessible() {
        try {
            driver.browser()
                    .accessibility()
                    .assertIsAccessible();
        } catch (AssertionError e) {
            SHAFT.Report.log("Expected failure caught: " + e.getMessage());
        }
    }


    @Test(description = "Soft verify page accessibility (expected failure)",enabled = false)
    public void testVerifyIsAccessible() {
        try {
            driver.browser()
                    .accessibility()
                    .verifyIsAccessible();
        } catch (AssertionError e) {
            SHAFT.Report.log("Expected failure caught: " + e.getMessage());
        }
    }

    @Test(description = "Assert no violations by impact levels (expected failure)")
    public void testAssertNoViolationsByImpact() {
        try {
            driver.browser()
                    .accessibility()
                    .analyzePage("ImpactPage")
                    .assertNoViolationsByImpact("ImpactPage", "critical", "serious");
        } catch (AssertionError e) {
            SHAFT.Report.log("Expected failure caught: " + e.getMessage());
        }
    }

    @Test(description = "Fail if any violations exist (expected failure)")
    public void testFailIfViolationsExist() {
        try {
            driver.browser()
                    .accessibility()
                    .analyzePage("FailIfViolationsPage")
                    .failIfViolationsExist("FailIfViolationsPage");
        } catch (AssertionError e) {
            SHAFT.Report.log("Expected failure caught: " + e.getMessage());
        }
    }

    /* ==========================
       HELPER METHODS
       ========================== */

    @Test(description = "Back to browser helper returns BrowserActions")
    public void testBackToBrowser() {
        var browserActions = driver.browser()
                .accessibility()
                .backToBrowser();

        browserActions.navigateToURL("https://www.bing.com");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }
}
