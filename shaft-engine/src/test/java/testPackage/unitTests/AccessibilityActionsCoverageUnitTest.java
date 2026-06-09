package testPackage.unitTests;

import com.deque.html.axecore.results.Rule;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.accessibility.AccessibilityActions;
import com.shaft.validation.accessibility.AccessibilityHelper;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;

@Test(singleThreaded = true)
public class AccessibilityActionsCoverageUnitTest {
    private WebDriver rawDriver;
    private BrowserActions browserActions;
    private AccessibilityActions accessibilityActions;

    @BeforeMethod(alwaysRun = true)
    public void setUp() {
        rawDriver = Mockito.mock(WebDriver.class);
        browserActions = Mockito.mock(BrowserActions.class);
        accessibilityActions = new AccessibilityActions(rawDriver, browserActions);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void shouldAnalyzePagesAndReuseCachedResults() {
        AccessibilityHelper.AccessibilityResult defaultResult = resultWithViolations(List.of(), 95.0);
        AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig().setTags(List.of("wcag21aa"));
        AccessibilityHelper.AccessibilityResult customResult = resultWithViolations(List.of(), 90.0);

        try (MockedStatic<AccessibilityHelper> helper = Mockito.mockStatic(AccessibilityHelper.class)) {
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Home"), Mockito.eq(true)))
                    .thenReturn(defaultResult);
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Home"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(true)))
                    .thenReturn(defaultResult);
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Custom"), Mockito.eq(config), Mockito.eq(true)))
                    .thenReturn(customResult);
            helper.when(() -> AccessibilityHelper.attachReportToAllure("Home")).thenAnswer(invocation -> null);
            helper.when(() -> AccessibilityHelper.attachReportToAllure("Custom")).thenAnswer(invocation -> null);

            accessibilityActions.analyzePage("Home");
            accessibilityActions.analyzePage("Custom", config);

            AccessibilityHelper.AccessibilityResult first = accessibilityActions.analyzeAndReturn("Home");
            AccessibilityHelper.AccessibilityResult second = accessibilityActions.analyzeAndReturn("Home", true);
            AccessibilityHelper.AccessibilityResult third = accessibilityActions.analyzeAndReturn("Custom", config, true);

            Assert.assertSame(first, second);
            Assert.assertSame(third, customResult);

            helper.verify(() -> AccessibilityHelper.attachReportToAllure("Home"), times(1));
            helper.verify(() -> AccessibilityHelper.attachReportToAllure("Custom"), times(1));
        }
    }

    @Test
    public void shouldAnalyzeWithIgnoredRulesAndRestoreCachedViolations() {
        Rule ignoredRule = mockedRule("color-contrast", "critical");
        Rule keptRule = mockedRule("label", "serious");
        List<Rule> violations = new ArrayList<>(List.of(ignoredRule, keptRule));
        AccessibilityHelper.AccessibilityResult result = resultWithViolations(violations, 50.0);

        try (MockedStatic<AccessibilityHelper> helper = Mockito.mockStatic(AccessibilityHelper.class)) {
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Ignored"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(true)))
                    .thenReturn(result);
            helper.when(() -> AccessibilityHelper.attachFilteredReportToAllure(Mockito.eq("Ignored"), Mockito.eq(result), any(WebDriver.class))).thenAnswer(invocation -> null);

            accessibilityActions.analyzeWithIgnoredRules("Ignored", List.of("color-contrast"));

            Assert.assertEquals(result.getViolations().size(), 2);
            Assert.assertTrue(result.getViolations().stream().anyMatch(rule -> "color-contrast".equals(rule.getId())));
            helper.verify(() -> AccessibilityHelper.attachFilteredReportToAllure(Mockito.eq("Ignored"), Mockito.eq(result), any(WebDriver.class)), times(1));
        }
    }

    @Test
    public void shouldPassAssertionAndVerificationPathsWhenNoViolationsExist() {
        AccessibilityHelper.AccessibilityResult result = resultWithViolations(List.of(mockedRule("aria-hidden-focus", "minor")), 99.0);

        try (MockedStatic<AccessibilityHelper> helper = Mockito.mockStatic(AccessibilityHelper.class)) {
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Safe"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(true)))
                    .thenReturn(result);
            helper.when(() -> AccessibilityHelper.isAccessible(any(WebDriver.class))).thenReturn(true);

            Assert.assertSame(accessibilityActions.assertNoCriticalViolations("Safe"), accessibilityActions);
            Assert.assertSame(accessibilityActions.verifyNoCriticalViolations("Safe"), accessibilityActions);
            Assert.assertSame(accessibilityActions.assertIsAccessible(), accessibilityActions);
            Assert.assertSame(accessibilityActions.verifyIsAccessible(), accessibilityActions);
        }
    }

    @Test
    public void shouldFilterByImpactAndFailWhenViolationsArePresent() {
        AccessibilityHelper.AccessibilityResult impactResult = resultWithViolations(List.of(mockedRule("region", "minor")), 88.0);
        AccessibilityHelper.AccessibilityResult noViolations = resultWithViolations(List.of(), 100.0);
        AccessibilityHelper.AccessibilityResult withViolations = resultWithViolations(List.of(mockedRule("color-contrast", "critical")), 40.0);

        try (MockedStatic<AccessibilityHelper> helper = Mockito.mockStatic(AccessibilityHelper.class)) {
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Impact"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(false)))
                    .thenReturn(impactResult);
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("NoFail"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(false)))
                    .thenReturn(noViolations);
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("FailPage"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(false)))
                    .thenReturn(withViolations);
            helper.when(() -> AccessibilityHelper.attachFilteredReportToAllure(Mockito.eq("Impact"), Mockito.eq(impactResult), any(WebDriver.class))).thenAnswer(invocation -> null);
            helper.when(() -> AccessibilityHelper.attachFilteredReportToAllure(Mockito.eq("NoFail"), Mockito.eq(noViolations), any(WebDriver.class))).thenAnswer(invocation -> null);
            helper.when(() -> AccessibilityHelper.attachFilteredReportToAllure(Mockito.eq("FailPage"), Mockito.eq(withViolations), any(WebDriver.class))).thenAnswer(invocation -> null);

            Assert.assertSame(accessibilityActions.assertNoViolationsByImpact("Impact", "critical", "serious"), accessibilityActions);
            Assert.assertSame(accessibilityActions.failIfViolationsExist("NoFail"), accessibilityActions);
            Assert.assertThrows(AssertionError.class, () -> accessibilityActions.failIfViolationsExist("FailPage"));
        }
    }

    @Test
    public void shouldValidateAccessibilityScoreOverloadsAndScoreGuards() {
        AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig().setTags(List.of("wcag2aa"));
        AccessibilityHelper.AccessibilityResult highScore = resultWithViolations(List.of(), 95.34);
        AccessibilityHelper.AccessibilityResult mediumScore = resultWithViolations(List.of(), 80.01);
        AccessibilityHelper.AccessibilityResult lowScore = resultWithViolations(List.of(), 70.0);

        try (MockedStatic<AccessibilityHelper> helper = Mockito.mockStatic(AccessibilityHelper.class)) {
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Score1"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(true)))
                    .thenReturn(highScore);
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Score2"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(false)))
                    .thenReturn(mediumScore);
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Score3"), Mockito.eq(config), Mockito.eq(true)))
                    .thenReturn(highScore);
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("Score4"), Mockito.eq(config), Mockito.eq(false)))
                    .thenReturn(mediumScore);
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("ScoreLow"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(true)))
                    .thenReturn(lowScore);
            helper.when(() -> AccessibilityHelper.analyzePageAccessibilityAndSave(any(WebDriver.class), Mockito.eq("InvalidMin"), (AccessibilityHelper.AccessibilityConfig) Mockito.isNull(), Mockito.eq(true)))
                    .thenReturn(highScore);

            Assert.assertSame(accessibilityActions.assertAccessibilityScoreAtLeast("Score1", 80.0), accessibilityActions);
            Assert.assertSame(accessibilityActions.assertAccessibilityScoreAtLeast("Score2", 80.0, false), accessibilityActions);
            Assert.assertSame(accessibilityActions.assertAccessibilityScoreAtLeast("Score3", 90.0, config), accessibilityActions);
            Assert.assertSame(accessibilityActions.assertAccessibilityScoreAtLeast("Score4", 80.0, config, false), accessibilityActions);

            Assert.assertThrows(AssertionError.class,
                    () -> accessibilityActions.assertAccessibilityScoreAtLeast("ScoreLow", 75.0));
            Assert.assertThrows(IllegalArgumentException.class,
                    () -> accessibilityActions.assertAccessibilityScoreAtLeast("InvalidMin", -1.0));
            Assert.assertSame(accessibilityActions.backToBrowser(), browserActions);
        }
    }

    private static AccessibilityHelper.AccessibilityResult resultWithViolations(List<Rule> violations, double score) {
        return new AccessibilityHelper.AccessibilityResult()
                .setPageName("TestPage")
                .setViolations(new ArrayList<>(violations))
                .setScore(score);
    }

    private static Rule mockedRule(String id, String impact) {
        Rule rule = Mockito.mock(Rule.class);
        Mockito.when(rule.getId()).thenReturn(id);
        Mockito.when(rule.getImpact()).thenReturn(impact);
        return rule;
    }
}
