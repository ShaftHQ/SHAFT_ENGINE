package com.shaft.validation.accessibility;

import com.deque.html.axecore.results.CheckedNode;
import com.deque.html.axecore.results.Results;
import com.deque.html.axecore.results.Rule;
import com.deque.html.axecore.selenium.AxeBuilder;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class AccessibilityHelperCoverageUnitTest {
    private static final Path REPORT_ROOT = Path.of("target", "accessibility-helper-coverage");

    @AfterMethod(alwaysRun = true)
    public void cleanUp() throws IOException {
        Properties.clearForCurrentThread();
        deleteDirectory(REPORT_ROOT);
    }

    @Test(description = "Covers the successful accessibility report generation flow with mocked driver/context")
    public void analyzePageAccessibilityShouldGenerateReportsForSuccessfulScan() throws IOException {
        Path reportsDir = REPORT_ROOT.resolve("successful-flow");
        AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
                .setReportsDir(reportsDir + "/")
                .setContext("main")
                .setTags(List.of("wcag21aa"));
        Results results = results(List.of(), List.of(rule("page-has-heading-one", "moderate", "wcag21aa", 1)), List.of(), List.of());

        try (MockedConstruction<AxeBuilder> axeBuilders = mockAxeBuilders(results)) {
            AccessibilityHelper.analyzePageAccessibility(mockLoadedDriver(), "SuccessfulPage", config);

            Assert.assertEquals(axeBuilders.constructed().size(), 1);
            AxeBuilder builder = axeBuilders.constructed().get(0);
            verify(builder).withTags(List.of("wcag21aa"));
            verify(builder).include("main");
            verify(builder).setOptions("{ \"resultTypes\": [\"violations\", \"incomplete\", \"inapplicable\", \"passes\"] }");
            Assert.assertEquals(countFiles(reportsDir, "AccessibilityJSON_SuccessfulPage_", ".json"), 1);
            Assert.assertEquals(countFiles(reportsDir, "AccessibilityReport_SuccessfulPage_", ".html"), 1);
        }
    }

    @Test(description = "Covers accepted null result buckets when at least one axe bucket has content")
    public void analyzePageAccessibilityAndSaveShouldNormalizeNullBucketsWhenInapplicableResultsExist() {
        Results results = results(null, null, null, List.of(rule("meta-viewport", "minor", "best-practice", 1)));

        try (MockedConstruction<AxeBuilder> ignored = mockAxeBuilders(results)) {
            AccessibilityHelper.AccessibilityResult result = AccessibilityHelper.analyzePageAccessibilityAndSave(
                    mock(WebDriver.class), "NullBuckets", new AccessibilityHelper.AccessibilityConfig(), false);

            Assert.assertEquals(result.getPageName(), "NullBuckets");
            Assert.assertEquals(result.getViolationsCount(), 0);
            Assert.assertEquals(result.getPassCount(), 0);
            Assert.assertEquals(result.getAccessibilityScore(), 0.0);
            Assert.assertFalse(result.hasViolations());
            Assert.assertNotNull(result.getTimestamp());
        }
    }

    @Test(description = "Covers empty-result handling and fallback report generation")
    public void analyzePageAccessibilityAndSaveShouldFailAndWriteFallbackWhenAllResultsAreEmpty() throws IOException {
        Path reportsDir = REPORT_ROOT.resolve("empty-results");
        AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
                .setReportsDir(reportsDir + "/")
                .setContext("#missing");

        try (MockedConstruction<AxeBuilder> ignored = mockAxeBuilders(results(List.of(), List.of(), List.of(), List.of()))) {
            RuntimeException exception = Assert.expectThrows(RuntimeException.class,
                    () -> AccessibilityHelper.analyzePageAccessibilityAndSave(mock(WebDriver.class), "EmptyPage", config, true));

            Assert.assertTrue(exception.getMessage().contains("Accessibility analysis failed for page 'EmptyPage'"));
            Assert.assertEquals(countFiles(reportsDir, "FailedReport_EmptyPage_", ".txt"), 1);
        }
    }

    @Test(description = "Covers violation/reporting branches and filtered report generation")
    public void violationEntryPointsShouldReportFailuresAndCreateFilteredHtml() throws Exception {
        Rule violation = rule("image-alt", "critical", "wcag2a", 2);
        Rule bestPracticeViolation = rule("landmark-one-main", "moderate", "best-practice", 1);
        Results violationResults = results(List.of(violation, bestPracticeViolation), List.of(rule("document-title", "minor", "wcag21aa", 1)), List.of(), List.of());
        Path filteredReport = REPORT_ROOT.resolve("filtered").resolve("FilteredReport_ViolationsPage.html");

        try (MockedConstruction<AxeBuilder> ignored = mockAxeBuilders(violationResults)) {
            Assert.assertTrue(AccessibilityHelper.hasCriticalViolations(mock(WebDriver.class), "ViolationsPage"));
            Assert.assertFalse(AccessibilityHelper.isAccessible(mock(WebDriver.class)));
            Map<String, Integer> violationsByType = AccessibilityHelper.getViolationsByType(mock(WebDriver.class));

            Assert.assertEquals(violationsByType.get("WCAG 2.0 A"), Integer.valueOf(2));
            Assert.assertEquals(violationsByType.get("Best Practice"), Integer.valueOf(1));

            AccessibilityHelper.AccessibilityResult result = AccessibilityHelper.analyzePageAccessibilityAndSave(
                    mock(WebDriver.class), "ViolationsPage", new AccessibilityHelper.AccessibilityConfig(), false);
            Assert.assertTrue(result.hasViolations());
            Assert.assertEquals(result.getViolationsCount(), 2);
            Assert.assertEquals(result.getPassCount(), 1);
            Assert.assertEquals(result.getAccessibilityScore(), 100.0 / 3.0, 0.001);

            Files.createDirectories(filteredReport.getParent());
            AccessibilityHelper.generateFilteredHTMLReport(result, "ViolationsPage", filteredReport.toString(), mock(WebDriver.class));
            Assert.assertTrue(Files.exists(filteredReport));
            Assert.assertTrue(Files.readString(filteredReport).contains("image-alt"));
        }
    }

    @Test(description = "Covers config branches driven by thread-local SHAFT properties")
    public void analyzePageAccessibilityAndSaveShouldHonorThreadLocalConfigurationValues() throws IOException {
        ThreadLocalPropertiesManager.setProperty("accessibility.includePasses", "false");
        ThreadLocalPropertiesManager.setProperty("accessibility.context", "#configured-region");
        ThreadLocalPropertiesManager.setProperty("accessibility.reportsDir", REPORT_ROOT.resolve("property-driven") + "/");

        AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
                .setIncludePasses(Boolean.parseBoolean(ThreadLocalPropertiesManager.getProperty("accessibility.includePasses")))
                .setContext(ThreadLocalPropertiesManager.getProperty("accessibility.context"))
                .setReportsDir(ThreadLocalPropertiesManager.getProperty("accessibility.reportsDir"));
        Results results = results(List.of(rule("button-name", "serious", "wcag21aa", 1)), List.of(rule("html-has-lang", "minor", "wcag2a", 1)), List.of(), List.of());

        try (MockedConstruction<AxeBuilder> axeBuilders = mockAxeBuilders(results)) {
            AccessibilityHelper.AccessibilityResult result = AccessibilityHelper.analyzePageAccessibilityAndSave(
                    mock(WebDriver.class), "PropertyDriven", config, true);

            Assert.assertTrue(result.hasViolations());
            Assert.assertEquals(result.getViolationsCount(), 1);
            AxeBuilder builder = axeBuilders.constructed().get(0);
            verify(builder).include("#configured-region");
            verify(builder, never()).setOptions(anyString());
            Path htmlReport = latestFile(Path.of(config.getReportsDir()), "AccessibilityReport_PropertyDriven_", ".html");
            Assert.assertNotNull(htmlReport);
            Assert.assertFalse(Files.readString(htmlReport).contains("Passed Rules"));
        }
    }

    @Test(description = "Covers report discovery and optional Allure attachment paths")
    public void latestReportAndAllureAttachmentHelpersShouldHandlePresentAndMissingReports() throws IOException {
        Path defaultReportDir = Path.of("accessibility-reports");
        Files.createDirectories(defaultReportDir);
        Path older = defaultReportDir.resolve("AccessibilityReport_DiscoveryPage_20000101_000000.html");
        Path newer = defaultReportDir.resolve("AccessibilityReport_DiscoveryPage_20000101_000001.html");
        Files.writeString(older, "<html>older</html>");
        Files.writeString(newer, "<html>newer</html>");
        older.toFile().setLastModified(1000L);
        newer.toFile().setLastModified(2000L);

        Path latestReportPath = AccessibilityHelper.getLatestReportPath("DiscoveryPage");

        Assert.assertEquals(latestReportPath, newer);
        AccessibilityHelper.attachReportToAllure("DiscoveryPage");
        AccessibilityHelper.attachReportToAllure("MissingPage");
        Assert.assertTrue(Files.exists(Path.of("allure-results", "accessibility", newer.getFileName().toString())));
        Files.deleteIfExists(older);
        Files.deleteIfExists(newer);
    }

    private static MockedConstruction<AxeBuilder> mockAxeBuilders(Results... results) {
        AtomicInteger analyses = new AtomicInteger();
        return Mockito.mockConstruction(AxeBuilder.class, (builder, context) -> {
            when(builder.withTags(any())).thenReturn(builder);
            when(builder.include(anyString())).thenReturn(builder);
            when(builder.analyze(any(WebDriver.class))).thenAnswer(invocation -> {
                int index = Math.min(analyses.getAndIncrement(), results.length - 1);
                return results[index];
            });
        });
    }

    private static WebDriver mockLoadedDriver() {
        WebDriver driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        when(driver.getWindowHandle()).thenReturn("window-handle");
        when(driver.getCurrentUrl()).thenReturn("https://example.test/page");
        JavascriptExecutor javascriptExecutor = (JavascriptExecutor) driver;
        doReturn("complete").when(javascriptExecutor).executeScript("return document.readyState");
        doReturn(3L).when(javascriptExecutor).executeScript("return document.getElementsByTagName('*').length");
        return driver;
    }

    private static Results results(List<Rule> violations, List<Rule> passes, List<Rule> incomplete, List<Rule> inapplicable) {
        Results results = new Results();
        results.setViolations(violations);
        results.setPasses(passes);
        results.setIncomplete(incomplete);
        results.setInapplicable(inapplicable);
        return results;
    }

    private static Rule rule(String id, String impact, String tag, int nodeCount) {
        Rule rule = new Rule();
        rule.setId(id);
        rule.setDescription("Description for " + id);
        rule.setHelpUrl("https://example.test/rules/" + id);
        rule.setImpact(impact);
        rule.setTags(List.of(tag));
        List<CheckedNode> nodes = new ArrayList<>();
        for (int i = 0; i < nodeCount; i++) {
            CheckedNode node = new CheckedNode();
            node.setHtml("<div id='" + id + "-" + i + "'>content</div>");
            node.setTarget(List.of("#" + id + "-" + i));
            node.setFailureSummary("Fix " + id);
            nodes.add(node);
        }
        rule.setNodes(nodes);
        return rule;
    }

    private static long countFiles(Path dir, String prefix, String suffix) throws IOException {
        if (!Files.exists(dir)) {
            return 0;
        }
        try (var stream = Files.list(dir)) {
            return stream.filter(path -> {
                String fileName = path.getFileName().toString();
                return fileName.startsWith(prefix) && fileName.endsWith(suffix);
            }).count();
        }
    }

    private static Path latestFile(Path dir, String prefix, String suffix) throws IOException {
        if (!Files.exists(dir)) {
            return null;
        }
        try (var stream = Files.list(dir)) {
            return stream.filter(path -> {
                        String fileName = path.getFileName().toString();
                        return fileName.startsWith(prefix) && fileName.endsWith(suffix);
                    })
                    .max(java.util.Comparator.comparingLong(path -> path.toFile().lastModified()))
                    .orElse(null);
        }
    }

    private static void deleteDirectory(Path directory) throws IOException {
        if (!Files.exists(directory)) {
            return;
        }
        try (var stream = Files.walk(directory)) {
            stream.sorted(java.util.Comparator.reverseOrder())
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    });
        }
    }
}
