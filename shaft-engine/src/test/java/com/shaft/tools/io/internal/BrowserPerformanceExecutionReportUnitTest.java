package com.shaft.tools.io.internal;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.concurrent.TimeUnit;

@Test(singleThreaded = true)
public class BrowserPerformanceExecutionReportUnitTest {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    @BeforeMethod(alwaysRun = true)
    public void setup() {
        BrowserPerformanceExecutionReport.reset();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        BrowserPerformanceExecutionReport.reset();
        Properties.clearForCurrentThread();
    }

    @Test
    public void generatePerformanceReportShouldWriteHtmlAndJsonForBrowserActionsAndPageLoads() throws Exception {
        SHAFT.Properties.performance.set()
                .generatePerformanceReport(true)
                .browserActionPerformanceBudgets("playwright.element.click=3")
                .pageLoadPerformanceBudgets("https://example.test=20")
                .failOnBrowserPerformanceBudgetViolation(false);
        BrowserPerformanceExecutionReport.recordBrowserAction("playwright.element.click", TimeUnit.MILLISECONDS.toNanos(5));
        BrowserPerformanceExecutionReport.recordPageLoad("https://example.test", TimeUnit.MILLISECONDS.toNanos(10));
        FileActions fileActions = Mockito.mock(FileActions.class);

        try (MockedStatic<FileActions> fileActionsMock = Mockito.mockStatic(FileActions.class)) {
            fileActionsMock.when(() -> FileActions.getInstance(true)).thenReturn(fileActions);

            BrowserPerformanceExecutionReport.generatePerformanceReport(1000L, 2000L);
        }

        ArgumentCaptor<String> fileNameCaptor = ArgumentCaptor.forClass(String.class);
        ArgumentCaptor<String> contentCaptor = ArgumentCaptor.forClass(String.class);
        Mockito.verify(fileActions).createFolder(SHAFT.Properties.paths.performanceReportPath());
        Mockito.verify(fileActions, Mockito.times(2)).writeToFile(
                Mockito.eq(SHAFT.Properties.paths.performanceReportPath()),
                fileNameCaptor.capture(),
                contentCaptor.capture());

        CapturedFile jsonFile = capturedContent(fileNameCaptor.getAllValues(), contentCaptor.getAllValues(), ".json");
        JsonNode root = MAPPER.readTree(jsonFile.content());
        Assert.assertEquals(root.get("reportType").asText(), "browser");
        Assert.assertEquals(root.get("budgetMetric").asText(), "p95");
        Assert.assertEquals(root.get("browserActions").get(0).get("metric").asText(), "playwright.element.click");
        Assert.assertEquals(root.get("browserActions").get(0).get("budgetStatus").asText(), "WARN");
        Assert.assertEquals(root.get("pageLoads").get(0).get("metric").asText(), "https://example.test");
        Assert.assertEquals(root.get("pageLoads").get(0).get("budgetStatus").asText(), "PASS");

        String html = capturedContent(fileNameCaptor.getAllValues(), contentCaptor.getAllValues(), ".html").content();
        Assert.assertTrue(html.contains("Browser Actions"));
        Assert.assertTrue(html.contains("Page Loads"));
        Assert.assertTrue(html.contains("--shaft-primary"), html);
        Assert.assertTrue(html.contains("status-chip"), html);
    }

    @Test
    public void generatePerformanceReportShouldReturnFailureWhenBrowserBudgetViolationIsConfiguredToFail() {
        SHAFT.Properties.performance.set()
                .generatePerformanceReport(false)
                .browserActionPerformanceBudgets("playwright.element.click=1")
                .failOnBrowserPerformanceBudgetViolation(true);
        BrowserPerformanceExecutionReport.recordBrowserAction("playwright.element.click", TimeUnit.MILLISECONDS.toNanos(2));

        AssertionError failure = BrowserPerformanceExecutionReport.generatePerformanceReportAndGetFailure(1000L, 2000L);

        Assert.assertNotNull(failure);
        Assert.assertTrue(failure.getMessage().contains("playwright.element.click"));
    }

    @Test
    public void recordShouldSkipTimingWhenReportsAndBudgetsAreDisabled() {
        SHAFT.Properties.performance.set().generatePerformanceReport(false);

        BrowserPerformanceExecutionReport.recordBrowserAction("playwright.element.click", TimeUnit.MILLISECONDS.toNanos(2));

        Assert.assertTrue(BrowserPerformanceExecutionReport.getBrowserActionPerformanceData().isEmpty());
    }

    private CapturedFile capturedContent(List<String> fileNames, List<String> contents, String extension) {
        for (int i = 0; i < fileNames.size(); i++) {
            if (fileNames.get(i).endsWith(extension)) {
                return new CapturedFile(fileNames.get(i), contents.get(i));
            }
        }
        Assert.fail("No captured report file ended with " + extension);
        return null;
    }

    private record CapturedFile(String fileName, String content) {
    }
}
