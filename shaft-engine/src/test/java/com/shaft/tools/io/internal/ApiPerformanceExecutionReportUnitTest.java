package com.shaft.tools.io.internal;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

@Test(singleThreaded = true)
public class ApiPerformanceExecutionReportUnitTest {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void generatePerformanceReportShouldWriteHtmlAndStableJsonWithPercentilesAndBudgetStatus() throws Exception {
        SHAFT.Properties.performance.set()
                .generatePerformanceReport(true)
                .apiEndpointPerformanceBudgets("orders=500,users=650")
                .failOnApiPerformanceBudgetViolation(false);
        FileActions fileActions = Mockito.mock(FileActions.class);

        try (MockedStatic<FileActions> fileActionsMock = Mockito.mockStatic(FileActions.class)) {
            fileActionsMock.when(() -> FileActions.getInstance(true)).thenReturn(fileActions);

            ApiPerformanceExecutionReport.generatePerformanceReport(Map.of(
                    "users", List.of(100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0, 1000.0),
                    "orders", List.of(40.0, 50.0, 60.0, 70.0)
            ), 1000L, 2500L);
        }

        ArgumentCaptor<String> fileNameCaptor = ArgumentCaptor.forClass(String.class);
        ArgumentCaptor<String> contentCaptor = ArgumentCaptor.forClass(String.class);
        Mockito.verify(fileActions).createFolder(SHAFT.Properties.paths.performanceReportPath());
        Mockito.verify(fileActions, Mockito.times(2)).writeToFile(
                Mockito.eq(SHAFT.Properties.paths.performanceReportPath()),
                fileNameCaptor.capture(),
                contentCaptor.capture());

        String htmlFileName = capturedContent(fileNameCaptor.getAllValues(), contentCaptor.getAllValues(), ".html").fileName();
        CapturedFile jsonFile = capturedContent(fileNameCaptor.getAllValues(), contentCaptor.getAllValues(), ".json");
        Assert.assertEquals(htmlFileName.replace(".html", ""), jsonFile.fileName().replace(".json", ""),
                "HTML and JSON reports should share one timestamped basename.");

        JsonNode root = MAPPER.readTree(jsonFile.content());
        Assert.assertEquals(root.get("budgetMetric").asText(), "p95");
        Assert.assertFalse(root.get("failOnBudgetViolation").asBoolean());
        Assert.assertEquals(root.get("endpoints").get(0).get("endpoint").asText(), "orders");

        JsonNode users = endpoint(root, "users");
        Assert.assertEquals(users.get("requests").asLong(), 10);
        Assert.assertEquals(users.get("minMillis").asDouble(), 100.0);
        Assert.assertEquals(users.get("maxMillis").asDouble(), 1000.0);
        Assert.assertEquals(users.get("averageMillis").asDouble(), 550.0);
        Assert.assertEquals(users.get("p50Millis").asDouble(), 500.0);
        Assert.assertEquals(users.get("p90Millis").asDouble(), 900.0);
        Assert.assertEquals(users.get("p95Millis").asDouble(), 1000.0);
        Assert.assertEquals(users.get("p99Millis").asDouble(), 1000.0);
        Assert.assertEquals(users.get("budgetMillis").asDouble(), 650.0);
        Assert.assertEquals(users.get("budgetStatus").asText(), "WARN");

        String html = capturedContent(fileNameCaptor.getAllValues(), contentCaptor.getAllValues(), ".html").content();
        Assert.assertTrue(html.contains("<th>P50 Response Time (ms)</th>"));
        Assert.assertTrue(html.contains("<td>WARN</td>"));
    }

    @Test
    public void generatePerformanceReportShouldReturnFailureWhenBudgetViolationIsConfiguredToFail() {
        SHAFT.Properties.performance.set()
                .generatePerformanceReport(true)
                .apiEndpointPerformanceBudgets("users=650")
                .failOnApiPerformanceBudgetViolation(true);
        FileActions fileActions = Mockito.mock(FileActions.class);

        AssertionError failure;
        try (MockedStatic<FileActions> fileActionsMock = Mockito.mockStatic(FileActions.class)) {
            fileActionsMock.when(() -> FileActions.getInstance(true)).thenReturn(fileActions);

            failure = ApiPerformanceExecutionReport.generatePerformanceReportAndGetFailure(Map.of(
                    "users", List.of(100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0, 1000.0)
            ), 1000L, 2500L);
        }

        Assert.assertNotNull(failure);
        Assert.assertTrue(failure.getMessage().contains("users"));
    }

    private JsonNode endpoint(JsonNode root, String endpoint) {
        for (JsonNode candidate : root.get("endpoints")) {
            if (endpoint.equals(candidate.get("endpoint").asText())) {
                return candidate;
            }
        }
        Assert.fail("Endpoint not found: " + endpoint);
        return null;
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
