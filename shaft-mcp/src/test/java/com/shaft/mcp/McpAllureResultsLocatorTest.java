package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;

/**
 * Verifies newest-evidence discovery across the two shapes an MCP Doctor request may omit paths
 * for: a populated {@code allure-results} directory, and a SHAFT single-file
 * {@code *AllureReport.html} report inside an {@code allure-report} directory.
 */
class McpAllureResultsLocatorTest {

    @Test
    void latestReportFindsTheNewestSingleFileReportAcrossModules(@TempDir Path temp) throws IOException {
        Path staleReportDir = Files.createDirectories(temp.resolve("module-a/allure-report"));
        Path staleReport = staleReportDir.resolve("2026-01-01_00-00-00-000_AllureReport.html");
        Files.writeString(staleReport, "<html>stale</html>", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(staleReport, FileTime.fromMillis(1_000_000L));

        Path freshReportDir = Files.createDirectories(temp.resolve("module-b/allure-report"));
        Path freshReport = freshReportDir.resolve("2026-01-02_00-00-00-000_AllureReport.html");
        Files.writeString(freshReport, "<html>fresh</html>", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(freshReport, FileTime.fromMillis(2_000_000L));

        var found = McpAllureResultsLocator.latestReport(temp);

        assertTrue(found.isPresent());
        assertEquals(freshReport, found.get());
    }

    @Test
    void latestReportIgnoresNonReportHtmlAndSkippedDirectories(@TempDir Path temp) throws IOException {
        Files.createDirectories(temp.resolve("allure-report"));
        Files.writeString(temp.resolve("allure-report/index.html"), "<html>not a report</html>", StandardCharsets.UTF_8);
        Path gitReportDir = Files.createDirectories(temp.resolve(".git/allure-report"));
        Files.writeString(gitReportDir.resolve("AllureReport.html"), "<html>ignored</html>", StandardCharsets.UTF_8);

        var found = McpAllureResultsLocator.latestReport(temp);

        assertTrue(found.isEmpty());
    }

    @Test
    void latestEvidencePrefersTheNewerAllureResultsDirectoryOverAnOlderReport(@TempDir Path temp) throws IOException {
        Path resultsDir = Files.createDirectories(temp.resolve("target/allure-results"));
        Path result = resultsDir.resolve("fresh-result.json");
        Files.writeString(result, "{}", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(result, FileTime.fromMillis(2_000_000L));

        Path reportDir = Files.createDirectories(temp.resolve("allure-report"));
        Path report = reportDir.resolve("AllureReport.html");
        Files.writeString(report, "<html></html>", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(report, FileTime.fromMillis(1_000_000L));

        var found = McpAllureResultsLocator.latestEvidence(temp);

        assertTrue(found.isPresent());
        assertEquals(resultsDir, found.get());
    }

    @Test
    void latestEvidencePrefersTheNewerReportOverAnOlderAllureResultsDirectory(@TempDir Path temp) throws IOException {
        Path resultsDir = Files.createDirectories(temp.resolve("target/allure-results"));
        Path result = resultsDir.resolve("stale-result.json");
        Files.writeString(result, "{}", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(result, FileTime.fromMillis(1_000_000L));

        Path reportDir = Files.createDirectories(temp.resolve("allure-report"));
        Path report = reportDir.resolve("AllureReport.html");
        Files.writeString(report, "<html></html>", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(report, FileTime.fromMillis(2_000_000L));

        var found = McpAllureResultsLocator.latestEvidence(temp);

        assertTrue(found.isPresent());
        assertEquals(report, found.get());
    }

    @Test
    void latestEvidencePrefersTheAllureResultsDirectoryOnAnExactTimestampTie(@TempDir Path temp) throws IOException {
        FileTime tie = FileTime.fromMillis(1_500_000L);
        Path resultsDir = Files.createDirectories(temp.resolve("target/allure-results"));
        Path result = resultsDir.resolve("tied-result.json");
        Files.writeString(result, "{}", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(result, tie);

        Path reportDir = Files.createDirectories(temp.resolve("allure-report"));
        Path report = reportDir.resolve("AllureReport.html");
        Files.writeString(report, "<html></html>", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(report, tie);

        var found = McpAllureResultsLocator.latestEvidence(temp);

        assertTrue(found.isPresent());
        assertEquals(resultsDir, found.get(), "An exact tie must prefer the richer allure-results directory");
    }

    @Test
    void latestEvidenceFallsBackToWhicheverShapeIsPresent(@TempDir Path temp) throws IOException {
        assertTrue(McpAllureResultsLocator.latestEvidence(temp).isEmpty());

        Path reportDir = Files.createDirectories(temp.resolve("allure-report"));
        Path report = reportDir.resolve("AllureReport.html");
        Files.writeString(report, "<html></html>", StandardCharsets.UTF_8);

        var found = McpAllureResultsLocator.latestEvidence(temp);
        assertTrue(found.isPresent());
        assertEquals(report, found.get());
    }

    @Test
    void latestIgnoresEmptyAllureResultsDirectoriesAndDirectoriesInsideSkippedTools(@TempDir Path temp)
            throws IOException {
        // An allure-results directory with no *-result.json files must not be picked up.
        Files.createDirectories(temp.resolve("empty-module/allure-results"));

        // An allure-results directory buried inside a skipped tool directory must be ignored too.
        Path skippedResults = Files.createDirectories(temp.resolve("node_modules/some-dep/allure-results"));
        Files.writeString(skippedResults.resolve("skipped-result.json"), "{}", StandardCharsets.UTF_8);

        assertEquals(null, McpAllureResultsLocator.latest(temp));

        // Only once a real, populated allure-results directory exists is it discovered.
        Path realResults = Files.createDirectories(temp.resolve("real-module/allure-results"));
        Path realResult = realResults.resolve("real-result.json");
        Files.writeString(realResult, "{}", StandardCharsets.UTF_8);

        assertEquals(realResults, McpAllureResultsLocator.latest(temp));
    }

    @Test
    void latestPicksTheNewestAmongSeveralPopulatedAllureResultsDirectories(@TempDir Path temp) throws IOException {
        Path staleResults = Files.createDirectories(temp.resolve("module-a/allure-results"));
        Path staleResult = staleResults.resolve("stale-result.json");
        Files.writeString(staleResult, "{}", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(staleResult, FileTime.fromMillis(1_000_000L));

        Path freshResults = Files.createDirectories(temp.resolve("module-b/allure-results"));
        Path freshResult = freshResults.resolve("fresh-result.json");
        Files.writeString(freshResult, "{}", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(freshResult, FileTime.fromMillis(2_000_000L));

        assertEquals(freshResults, McpAllureResultsLocator.latest(temp));
    }

    @Test
    void latestAndLatestReportReturnNothingForANonexistentWorkspaceRoot() {
        Path missing = Path.of("this-workspace-does-not-exist-anywhere");

        assertEquals(null, McpAllureResultsLocator.latest(missing));
        assertTrue(McpAllureResultsLocator.latestReport(missing).isEmpty());
        assertTrue(McpAllureResultsLocator.latestEvidence(missing).isEmpty());
    }

    @Test
    void latestAndLatestReportFallBackSafelyWhenTheWorkspaceWalkItselfFails(@TempDir Path temp) {
        // SimpleFileVisitor.visitFileFailed swallows per-file/directory I/O errors (including a
        // missing root), so the only way to genuinely exercise latest()/latestReport()'s own
        // catch (IOException) is to fail Files.walkFileTree itself, not just the target path.
        try (MockedStatic<Files> filesMock = mockStatic(Files.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            filesMock.when(() -> Files.walkFileTree(eq(temp), any(), anyInt(), any(FileVisitor.class)))
                    .thenThrow(new IOException("simulated workspace walk failure"));

            assertEquals(null, McpAllureResultsLocator.latest(temp));
            assertTrue(McpAllureResultsLocator.latestReport(temp).isEmpty());
        }
    }

}
