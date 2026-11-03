package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AllureReportActionsTest {
    @Test
    void findsAllureResultsDirectory(@TempDir Path root) throws Exception {
        assertNull(AllureReportActions.findAllureResults(root));
        Path results = root.resolve("target/allure-results");
        Files.createDirectories(results);
        assertEquals(results, AllureReportActions.findAllureResults(root));
    }

    @Test
    void prefersRootAllureResultsWhenPresent(@TempDir Path root) throws Exception {
        Path rootResults = root.resolve("allure-results");
        Path targetResults = root.resolve("target/allure-results");
        Files.createDirectories(rootResults);
        Files.createDirectories(targetResults);
        assertEquals(rootResults, AllureReportActions.findAllureResults(root));
    }

    @Test
    void findsLatestScreenshotUnderResults(@TempDir Path root) throws Exception {
        assertNull(AllureReportActions.findLatestScreenshot(root));
        Path results = root.resolve("build/allure-results");
        Files.createDirectories(results);
        Path older = results.resolve("older.png");
        Path newer = results.resolve("newer.jpg");
        Files.writeString(older, "old");
        Thread.sleep(5);
        Files.writeString(newer, "new");
        assertEquals(newer, AllureReportActions.findLatestScreenshot(root));
    }

    @Test
    void evidenceMessageCoversMissingAndPresentResults(@TempDir Path root) throws Exception {
        String missing = AllureReportActions.evidenceMessage(root);
        assertTrue(missing.contains("No Allure results"));
        Path results = root.resolve("allure-results");
        Files.createDirectories(results);
        String ready = AllureReportActions.evidenceMessage(root);
        assertTrue(ready.contains("Allure results ready"));
        assertTrue(ready.contains("allure-results"));
    }
}
