package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

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

    @Test
    void mergesCleansAndListsTags(@TempDir Path root) throws Exception {
        Path android = root.resolve("android");
        Path ios = root.resolve("ios");
        Files.createDirectories(android);
        Files.createDirectories(ios);
        Files.writeString(android.resolve("a-result.json"),
                "{\"labels\":[{\"name\":\"tag\",\"value\":\"android\"},{\"name\":\"suite\",\"value\":\"smoke\"}]}");
        Files.writeString(ios.resolve("b-result.json"),
                "{\"labels\":[{\"name\":\"tag\",\"value\":\"ios\"}]}");
        Path merged = AllureReportActions.mergeResults(root.resolve("merged"), List.of(android, ios));
        assertTrue(Files.isRegularFile(merged.resolve("a-result.json")));
        assertTrue(Files.isRegularFile(merged.resolve("b-result.json")));
        var labels = AllureReportActions.listTagsAndGroups(merged);
        assertTrue(labels.stream().anyMatch(l -> l.contains("android")));
        Path project = root.resolve("proj");
        Files.createDirectories(project.resolve("allure-results"));
        Files.writeString(project.resolve("allure-results/x.txt"), "x");
        var deleted = AllureReportActions.cleanResults(project);
        assertTrue(deleted.stream().anyMatch(p -> p.endsWith("allure-results")));
        assertTrue(Files.notExists(project.resolve("allure-results")));
    }

    @Test
    void mergeRequiresSourcesAndSkipsMissing(@TempDir Path root) throws Exception {
        Path dest = root.resolve("out");
        org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class,
                () -> AllureReportActions.mergeResults(dest, List.of()));
        Path only = root.resolve("only");
        Files.createDirectories(only);
        Files.writeString(only.resolve("a.json"), "{}");
        Path merged = AllureReportActions.mergeResults(dest, java.util.Arrays.asList(only, root.resolve("missing"), null));
        assertTrue(Files.isRegularFile(merged.resolve("a.json")));
        // collision suffix
        Files.writeString(only.resolve("a.json"), "{\"n\":2}");
        AllureReportActions.mergeResults(dest, List.of(only));
        assertTrue(Files.exists(dest.resolve("a.json")));
        long jsonCount = Files.list(dest).filter(f -> f.getFileName().toString().startsWith("a")).count();
        assertTrue(jsonCount >= 2);
        assertTrue(AllureReportActions.listTagsAndGroups(root.resolve("nope")).isEmpty());
    }
}
