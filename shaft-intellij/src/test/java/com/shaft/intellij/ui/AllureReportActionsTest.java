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


    @Test
    void findResultsAndScreenshot(@TempDir Path root) throws Exception {
        assertEquals(null, AllureReportActions.findAllureResults(root));
        Path results = root.resolve("allure-results");
        Files.createDirectories(results);
        assertEquals(results, AllureReportActions.findAllureResults(root));
        Path shotDir = results.resolve("attachments");
        Files.createDirectories(shotDir);
        Path older = shotDir.resolve("a.png");
        Path newer = shotDir.resolve("b.png");
        Files.writeString(older, "old");
        Files.writeString(newer, "new");
        Files.setLastModifiedTime(older, java.nio.file.attribute.FileTime.fromMillis(1_000));
        Files.setLastModifiedTime(newer, java.nio.file.attribute.FileTime.fromMillis(2_000));
        Path found = AllureReportActions.findLatestScreenshot(root);
        assertTrue(found != null && found.getFileName().toString().endsWith(".png"));
        assertTrue(AllureReportActions.evidenceMessage(root).contains("Allure results ready"));
        assertTrue(AllureReportActions.evidenceMessage(root.resolve("missing-proj")).contains("No Allure"));
        assertTrue(AllureReportActions.listTagsAndGroups(results).isEmpty());
        Files.writeString(results.resolve("x-result.json"),
                "{\"labels\":[{\"name\":\"tag\",\"value\":\"smoke\"},{\"name\":\"epic\",\"value\":\"e1\"}]}");
        var labels = AllureReportActions.listTagsAndGroups(results);
        assertTrue(labels.stream().anyMatch(l -> l.contains("smoke")));
    }


    @Test
    void coversLabelExtractAndCleanReportDirs(@TempDir Path root) throws Exception {
        Path results = root.resolve("target/allure-results");
        Files.createDirectories(results);
        assertEquals(results, AllureReportActions.findAllureResults(root));
        Files.writeString(results.resolve("a-result.json"),
                "{\"labels\":[{\"name\":\"suite\",\"value\":\"s1\"},{\"name\":\"story\",\"value\":\"st\"},{\"name\":\"package\",\"value\":\"p\"},{\"name\":\"tag\",\"value\":\"\"},{\"name\":\"tag\"}]}");
        var labels = AllureReportActions.listTagsAndGroups(results);
        assertTrue(labels.stream().anyMatch(l -> l.startsWith("suite:")));
        assertTrue(labels.stream().anyMatch(l -> l.startsWith("story:")));
        Path report = root.resolve("allure-report");
        Files.createDirectories(report.resolve("x"));
        Files.writeString(report.resolve("x/f.txt"), "z");
        var deleted = AllureReportActions.cleanResults(root);
        assertTrue(deleted.stream().anyMatch(p -> p.getFileName().toString().contains("allure")));
        Path results2 = root.resolve("build/allure-results");
        Files.createDirectories(results2);
        Files.writeString(results2.resolve("shot.jpg"), "j");
        Path shot = AllureReportActions.findLatestScreenshot(root);
        assertTrue(shot != null && shot.toString().endsWith(".jpg"));
    }
}
