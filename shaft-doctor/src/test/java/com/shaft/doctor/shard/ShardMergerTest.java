package com.shaft.doctor.shard;

import com.shaft.tools.internal.support.ReportHtmlTheme;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShardMergerTest {

    @TempDir
    Path tempDir;

    @Test
    void mergesAllureResultsFromEveryShardIntoOneDirectory() throws Exception {
        Path shard1 = writeShard("shard-1", result("test-a", "com.example.Test.a", "passed", 100, 200));
        Path shard2 = writeShard("shard-2", result("test-b", "com.example.Test.b", "passed", 300, 450));

        MergedReport report = ShardMerger.merge(List.of(shard1, shard2), tempDir.resolve("merged"));

        assertEquals(2, report.shardCount());
        assertEquals(2, report.totalResults());
        assertTrue(Files.isDirectory(report.mergedAllureResultsDirectory()));
        assertTrue(Files.exists(report.mergedAllureResultsDirectory().resolve("test-a-result.json")));
        assertTrue(Files.exists(report.mergedAllureResultsDirectory().resolve("test-b-result.json")));
        assertTrue(report.warnings().isEmpty(), report.warnings().toString());
    }

    @Test
    void detectsFlakyClusterAcrossShardsWithInconsistentOutcomes() throws Exception {
        Path shard1 = writeShard("shard-1", result("t1", "com.example.Flaky.test", "passed", 0, 100));
        Path shard2 = writeShard("shard-2", result("t2", "com.example.Flaky.test", "failed", 0, 120));
        Path shard3 = writeShard("shard-3", result("t3", "com.example.Stable.test", "passed", 0, 90));

        MergedReport report = ShardMerger.merge(List.of(shard1, shard2, shard3), tempDir.resolve("merged"));

        assertEquals(1, report.flakyClusters().size());
        FlakyCluster cluster = report.flakyClusters().getFirst();
        assertEquals("com.example.Flaky.test", cluster.fullName());
        assertEquals(1, cluster.passedCount());
        assertEquals(1, cluster.failedCount());
        assertEquals(2, cluster.shardIds().size());
    }

    @Test
    void speedboardHtmlListsSlowestTestsFirstAndFlagsFlakyClusters() throws Exception {
        Path shard1 = writeShard("shard-1",
                result("fast", "com.example.Fast.test", "passed", 0, 10),
                result("slow", "com.example.Slow.test", "passed", 0, 5000));

        MergedReport report = ShardMerger.merge(List.of(shard1), tempDir.resolve("merged"));

        assertTrue(Files.isRegularFile(report.speedboardHtmlPath()));
        String html = Files.readString(report.speedboardHtmlPath(), StandardCharsets.UTF_8);
        int slowIndex = html.indexOf("com.example.Slow.test");
        int fastIndex = html.indexOf("com.example.Fast.test");
        assertTrue(slowIndex >= 0 && fastIndex >= 0 && slowIndex < fastIndex,
                "The slower test must be listed before the faster one, got:\n" + html);
    }

    @Test
    void speedboardUsesTheSharedShaftReportThemeNotOffPaletteHardcodedColors() throws Exception {
        Path shard = writeShard("shard-1",
                result("t1", "com.example.Flaky.test", "passed", 0, 100));
        Path shard2 = writeShard("shard-2",
                result("t2", "com.example.Flaky.test", "failed", 0, 120));

        MergedReport report = ShardMerger.merge(List.of(shard, shard2), tempDir.resolve("merged"));
        String html = Files.readString(report.speedboardHtmlPath(), StandardCharsets.UTF_8);

        // Convergence (#3534 "cheap first convergence step"): the speedboard embeds the canonical,
        // theme-aware SHAFT report stylesheet and draws its colors from the shared palette.
        assertTrue(html.contains(ReportHtmlTheme.style()),
                "Speedboard must embed the shared ReportHtmlTheme stylesheet.");
        assertTrue(html.contains("var(--shaft-fail)") && html.contains("var(--shaft-pass)")
                        && html.contains("var(--shaft-border)"),
                "Speedboard colors must reference the shared palette variables.");
        // Drift guard: the former off-palette hardcoded colors must not creep back in.
        for (String offPalette : List.of("#fff3cd", "#a33", "#292", "#f4f4f4", "#ccc", "font-family:sans-serif")) {
            assertFalse(html.contains(offPalette),
                    "Off-palette token \"" + offPalette + "\" must not appear in the themed speedboard:\n" + html);
        }
    }

    @Test
    void missingShardBlobIsSkippedWithAWarningRatherThanThrowing() {
        MergedReport report = ShardMerger.merge(
                List.of(tempDir.resolve("does-not-exist")), tempDir.resolve("merged"));

        assertEquals(0, report.shardCount());
        assertEquals(0, report.totalResults());
        assertFalse(report.warnings().isEmpty());
    }

    @Test
    void shardWithNoAllureResultsDirectoryIsCountedButProducesNoResults() throws Exception {
        Path shard = Files.createDirectories(tempDir.resolve("empty-shard"));

        MergedReport report = ShardMerger.merge(List.of(shard), tempDir.resolve("merged"));

        assertEquals(1, report.shardCount());
        assertEquals(0, report.totalResults());
        assertFalse(report.warnings().isEmpty());
    }

    @Test
    void copiesShaftTracesFromEveryShardIntoMergedOutputNamespacedByShard() throws Exception {
        Path shard1 = writeShard("shard-1", result("test-a", "com.example.Test.a", "passed", 0, 100));
        writeTrace(shard1, "com.example.Test.a", "{\"testId\": \"com.example.Test.a\"}");
        Path shard2 = writeShard("shard-2", result("test-b", "com.example.Test.b", "passed", 0, 100));
        writeTrace(shard2, "com.example.Test.b", "{\"testId\": \"com.example.Test.b\"}");

        MergedReport report = ShardMerger.merge(List.of(shard1, shard2), tempDir.resolve("merged"));

        Path mergedTraces = report.mergedAllureResultsDirectory().getParent().resolve("shaft-traces");
        assertTrue(Files.exists(mergedTraces.resolve("shard-1").resolve("com.example.Test.a").resolve("index.json")));
        assertTrue(Files.exists(mergedTraces.resolve("shard-2").resolve("com.example.Test.b").resolve("index.json")));
        assertTrue(report.warnings().isEmpty(), report.warnings().toString());
    }

    @Test
    void shardWithNoTracesDirectoryMergesWithoutWarning() throws Exception {
        Path shard = writeShard("shard-1", result("test-a", "com.example.Test.a", "passed", 0, 100));

        MergedReport report = ShardMerger.merge(List.of(shard), tempDir.resolve("merged"));

        assertTrue(report.warnings().isEmpty(), report.warnings().toString());
    }

    @Test
    void aggregatesExecutionIntelligenceAcrossShardsIntoSpeedboard() throws Exception {
        Path shard1 = writeShard("shard-1", result("test-a", "com.example.Test.a", "failed", 0, 100));
        writeExecutionIntelligence(shard1, "LOCATOR", "HIGH", 3, 1, 2, "Locator drift on shard 1.");
        Path shard2 = writeShard("shard-2", result("test-b", "com.example.Test.b", "passed", 0, 100));
        writeExecutionIntelligence(shard2, "TIMING_SYNCHRONIZATION", "MEDIUM", 1, 0, 0, "Timing flake on shard 2.");

        MergedReport report = ShardMerger.merge(List.of(shard1, shard2), tempDir.resolve("merged"));

        assertEquals(2, report.shardIntelligence().size());
        ShardIntelligence first = report.shardIntelligence().get(0);
        assertEquals("shard-1", first.shardId());
        assertEquals("LOCATOR", first.primaryCause());
        assertEquals("HIGH", first.confidence());
        assertEquals(3, first.failingAttempts());
        String speedboard = Files.readString(report.speedboardHtmlPath(), StandardCharsets.UTF_8);
        assertTrue(speedboard.contains("Doctor triage across shards"), speedboard);
        assertTrue(speedboard.contains("Locator drift on shard 1."), speedboard);
        assertTrue(speedboard.contains("Timing flake on shard 2."), speedboard);
    }

    @Test
    void malformedExecutionIntelligenceIsSkippedWithAWarningRatherThanThrowing() throws Exception {
        Path shard = writeShard("shard-1", result("test-a", "com.example.Test.a", "passed", 0, 100));
        Files.writeString(shard.resolve("execution-intelligence.json"), "{not valid json", StandardCharsets.UTF_8);

        MergedReport report = ShardMerger.merge(List.of(shard), tempDir.resolve("merged"));

        assertTrue(report.shardIntelligence().isEmpty());
        assertFalse(report.warnings().isEmpty());
    }

    private void writeTrace(Path shard, String testId, String indexJson) throws Exception {
        Path traceDirectory = Files.createDirectories(shard.resolve("shaft-traces").resolve(testId));
        Files.writeString(traceDirectory.resolve("index.json"), indexJson, StandardCharsets.UTF_8);
    }

    private void writeExecutionIntelligence(Path shard, String primaryCause, String confidence,
            int failingAttempts, int hiddenRetryFailures, int recurringFailures, String summary) throws Exception {
        Files.writeString(shard.resolve("execution-intelligence.json"), """
                {
                  "schemaVersion": "1.0",
                  "bundleId": "bundle-1",
                  "totalAllureResults": 1,
                  "failingAttempts": %d,
                  "hiddenRetryFailures": %d,
                  "recurringFailures": %d,
                  "primaryCause": "%s",
                  "confidence": "%s",
                  "summary": "%s"
                }
                """.formatted(failingAttempts, hiddenRetryFailures, recurringFailures, primaryCause, confidence, summary),
                StandardCharsets.UTF_8);
    }

    private Path writeShard(String name, String... resultJsonFiles) throws Exception {
        Path shard = Files.createDirectories(tempDir.resolve(name));
        Path allureResults = Files.createDirectories(shard.resolve("allure-results"));
        for (int i = 0; i < resultJsonFiles.length; i++) {
            String content = resultJsonFiles[i];
            String testId = content.substring(0, content.indexOf('|'));
            Files.writeString(allureResults.resolve(testId + "-result.json"),
                    content.substring(content.indexOf('|') + 1), StandardCharsets.UTF_8);
        }
        return shard;
    }

    private static String result(String id, String fullName, String status, long start, long stop) {
        return id + "|{\"fullName\":\"" + fullName + "\",\"name\":\"" + fullName
                + "\",\"status\":\"" + status + "\",\"start\":" + start + ",\"stop\":" + stop + "}";
    }
}
