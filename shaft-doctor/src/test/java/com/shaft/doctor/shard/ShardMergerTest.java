package com.shaft.doctor.shard;

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
