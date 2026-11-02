package com.shaft.reportaggregate;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShardMergeCliTest {

    @TempDir
    Path tempDir;

    @Test
    void mainMergesShardsAndPrintsASummary() throws Exception {
        Path shard = Files.createDirectories(tempDir.resolve("shard-1/allure-results"));
        Files.writeString(shard.resolve("t-result.json"),
                "{\"fullName\":\"com.example.Test.t\",\"status\":\"passed\",\"start\":0,\"stop\":10}",
                StandardCharsets.UTF_8);
        Path output = tempDir.resolve("merged");

        String stdout = captureStdout(() -> ShardMergeCli.main(new String[]{
                "--output", output.toString(), shard.getParent().toString()}));

        assertTrue(stdout.contains("Merged 1 shard(s), 1 Allure result(s)"), stdout);
        assertTrue(Files.isDirectory(output.resolve("allure-results")));
        assertTrue(Files.isRegularFile(output.resolve("speedboard.html")));
    }

    @Test
    void mainMergesShardsWithExecutionIntelligenceAndTraces() throws Exception {
        Path shard1 = writeShardWithIntelligence("shard-1", "com.example.Test.a", "failed", "LOCATOR", "HIGH");
        Path shard2 = writeShardWithIntelligence("shard-2", "com.example.Test.b", "passed", "TIMING_SYNCHRONIZATION", "MEDIUM");
        Path output = tempDir.resolve("merged");

        String stdout = captureStdout(() -> ShardMergeCli.main(new String[]{
                "--output", output.toString(), shard1.toString(), shard2.toString()}));

        assertTrue(stdout.contains("Merged 2 shard(s), 2 Allure result(s)"), stdout);
        assertTrue(Files.isDirectory(output.resolve("allure-results")));
        assertTrue(Files.isRegularFile(output.resolve("speedboard.html")));
        assertTrue(Files.isDirectory(output.resolve("shaft-traces").resolve("shard-1")));
        assertTrue(Files.isDirectory(output.resolve("shaft-traces").resolve("shard-2")));
        String speedboard = Files.readString(output.resolve("speedboard.html"), StandardCharsets.UTF_8);
        assertTrue(speedboard.contains("Doctor triage across shards"), speedboard);
        assertTrue(speedboard.contains("LOCATOR"), speedboard);
        assertTrue(speedboard.contains("TIMING_SYNCHRONIZATION"), speedboard);
    }

    @Test
    void mainRequiresAtLeastOneShardDirectory() {
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> ShardMergeCli.main(new String[]{"--output", tempDir.resolve("merged").toString()}));

        assertTrue(failure.getMessage().contains("At least one shard blob directory is required"));
    }

    private Path writeShardWithIntelligence(String shardName, String fullName, String status,
            String primaryCause, String confidence) throws Exception {
        Path shard = Files.createDirectories(tempDir.resolve(shardName).resolve("allure-results"));
        Files.writeString(shard.resolve("t-result.json"),
                "{\"fullName\":\"" + fullName + "\",\"status\":\"" + status + "\",\"start\":0,\"stop\":10}",
                StandardCharsets.UTF_8);
        Path traceDirectory = Files.createDirectories(
                tempDir.resolve(shardName).resolve("shaft-traces").resolve(fullName));
        Files.writeString(traceDirectory.resolve("index.json"), "{}", StandardCharsets.UTF_8);
        Files.writeString(tempDir.resolve(shardName).resolve("execution-intelligence.json"), """
                {
                  "schemaVersion": "1.0",
                  "bundleId": "bundle-1",
                  "totalAllureResults": 1,
                  "failingAttempts": 1,
                  "hiddenRetryFailures": 0,
                  "recurringFailures": 0,
                  "primaryCause": "%s",
                  "confidence": "%s",
                  "summary": "Deterministic summary for %s."
                }
                """.formatted(primaryCause, confidence, shardName), StandardCharsets.UTF_8);
        return shard.getParent();
    }

    private static String captureStdout(Runnable action) {
        PrintStream original = System.out;
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(buffer, true, StandardCharsets.UTF_8));
            action.run();
        } finally {
            System.setOut(original);
        }
        return buffer.toString(StandardCharsets.UTF_8);
    }
}
