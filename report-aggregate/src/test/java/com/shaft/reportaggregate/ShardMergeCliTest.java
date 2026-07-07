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
    void mainRequiresAtLeastOneShardDirectory() {
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> ShardMergeCli.main(new String[]{"--output", tempDir.resolve("merged").toString()}));

        assertTrue(failure.getMessage().contains("At least one shard blob directory is required"));
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
