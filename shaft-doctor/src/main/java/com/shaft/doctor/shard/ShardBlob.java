package com.shaft.doctor.shard;

import java.nio.file.Path;

/**
 * One resolved per-shard blob directory laid out by convention as:
 * <pre>{@code
 * <shardRoot>/
 *   allure-results/*.json        (raw Allure result/container/attachment files)
 *   shaft-traces/...             (optional, copied through untouched)
 *   execution-intelligence.json  (optional, doctor ExecutionIntelligence for this shard)
 * }</pre>
 *
 * @param root shard blob root directory
 * @param allureResults {@code allure-results} subdirectory, may not exist
 * @param traces {@code shaft-traces} subdirectory, may not exist
 * @param executionIntelligence {@code execution-intelligence.json} file, may not exist
 */
public record ShardBlob(Path root, Path allureResults, Path traces, Path executionIntelligence) {
    /**
     * Resolves the conventional blob layout under a shard root directory.
     *
     * @param root shard blob root directory
     * @return resolved shard blob paths (existence is not checked here)
     */
    public static ShardBlob of(Path root) {
        Path normalized = root.toAbsolutePath().normalize();
        return new ShardBlob(
                normalized,
                normalized.resolve("allure-results"),
                normalized.resolve("shaft-traces"),
                normalized.resolve("execution-intelligence.json"));
    }
}
