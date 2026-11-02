package com.shaft.doctor.shard;

import java.util.List;

/**
 * One test observed with inconsistent outcomes across merged shards -- passed in at least one
 * shard/attempt and failed (or broken) in at least one other, which flags it as a flaky-clustering
 * candidate rather than a deterministic failure.
 *
 * @param fullName fully qualified {@code class.method} test identifier
 * @param passedCount observed passed outcomes across all merged shards
 * @param failedCount observed failed/broken outcomes across all merged shards
 * @param shardIds shard identifiers (blob directory names) this test was observed in, in merge order
 */
public record FlakyCluster(String fullName, int passedCount, int failedCount, List<String> shardIds) {
    public FlakyCluster {
        fullName = fullName == null ? "" : fullName;
        passedCount = Math.max(0, passedCount);
        failedCount = Math.max(0, failedCount);
        shardIds = shardIds == null ? List.of() : List.copyOf(shardIds);
    }
}
