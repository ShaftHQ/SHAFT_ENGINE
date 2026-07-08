package com.shaft.doctor.shard;

import java.nio.file.Path;
import java.util.List;

/**
 * Outcome of merging N shard blobs into one Allure result set plus a timeline "speedboard" and
 * cross-shard flaky-clustering summary.
 *
 * @param mergedAllureResultsDirectory one Allure results directory combining every shard's results
 * @param speedboardHtmlPath timeline HTML path (slowest tests first, flaky tests flagged)
 * @param shardCount number of shard blobs merged
 * @param totalResults total Allure result files merged (containers/attachments not counted)
 * @param flakyClusters tests observed with inconsistent pass/fail outcomes across shards
 * @param shardIntelligence per-shard doctor {@code ExecutionIntelligence} digests, for shards that had one
 * @param warnings safe warnings (e.g. an unreadable shard blob was skipped)
 */
public record MergedReport(
        Path mergedAllureResultsDirectory,
        Path speedboardHtmlPath,
        int shardCount,
        int totalResults,
        List<FlakyCluster> flakyClusters,
        List<ShardIntelligence> shardIntelligence,
        List<String> warnings) {
    public MergedReport {
        flakyClusters = flakyClusters == null ? List.of() : List.copyOf(flakyClusters);
        shardIntelligence = shardIntelligence == null ? List.of() : List.copyOf(shardIntelligence);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
