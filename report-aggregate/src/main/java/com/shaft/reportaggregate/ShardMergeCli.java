package com.shaft.reportaggregate;

import com.shaft.doctor.shard.MergedReport;
import com.shaft.doctor.shard.ShardMerger;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * CLI entry point merging N shard blob directories into one Allure result set plus a speedboard
 * HTML and flaky-clustering summary. The merge logic itself ({@link ShardMerger}) lives in
 * {@code shaft-doctor} so it can also be called directly by {@code shaft-mcp}'s
 * {@code report_merge_shards} tool without a circular module dependency
 * ({@code report-aggregate} already depends on both {@code shaft-doctor} and {@code shaft-mcp}).
 * Usage:
 * <pre>{@code
 * java -cp report-aggregate.jar com.shaft.reportaggregate.ShardMergeCli \
 *     --output target/merged-report \
 *     target/shard-blobs/1 target/shard-blobs/2 target/shard-blobs/3
 * }</pre>
 */
public final class ShardMergeCli {
    private ShardMergeCli() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * CLI entry point.
     *
     * @param args {@code --output <dir>} followed by one or more shard blob directories
     */
    public static void main(String[] args) {
        Arguments arguments = parse(args);
        MergedReport report = ShardMerger.merge(arguments.shardRoots(), arguments.output());
        System.out.println("Merged " + report.shardCount() + " shard(s), " + report.totalResults()
                + " Allure result(s) into " + report.mergedAllureResultsDirectory());
        System.out.println("Speedboard: " + report.speedboardHtmlPath());
        System.out.println("Flaky clusters: " + report.flakyClusters().size());
        report.warnings().forEach(warning -> System.out.println("WARNING: " + warning));
    }

    private static Arguments parse(String[] args) {
        Path output = Path.of("target", "merged-report");
        List<Path> shardRoots = new ArrayList<>();
        for (int i = 0; i < args.length; i++) {
            if ("--output".equals(args[i]) && i + 1 < args.length) {
                output = Path.of(args[++i]);
            } else {
                shardRoots.add(Path.of(args[i]));
            }
        }
        if (shardRoots.isEmpty()) {
            throw new IllegalArgumentException(
                    "At least one shard blob directory is required. Usage: ShardMergeCli [--output <dir>] <shardDir>...");
        }
        return new Arguments(output, List.copyOf(shardRoots));
    }

    private record Arguments(Path output, List<Path> shardRoots) {
    }
}
