package com.shaft.listeners.internal;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.regex.Pattern;

/**
 * Deterministic {@code -Dshaft.shard=N/M} test partitioning by a stable SHA-256 hash of
 * {@code className + "#" + methodName}. The same input always yields the same shard, and the
 * union of every shard {@code 1..M} for a fixed {@code M} equals the full suite with no overlap,
 * since every class+method maps to exactly one shard index.
 */
public final class ShardPartitioner {
    private static final Pattern SHARD_PATTERN = Pattern.compile("(\\d+)\\s*/\\s*(\\d+)");

    private ShardPartitioner() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Parses a {@code "N/M"} shard specification (1-based shard index / total shard count).
     *
     * @param spec raw {@code shaft.shard} system property value, e.g. {@code "3/8"}
     * @return parsed specification, or {@code Spec.disabled()} when {@code spec} is blank or malformed
     */
    public static Spec parse(String spec) {
        if (spec == null || spec.isBlank()) {
            return Spec.disabled();
        }
        var matcher = SHARD_PATTERN.matcher(spec.trim());
        if (!matcher.matches()) {
            return Spec.disabled();
        }
        int index;
        int total;
        try {
            index = Integer.parseInt(matcher.group(1));
            total = Integer.parseInt(matcher.group(2));
        } catch (NumberFormatException tooManyDigitsToFitAnInt) {
            // The regex only matches digit runs, but a pathologically long run (more digits than
            // an int can hold) still fails to parse -- treat that the same as any other malformed
            // spec rather than letting an unbounded system property crash test startup.
            return Spec.disabled();
        }
        if (total < 1 || index < 1 || index > total) {
            return Spec.disabled();
        }
        return new Spec(true, index, total);
    }

    /**
     * Returns whether {@code className#methodName} is assigned to the 1-based shard {@code shardIndex}
     * out of {@code totalShards}.
     *
     * @param className fully qualified test class name
     * @param methodName test method name
     * @param shardIndex 1-based shard index to check membership against
     * @param totalShards total number of shards
     * @return {@code true} when this class+method belongs to {@code shardIndex}
     */
    public static boolean belongsToShard(String className, String methodName, int shardIndex, int totalShards) {
        if (totalShards <= 1) {
            return true;
        }
        return shardOf(className, methodName, totalShards) == shardIndex;
    }

    /**
     * Returns the 1-based shard index {@code className#methodName} is assigned to, out of
     * {@code totalShards}.
     *
     * @param className fully qualified test class name
     * @param methodName test method name
     * @param totalShards total number of shards
     * @return 1-based shard index in {@code [1, totalShards]}
     */
    public static int shardOf(String className, String methodName, int totalShards) {
        if (totalShards <= 1) {
            return 1;
        }
        long hash = stableHash(safe(className) + "#" + safe(methodName));
        // Floor-mod (not %) so the result is always non-negative regardless of hash sign.
        return (int) Math.floorMod(hash, (long) totalShards) + 1;
    }

    private static String safe(String value) {
        return value == null ? "" : value;
    }

    private static long stableHash(String key) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] bytes = digest.digest(key.getBytes(StandardCharsets.UTF_8));
            long hash = 0L;
            for (int i = 0; i < 8; i++) {
                hash = (hash << 8) | (bytes[i] & 0xFFL);
            }
            return hash;
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
        }
    }

    /**
     * Parsed {@code shaft.shard} specification.
     *
     * @param enabled whether sharding is active
     * @param index 1-based shard index this run executes; meaningless when {@code enabled} is {@code false}
     * @param total total number of shards; meaningless when {@code enabled} is {@code false}
     */
    public record Spec(boolean enabled, int index, int total) {
        /**
         * Returns a disabled specification (no partitioning applied).
         *
         * @return disabled spec
         */
        public static Spec disabled() {
            return new Spec(false, 1, 1);
        }

        /**
         * Returns whether {@code className#methodName} belongs to this shard.
         *
         * @param className fully qualified test class name
         * @param methodName test method name
         * @return {@code true} when this class+method belongs to this shard, or when sharding is disabled
         */
        public boolean includes(String className, String methodName) {
            return !enabled || ShardPartitioner.belongsToShard(className, methodName, index, total);
        }
    }
}
