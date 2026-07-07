package com.shaft.listeners.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ShardPartitionerUnitTest {

    @Test
    public void parseAcceptsAWellFormedSpec() {
        ShardPartitioner.Spec spec = ShardPartitioner.parse("3/8");

        Assert.assertTrue(spec.enabled());
        Assert.assertEquals(spec.index(), 3);
        Assert.assertEquals(spec.total(), 8);
    }

    @Test
    public void parseReturnsDisabledForBlankOrMalformedSpecs() {
        Assert.assertFalse(ShardPartitioner.parse(null).enabled());
        Assert.assertFalse(ShardPartitioner.parse("").enabled());
        Assert.assertFalse(ShardPartitioner.parse("  ").enabled());
        Assert.assertFalse(ShardPartitioner.parse("not-a-shard-spec").enabled());
        Assert.assertFalse(ShardPartitioner.parse("0/8").enabled(), "Shard index must be 1-based.");
        Assert.assertFalse(ShardPartitioner.parse("9/8").enabled(), "Shard index cannot exceed total.");
        Assert.assertFalse(ShardPartitioner.parse("1/0").enabled(), "Total shards must be positive.");
    }

    @Test
    public void parseReturnsDisabledRatherThanThrowingWhenDigitsOverflowAnInt() {
        // The regex only matches digit runs; a run far longer than Integer.MAX_VALUE's digits
        // still matches the pattern but cannot fit in an int, so parse() must degrade to disabled
        // rather than propagate a NumberFormatException.
        ShardPartitioner.Spec spec = ShardPartitioner.parse("99999999999999999999/8");

        Assert.assertFalse(spec.enabled());
    }

    @Test
    public void sameInputAlwaysYieldsTheSameShard() {
        int shardA = ShardPartitioner.shardOf("com.example.CheckoutTest", "payShouldSucceed", 8);
        int shardB = ShardPartitioner.shardOf("com.example.CheckoutTest", "payShouldSucceed", 8);

        Assert.assertEquals(shardA, shardB);
    }

    @Test
    public void everyShardIndexIsWithinBounds() {
        for (int i = 0; i < 500; i++) {
            int shard = ShardPartitioner.shardOf("com.example.Test" + i, "method" + i, 6);
            Assert.assertTrue(shard >= 1 && shard <= 6, "Shard " + shard + " out of [1,6] bounds");
        }
    }

    @Test
    public void unionOfAllShardsEqualsTheFullSuiteWithNoOverlap() {
        int totalShards = 5;
        List<String> methods = new ArrayList<>();
        for (int i = 0; i < 200; i++) {
            methods.add("com.example.Suite" + (i % 17) + "#method" + i);
        }

        Set<String> unassigned = new HashSet<>(methods);
        Set<String> seenAcrossShards = new HashSet<>();
        for (int shardIndex = 1; shardIndex <= totalShards; shardIndex++) {
            for (String method : methods) {
                String[] parts = method.split("#");
                if (ShardPartitioner.belongsToShard(parts[0], parts[1], shardIndex, totalShards)) {
                    Assert.assertTrue(seenAcrossShards.add(method),
                            method + " was assigned to more than one shard -- shards must not overlap.");
                    unassigned.remove(method);
                }
            }
        }

        Assert.assertTrue(unassigned.isEmpty(),
                "Every method must belong to exactly one shard; unassigned: " + unassigned);
    }

    @Test
    public void singleShardOrDisabledSpecIncludesEverything() {
        Assert.assertTrue(ShardPartitioner.belongsToShard("com.example.Test", "method", 1, 1));
        Assert.assertTrue(ShardPartitioner.Spec.disabled().includes("com.example.Test", "method"));
    }

    @Test
    public void specIncludesDelegatesToBelongsToShard() {
        ShardPartitioner.Spec spec = ShardPartitioner.parse("2/4");
        int actualShard = ShardPartitioner.shardOf("com.example.CheckoutTest", "payShouldSucceed", 4);

        boolean expectedInclusion = actualShard == 2;
        Assert.assertEquals(spec.includes("com.example.CheckoutTest", "payShouldSucceed"), expectedInclusion);
    }

    @Test
    public void distributionIsReasonablyBalancedAcrossManyMethods() {
        int totalShards = 4;
        int[] counts = new int[totalShards + 1];
        int methodCount = 4000;
        for (int i = 0; i < methodCount; i++) {
            int shard = ShardPartitioner.shardOf("com.example.Balanced" + i, "m" + i, totalShards);
            counts[shard]++;
        }
        int expectedPerShard = methodCount / totalShards;
        for (int shard = 1; shard <= totalShards; shard++) {
            double deviation = Math.abs(counts[shard] - expectedPerShard) / (double) expectedPerShard;
            Assert.assertTrue(deviation < 0.15,
                    "Shard " + shard + " has " + counts[shard] + " methods, expected ~" + expectedPerShard
                            + " (deviation " + deviation + " too high for a stable hash)");
        }
    }
}
