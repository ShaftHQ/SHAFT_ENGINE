package com.shaft.fuzz;

import com.shaft.listeners.internal.ShardPartitioner;

import java.nio.charset.StandardCharsets;

/**
 * Jazzer target (#7012): {@code ShardPartitioner.parse} must never throw on any input.
 */
public final class ShardSpecFuzzer {
    private ShardSpecFuzzer() {
    }

    public static void fuzzerTestOneInput(byte[] data) {
        if (ShardPartitioner.parse(new String(data, StandardCharsets.UTF_8)) == null) {
            throw new IllegalStateException("parse returned null");
        }
    }
}
