package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;

import java.util.concurrent.atomic.AtomicLong;

/**
 * Process-wide ledger of persisted SHAFT trace bytes. Caps {@code target/shaft-traces}
 * publication before maximum resource capture can fill the disk.
 */
final class TraceSessionBudget {
    private static final long BYTES_PER_MEBIBYTE = 1024L * 1024L;
    private static final AtomicLong PUBLISHED_BYTES = new AtomicLong();

    private TraceSessionBudget() {
        throw new IllegalStateException("Utility class");
    }

    static void resetForTesting() {
        PUBLISHED_BYTES.set(0);
    }

    static long configuredMaxBytes() {
        int configuredMiB = SHAFT.Properties.reporting.traceMaxSessionMb();
        return Math.multiplyExact((long) Math.max(1, configuredMiB), BYTES_PER_MEBIBYTE);
    }

    static long remaining() {
        return Math.max(0L, configuredMaxBytes() - PUBLISHED_BYTES.get());
    }

    static boolean canPublish(long bytes) {
        return bytes >= 0 && remaining() >= bytes;
    }

    static boolean tryReserve(long bytes) {
        if (bytes < 0) {
            return false;
        }
        while (true) {
            long current = PUBLISHED_BYTES.get();
            long maximum = configuredMaxBytes();
            if (current > maximum - bytes) {
                return false;
            }
            if (PUBLISHED_BYTES.compareAndSet(current, current + bytes)) {
                return true;
            }
        }
    }

    static void recordPublished(long bytes) {
        if (bytes > 0) {
            PUBLISHED_BYTES.addAndGet(bytes);
        }
    }

    static String omissionReason() {
        return "Omitted because the JVM session exceeded shaft.trace.maxSessionMb="
                + Math.max(1, SHAFT.Properties.reporting.traceMaxSessionMb());
    }
}
