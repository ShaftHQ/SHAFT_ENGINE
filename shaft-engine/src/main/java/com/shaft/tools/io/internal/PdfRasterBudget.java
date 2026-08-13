package com.shaft.tools.io.internal;

import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;

/** Shared admission control for concurrent rendered PDF pages. */
final class PdfRasterBudget {
    private static final int UNIT_BYTES = 1024 * 1024;
    private final int capacity;
    private final Semaphore permits;

    PdfRasterBudget(long maximumBytes) {
        capacity = Math.max(1, Math.toIntExact(Math.min(Integer.MAX_VALUE, Math.ceilDiv(maximumBytes, UNIT_BYTES))));
        permits = new Semaphore(capacity, true);
    }

    Lease acquire(long bytes) {
        long required = Math.ceilDiv(bytes, UNIT_BYTES);
        if (required > capacity) {
            throw new IllegalArgumentException("Rendered PDF page requires " + bytes
                    + " bytes, above the configured in-flight raster budget.");
        }
        int requested = Math.max(1, Math.toIntExact(required));
        try {
            permits.acquire(requested);
            return new Lease(requested);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("PDF raster budget acquisition was interrupted.", exception);
        }
    }

    final class Lease implements AutoCloseable {
        private final int acquired;
        private final AtomicBoolean closed = new AtomicBoolean();

        private Lease(int acquired) {
            this.acquired = acquired;
        }

        @Override
        public void close() {
            if (closed.compareAndSet(false, true)) {
                permits.release(acquired);
            }
        }
    }
}
