package com.shaft.capture.collector;

import java.util.List;
import java.util.function.Consumer;

/**
 * Combines protocol and compatibility collectors behind one lifecycle.
 */
public final class CompositeBrowserEventCollector implements BrowserEventCollector {
    private final List<BrowserEventCollector> collectors;

    /**
     * Creates a composite collector.
     *
     * @param collectors collectors to start and close together
     */
    public CompositeBrowserEventCollector(List<BrowserEventCollector> collectors) {
        if (collectors == null || collectors.isEmpty() || collectors.stream().anyMatch(java.util.Objects::isNull)) {
            throw new IllegalArgumentException("At least one browser event collector is required.");
        }
        this.collectors = List.copyOf(collectors);
    }

    @Override
    public void start(Consumer<BrowserSignal> signalConsumer, Consumer<String> warningConsumer) {
        int started = 0;
        try {
            for (BrowserEventCollector collector : collectors) {
                collector.start(signalConsumer, warningConsumer);
                started++;
            }
        } catch (RuntimeException exception) {
            for (int index = started - 1; index >= 0; index--) {
                collectors.get(index).close();
            }
            throw exception;
        }
    }

    @Override
    public void close() {
        for (int index = collectors.size() - 1; index >= 0; index--) {
            try {
                collectors.get(index).close();
            } catch (RuntimeException ignored) {
                // Best-effort cleanup continues for the remaining collectors.
            }
        }
    }
}
