package com.shaft.capture.collector;

import java.util.function.Consumer;

/**
 * Collects browser signals without persisting unclassified values.
 */
public interface BrowserEventCollector extends AutoCloseable {
    /**
     * Starts event collection.
     *
     * @param signalConsumer signal destination
     * @param warningConsumer safe warning destination
     */
    void start(Consumer<BrowserSignal> signalConsumer, Consumer<String> warningConsumer);

    /**
     * Stops collection and releases protocol listeners.
     */
    @Override
    void close();
}
