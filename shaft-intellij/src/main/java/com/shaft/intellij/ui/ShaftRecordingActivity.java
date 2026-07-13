package com.shaft.intellij.ui;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

/**
 * Process-wide recording-activity signal shared by every plugin surface that starts, stops, or
 * observes a SHAFT recording (Assistant flows, Guided workflow poller). The readiness strip
 * (issue #3500 A4/O4) listens here so "a recording is running" is answered in one place without
 * each surface polling capture_status on its own.
 *
 * <p>Static by design: the recorder itself is process-wide (one managed browser per IDE), so a
 * per-project signal would be dishonest about ownership.
 */
public final class ShaftRecordingActivity {
    private static final AtomicBoolean ACTIVE = new AtomicBoolean();
    private static final List<Consumer<Boolean>> LISTENERS = new CopyOnWriteArrayList<>();

    private ShaftRecordingActivity() {
    }

    /**
     * Publishes the current recording-activity state; listeners fire only on changes.
     *
     * @param active whether a recording session is currently active
     */
    public static void publish(boolean active) {
        if (ACTIVE.getAndSet(active) != active) {
            LISTENERS.forEach(listener -> listener.accept(active));
        }
    }

    /**
     * Returns whether a recording session is currently active.
     *
     * @return last published activity state
     */
    public static boolean active() {
        return ACTIVE.get();
    }

    /**
     * Registers a state-change listener (fired on the publisher's thread).
     *
     * @param listener change consumer
     */
    public static void listen(Consumer<Boolean> listener) {
        LISTENERS.add(listener);
    }

    /**
     * Removes a previously registered listener.
     *
     * @param listener change consumer registered via {@link #listen}
     */
    public static void unlisten(Consumer<Boolean> listener) {
        LISTENERS.remove(listener);
    }
}
