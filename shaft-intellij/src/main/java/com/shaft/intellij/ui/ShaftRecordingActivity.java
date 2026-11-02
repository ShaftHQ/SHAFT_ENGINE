package com.shaft.intellij.ui;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
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
 *
 * <p>Tracked by a set of per-surface session keys rather than a single boolean (issue #3591 item
 * 3): overlapping recordings from different surfaces used to collapse onto one process-wide flag,
 * so stopping one recording could publish {@code false} while another was still running.
 */
public final class ShaftRecordingActivity {
    private static final Object LOCK = new Object();
    private static final Set<String> ACTIVE_KEYS = new HashSet<>(); // guarded by LOCK
    private static final AtomicBoolean LAST = new AtomicBoolean();
    private static final List<Consumer<Boolean>> LISTENERS = new CopyOnWriteArrayList<>();

    private ShaftRecordingActivity() {
    }

    /**
     * Marks {@code sessionKey} as an active recording session; listeners fire only if this is the
     * first active session overall.
     *
     * @param sessionKey stable per-instance key identifying the recording surface/session
     */
    public static void started(String sessionKey) {
        update(() -> ACTIVE_KEYS.add(sessionKey));
    }

    /**
     * Marks {@code sessionKey} as no longer recording; listeners fire only if this was the last
     * active session overall.
     *
     * @param sessionKey stable per-instance key previously passed to {@link #started}
     */
    public static void stopped(String sessionKey) {
        update(() -> ACTIVE_KEYS.remove(sessionKey));
    }

    private static void update(Runnable mutation) {
        boolean now;
        synchronized (LOCK) {
            mutation.run();
            now = !ACTIVE_KEYS.isEmpty();
        }
        if (LAST.getAndSet(now) != now) {
            LISTENERS.forEach(listener -> listener.accept(now));
        }
    }

    /**
     * Returns whether any recording session is currently active.
     *
     * @return last published activity state
     */
    public static boolean active() {
        return LAST.get();
    }

    /**
     * Registers a state-change listener (fired on the publisher's thread).
     *
     * <p><b>Caution:</b> {@code LISTENERS} is a static, process-wide list, so a registration made
     * here outlives any single component unless something calls {@link #unlisten} for it. Forgetting
     * to do so is exactly how issue #3620 leaked a fresh {@code ShaftReadinessSummary} listener on
     * every tool-window view rebuild. Prefer {@link #listen(Consumer, Disposable)}, which detaches
     * automatically, unless the caller has some other reliable, always-run teardown path already.
     *
     * @param listener change consumer
     */
    public static void listen(Consumer<Boolean> listener) {
        LISTENERS.add(listener);
    }

    /**
     * Registers a state-change listener that detaches itself automatically when {@code
     * parentDisposable} is disposed, so callers cannot forget the matching {@link #unlisten} call
     * (issue #3620).
     *
     * @param listener change consumer
     * @param parentDisposable disposable whose disposal unregisters {@code listener}
     */
    public static void listen(Consumer<Boolean> listener, Disposable parentDisposable) {
        LISTENERS.add(listener);
        Disposer.register(parentDisposable, () -> unlisten(listener));
    }

    /**
     * Removes a previously registered listener.
     *
     * @param listener change consumer registered via {@link #listen}
     */
    public static void unlisten(Consumer<Boolean> listener) {
        LISTENERS.remove(listener);
    }

    /**
     * Test-only count of currently registered listeners. Package visible so only tests in this
     * package can reach it; used to assert that view rebuilds dispose their old listener instead of
     * leaking a new one onto this static, process-wide list (issue #3620).
     *
     * @return number of listeners currently registered via {@link #listen}
     */
    static int listenerCount() {
        return LISTENERS.size();
    }

    /**
     * Test-only reset of all static state (active keys, last-published flag, listeners). Package
     * visible so only tests in this package can reach it.
     */
    static void resetForTests() {
        synchronized (LOCK) {
            ACTIVE_KEYS.clear();
        }
        LAST.set(false);
        LISTENERS.clear();
    }
}
