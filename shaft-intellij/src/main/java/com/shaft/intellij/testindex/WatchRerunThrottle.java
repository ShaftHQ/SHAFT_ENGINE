package com.shaft.intellij.testindex;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Pure, clock-free rerun throttle for SHAFT watch mode.
 * <p>
 * Bounded scheduling exists because unbounded reruns on rapid file saves are a known repo risk on
 * Windows: each rerun forks a fresh Maven/Surefire JVM, and a save-triggered loop that isn't
 * capped can pile up forked-JVM starts faster than they finish. This throttle caps watch-mode
 * reruns to {@value #MAX_RERUNS_PER_WINDOW} within any rolling {@value #WINDOW_MILLIS}ms window.
 */
public final class WatchRerunThrottle {
    static final int MAX_RERUNS_PER_WINDOW = 6;
    static final long WINDOW_MILLIS = 5 * 60_000L;

    private final Deque<Long> rerunTimestamps = new ArrayDeque<>();
    private boolean throttledNotificationPending = false;

    /**
     * Attempts to acquire permission for a rerun at {@code nowMillis}. Records the timestamp and
     * returns {@code true} when under the limit; returns {@code false} without recording anything
     * when the rolling window already holds {@value #MAX_RERUNS_PER_WINDOW} reruns.
     *
     * @param nowMillis current time in epoch millis (caller-supplied so this stays clock-free)
     * @return {@code true} when the rerun is allowed
     */
    public synchronized boolean tryAcquire(long nowMillis) {
        evictExpired(nowMillis);
        if (rerunTimestamps.size() >= MAX_RERUNS_PER_WINDOW) {
            return false;
        }
        rerunTimestamps.addLast(nowMillis);
        // Back under the limit: the next time we get throttled deserves a fresh notification.
        throttledNotificationPending = true;
        return true;
    }

    /**
     * Returns whether the "rerun limit reached" notification should be shown right now. Only the
     * first call after entering a throttled state returns {@code true}; subsequent calls while
     * still throttled return {@code false} so watch mode does not spam a notification per keystroke.
     * The next successful {@link #tryAcquire} call re-arms this. Callers should only consult this
     * after a {@link #tryAcquire} call has just returned {@code false}.
     *
     * @param nowMillis current time in epoch millis
     * @return {@code true} at most once per throttled window
     */
    public synchronized boolean shouldNotifyThrottled(long nowMillis) {
        evictExpired(nowMillis);
        if (rerunTimestamps.size() < MAX_RERUNS_PER_WINDOW || !throttledNotificationPending) {
            return false;
        }
        throttledNotificationPending = false;
        return true;
    }

    private void evictExpired(long nowMillis) {
        while (!rerunTimestamps.isEmpty() && nowMillis - rerunTimestamps.peekFirst() >= WINDOW_MILLIS) {
            rerunTimestamps.pollFirst();
        }
    }
}
