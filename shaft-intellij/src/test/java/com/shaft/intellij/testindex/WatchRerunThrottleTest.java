package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class WatchRerunThrottleTest {
    private static final long WINDOW_MILLIS = 5 * 60_000L;

    @Test
    void allowsRerunsUnderTheLimit() {
        WatchRerunThrottle throttle = new WatchRerunThrottle();
        long now = 0L;
        for (int i = 0; i < 6; i++) {
            assertTrue(throttle.tryAcquire(now + i), "rerun " + i + " should be allowed");
        }
    }

    @Test
    void deniesTheSeventhRerunWithinTheWindow() {
        WatchRerunThrottle throttle = new WatchRerunThrottle();
        long now = 0L;
        for (int i = 0; i < 6; i++) {
            assertTrue(throttle.tryAcquire(now + i));
        }
        assertFalse(throttle.tryAcquire(now + 6), "the 7th rerun within the window should be denied");
    }

    @Test
    void allowsRerunsAgainAfterTheWindowExpires() {
        WatchRerunThrottle throttle = new WatchRerunThrottle();
        for (int i = 0; i < 6; i++) {
            assertTrue(throttle.tryAcquire(i));
        }
        assertFalse(throttle.tryAcquire(6));

        long afterWindow = WINDOW_MILLIS + 1;
        assertTrue(throttle.tryAcquire(afterWindow), "a rerun after the window expires should be allowed");
    }

    @Test
    void evictsOnlyExpiredTimestampsNotTheWholeWindow() {
        WatchRerunThrottle throttle = new WatchRerunThrottle();
        // An old batch of 3, then (after a large gap) a fresh batch of 3 -- 6 total, window full.
        assertTrue(throttle.tryAcquire(0));
        assertTrue(throttle.tryAcquire(1));
        assertTrue(throttle.tryAcquire(2));
        long freshBatchStart = WINDOW_MILLIS / 2;
        assertTrue(throttle.tryAcquire(freshBatchStart));
        assertTrue(throttle.tryAcquire(freshBatchStart + 1));
        assertTrue(throttle.tryAcquire(freshBatchStart + 2));
        assertFalse(throttle.tryAcquire(freshBatchStart + 3), "the window is already full");

        // Once the old batch (0, 1, 2) has aged out but the fresh batch (with a huge remaining
        // shelf life) has not, exactly 3 slots should free up, not the whole window.
        long now = WINDOW_MILLIS + 3;
        assertTrue(throttle.tryAcquire(now));
        assertTrue(throttle.tryAcquire(now + 1));
        assertTrue(throttle.tryAcquire(now + 2));
        assertFalse(throttle.tryAcquire(now + 3), "only 3 slots should have freed up, not the whole window");
    }

    @Test
    void notifiesOnceWhenThrottledThenStaysSilentUntilRearmed() {
        WatchRerunThrottle throttle = new WatchRerunThrottle();
        for (int i = 0; i < 6; i++) {
            assertTrue(throttle.tryAcquire(i));
        }
        long throttledAt = 6L;
        assertFalse(throttle.tryAcquire(throttledAt));
        assertTrue(throttle.shouldNotifyThrottled(throttledAt), "first throttle hit should notify");
        assertFalse(throttle.tryAcquire(throttledAt + 1));
        assertFalse(throttle.shouldNotifyThrottled(throttledAt + 1), "repeated throttle hits should stay silent");
    }

    @Test
    void rearmsNotificationAfterTheWindowOpensBackUp() {
        WatchRerunThrottle throttle = new WatchRerunThrottle();
        for (int i = 0; i < 6; i++) {
            assertTrue(throttle.tryAcquire(i));
        }
        assertFalse(throttle.tryAcquire(6));
        assertTrue(throttle.shouldNotifyThrottled(6));

        long afterWindow = WINDOW_MILLIS + 10;
        assertTrue(throttle.tryAcquire(afterWindow), "window has expired, so this rerun is allowed");

        for (int i = 0; i < 5; i++) {
            assertTrue(throttle.tryAcquire(afterWindow + 1 + i));
        }
        assertFalse(throttle.tryAcquire(afterWindow + 6));
        assertTrue(throttle.shouldNotifyThrottled(afterWindow + 6),
                "a fresh throttled window should notify again");
    }

    @Test
    void doesNotNotifyWhileStillUnderTheLimit() {
        WatchRerunThrottle throttle = new WatchRerunThrottle();
        assertTrue(throttle.tryAcquire(0));
        assertFalse(throttle.shouldNotifyThrottled(0));
    }
}
