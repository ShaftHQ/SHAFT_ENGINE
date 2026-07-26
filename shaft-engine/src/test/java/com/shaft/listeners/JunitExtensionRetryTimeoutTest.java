package com.shaft.listeners;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.platform.engine.discovery.DiscoverySelectors;
import org.junit.platform.launcher.LauncherDiscoveryRequest;
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder;

import java.time.Duration;
import java.util.concurrent.CountDownLatch;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * {@code JunitExtension.executeRetryRequest} used to call {@code executor.submit(...).get()} with
 * no timeout (#4074, the same shape as #4066/PR #4073's {@code CaptureManager.invoke()}): a retried
 * test that wedges blocked the retry executor forever, and the {@code executor.shutdownNow()}
 * cleanup sitting in the {@code finally} block below it never ran because the blocking {@code get()}
 * never returned. Each test carries its own hard wall-clock bound (assertTimeoutPreemptively) so a
 * regression in the fix -- the exact failure mode under repair -- cannot hang this suite.
 */
class JunitExtensionRetryTimeoutTest {
    @Test
    void retriedTestThatWedgesTimesOutWithANamedDiagnosableErrorInsteadOfHangingForever() {
        CountDownLatch releaseBlockedTest = new CountDownLatch(1);
        BlockingRetryFixture.releaseLatch = releaseBlockedTest;

        try {
            IllegalStateException failure = assertTimeoutPreemptively(Duration.ofSeconds(5), () ->
                    assertThrows(IllegalStateException.class, () -> JunitExtension.executeRetryRequest(
                            requestFor(BlockingRetryFixture.class), "wedgedFixture retry attempt 1/1",
                            Duration.ofMillis(200))));

            assertTrue(failure.getMessage().contains("wedgedFixture retry attempt 1/1"), failure.getMessage());
            assertTrue(failure.getMessage().toLowerCase(java.util.Locale.ROOT).contains("timed out"),
                    failure.getMessage());
        } finally {
            // Let the blocked worker thread unwind so it doesn't leak a permanently blocked
            // thread past this test.
            releaseBlockedTest.countDown();
        }
    }

    @Test
    void aLegitimatelySlowRetriedTestStillSucceedsUnderTheBound() {
        var summary = assertTimeoutPreemptively(Duration.ofSeconds(5), () -> JunitExtension.executeRetryRequest(
                requestFor(SlowButSuccessfulRetryFixture.class), "slowFixture retry attempt 1/1",
                Duration.ofSeconds(2)));

        assertEquals(0, summary.getFailures().size(), () -> summary.getFailures().toString());
        assertEquals(1, summary.getTestsSucceededCount());
    }

    private static LauncherDiscoveryRequest requestFor(Class<?> fixtureClass) {
        return LauncherDiscoveryRequestBuilder.request()
                .selectors(DiscoverySelectors.selectClass(fixtureClass))
                .configurationParameter("junit.jupiter.extensions.autodetection.enabled", "true")
                .configurationParameter("junit.jupiter.execution.parallel.enabled", "false")
                .build();
    }
}

@ExtendWith(JunitExtension.class)
class BlockingRetryFixture {
    static volatile CountDownLatch releaseLatch = new CountDownLatch(0);

    @Test
    void wedges() throws InterruptedException {
        releaseLatch.await();
    }
}

@ExtendWith(JunitExtension.class)
class SlowButSuccessfulRetryFixture {
    @Test
    void succeedsSlowly() throws InterruptedException {
        // Well under the 2s bound configured above, but not instantaneous: proves a legitimately
        // slow (bounded) retried test is not spuriously killed by the timeout.
        Thread.sleep(300);
    }
}
