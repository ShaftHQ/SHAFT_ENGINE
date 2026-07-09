package com.shaft.properties.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Duration;
import java.util.concurrent.atomic.AtomicInteger;

public class PropertiesHelperDownloadRetryUnitTest {

    @Test(description = "downloadWithRetry should retry a RuntimeException and succeed once the download recovers")
    public void downloadWithRetryRetriesRuntimeExceptionThenSucceeds() {
        AtomicInteger invocations = new AtomicInteger();
        Runnable attempt = () -> {
            if (invocations.incrementAndGet() < 3) {
                throw new RuntimeException("HTTP 429");
            }
        };

        PropertiesHelper.downloadWithRetry(attempt, 3, Duration.ZERO, Duration.ZERO);

        Assert.assertEquals(invocations.get(), 3);
    }

    @Test(description = "downloadWithRetry should retry an AssertionError, matching FailureReporter's failure type")
    public void downloadWithRetryRetriesAssertionErrorThenSucceeds() {
        AtomicInteger invocations = new AtomicInteger();
        Runnable attempt = () -> {
            if (invocations.incrementAndGet() < 2) {
                throw new AssertionError("HTTP 429");
            }
        };

        PropertiesHelper.downloadWithRetry(attempt, 3, Duration.ZERO, Duration.ZERO);

        Assert.assertEquals(invocations.get(), 2);
    }

    @Test(description = "downloadWithRetry should exhaust attempts and rethrow the original failure")
    public void downloadWithRetryRethrowsAfterExhaustingAttempts() {
        AtomicInteger invocations = new AtomicInteger();
        Runnable attempt = () -> {
            invocations.incrementAndGet();
            throw new RuntimeException("HTTP 429");
        };

        Assert.assertThrows(RuntimeException.class,
                () -> PropertiesHelper.downloadWithRetry(attempt, 3, Duration.ZERO, Duration.ZERO));
        Assert.assertEquals(invocations.get(), 3);
    }
}
