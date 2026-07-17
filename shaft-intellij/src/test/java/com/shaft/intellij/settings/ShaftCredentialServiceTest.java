package com.shaft.intellij.settings;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;

/**
 * Exercises {@link ShaftCredentialService#dispatchAsync(Runnable)}'s threading behavior directly
 * against the package-private constructor, so it runs without a live IntelliJ {@code
 * Application}/{@code PasswordSafe} (issue #3623, same headless-Gradle-JVM gotcha as prior
 * Phase-1 tickets -- see {@code ShaftPluginExecutorTest}, {@code NoUnboundedCommonPoolUsageTest}).
 */
class ShaftCredentialServiceTest {

    @Test
    @Timeout(10)
    void dispatchAsyncRunsBackgroundWorkOffTheCallingThreadAndCompletesOnADifferentEdtStandInThread() throws Exception {
        ExecutorService backgroundExecutor = Executors.newSingleThreadExecutor(
                runnable -> namedDaemonThread(runnable, "shaft-credential-test-background"));
        ExecutorService edtExecutor = Executors.newSingleThreadExecutor(
                runnable -> namedDaemonThread(runnable, "shaft-credential-test-edt-standin"));
        try {
            ShaftCredentialService service = new ShaftCredentialService(backgroundExecutor, edtExecutor);
            AtomicReference<Thread> backgroundThread = new AtomicReference<>();
            AtomicReference<Thread> completionThread = new AtomicReference<>();

            CompletableFuture<Void> future = service.dispatchAsync(() -> backgroundThread.set(Thread.currentThread()))
                    .thenRun(() -> completionThread.set(Thread.currentThread()));
            future.get(5, TimeUnit.SECONDS);

            Thread callingThread = Thread.currentThread();
            assertNotNull(backgroundThread.get(), "background work must have run");
            assertNotNull(completionThread.get(), "completion callback must have run");
            assertNotSame(callingThread, backgroundThread.get(),
                    "background work must not run inline on the calling/test thread");
            assertNotEquals("shaft-credential-test-background", completionThread.get().getName(),
                    "completion must not still be on the background thread -- the EDT hop must have "
                            + "actually left it");
            assertNotSame(backgroundThread.get(), completionThread.get(),
                    "the background thread and the edt-stand-in thread must be two distinct threads, "
                            + "proving dispatchAsync's completion hop left the background thread rather "
                            + "than continuing on it inline");
        } finally {
            backgroundExecutor.shutdownNow();
            edtExecutor.shutdownNow();
        }
    }

    private static Thread namedDaemonThread(Runnable runnable, String name) {
        Thread thread = new Thread(runnable, name);
        thread.setDaemon(true);
        return thread;
    }
}
