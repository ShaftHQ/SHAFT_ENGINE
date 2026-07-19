package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #3768: a soft {@link ShaftMcpInvocation#cancel()} rendered as "Failed" instead of
 * "Cancelled" because {@code cancel()} never called {@code future.cancel(...)} -- it only ran
 * {@code cancelAction} and left the future to complete however the async worker chose to. A
 * real local-agent worker (mirrored below) resolves cancellation by <em>throwing</em> a
 * {@link CancellationException} from inside {@link CompletableFuture#supplyAsync}, which the JDK
 * wraps in a {@code CompletionException}. {@link CompletableFuture#whenComplete} hands its
 * observer that wrapper verbatim -- unlike {@code Future#get()}, it does not unwrap
 * {@code CompletionException} to the bare cause -- so every {@code error instanceof
 * CancellationException} check downstream (e.g. {@code ShaftAssistantPanel#showAgentResult})
 * never fired for a soft cancel.
 *
 * <p>This test proves the bug at the {@link ShaftMcpInvocation} level: it drives a real
 * {@code supplyAsync} worker through {@code cancel()} and inspects exactly what
 * {@code whenComplete} observes, the same hand-off every real caller relies on.
 */
class ShaftMcpInvocationSoftCancelTest {

    @Test
    void softCancelLeavesTheFutureCancelledWithABareCancellationException() throws Exception {
        CountDownLatch workerStarted = new CountDownLatch(1);
        CountDownLatch cancelActionRan = new CountDownLatch(1);
        ExecutorService worker = Executors.newSingleThreadExecutor();
        try {
            // Mirrors AssistantLocalAgentRunner's real worker: it never calls future.cancel() itself,
            // it just throws CancellationException once it notices cancellation was requested --
            // exactly the shape CompletableFuture.supplyAsync wraps in a CompletionException.
            CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(() -> {
                workerStarted.countDown();
                await(cancelActionRan);
                throw new CancellationException("cancelled");
            }, worker);
            ShaftMcpInvocation invocation = new ShaftMcpInvocation(future, cancelActionRan::countDown);
            assertTrue(workerStarted.await(5, TimeUnit.SECONDS),
                    "The worker must be blocked waiting for cancelAction before cancel() runs, or the race is nondeterministic");

            AtomicReference<Throwable> observedError = new AtomicReference<>();
            CountDownLatch completed = new CountDownLatch(1);
            future.whenComplete((result, error) -> {
                observedError.set(error);
                completed.countDown();
            });

            boolean acknowledged = invocation.cancel();

            assertTrue(completed.await(5, TimeUnit.SECONDS), "The future must complete after cancel()");
            assertAll(
                    () -> assertTrue(acknowledged, "cancel() must report the cancellation request as acknowledged"),
                    () -> assertTrue(future.isCancelled(),
                            "future.isCancelled() must be true after a soft cancel -- it was false when cancel() "
                                    + "never called future.cancel(...) and only the worker's thrown "
                                    + "CancellationException (wrapped in a CompletionException) completed it"),
                    () -> assertEquals(CancellationException.class,
                            observedError.get() == null ? null : observedError.get().getClass(),
                            "whenComplete's error must be a BARE CancellationException, not a CompletionException "
                                    + "wrapper, or callers checking `error instanceof CancellationException` (issue "
                                    + "#3768) never see the cancellation and render \"Failed\" instead"));
        } finally {
            worker.shutdownNow();
        }
    }

    private static void await(CountDownLatch latch) {
        try {
            assertTrue(latch.await(5, TimeUnit.SECONDS), "cancelAction must run and unblock the worker");
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
