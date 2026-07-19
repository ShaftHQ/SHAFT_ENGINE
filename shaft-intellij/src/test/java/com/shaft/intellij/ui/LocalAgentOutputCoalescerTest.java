package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Deterministic coverage for {@link LocalAgentOutputCoalescer}, added for issue #3751 part 2 (HIGH
 * finding 2): {@code ShaftAssistantPanel} used to schedule one {@code invokeLater} per streamed
 * output line ({@code AssistantLocalAgentRunner}'s {@code readAsync} calls its consumer once per
 * line, from the stdout AND stderr reader threads concurrently), flooding the EDT queue on a chatty
 * CLI stream. This coalescer lets any number of threads {@link LocalAgentOutputCoalescer#enqueue}
 * lines cheaply (no scheduling) while a flush is already pending, and hands the whole accumulated
 * batch to one consumer call, in order, the next time the (test-supplied, synchronous here)
 * scheduler runs it -- deterministic and EDT/Timer-free so this test never depends on real time.
 */
class LocalAgentOutputCoalescerTest {
    @Test
    void enqueuedLinesAreDeliveredAsOneBatchInOrder() {
        List<List<String>> batches = new ArrayList<>();
        List<Runnable> scheduled = new ArrayList<>();
        LocalAgentOutputCoalescer coalescer = new LocalAgentOutputCoalescer(batches::add, scheduled::add);

        coalescer.enqueue("line 1");
        coalescer.enqueue("line 2");
        coalescer.enqueue("line 3");

        assertEquals(1, scheduled.size(), "Three enqueues while idle must schedule exactly one flush");
        scheduled.get(0).run();

        assertAll(
                () -> assertEquals(1, batches.size(), "All three lines must be delivered in a single batch"),
                () -> assertEquals(List.of("line 1", "line 2", "line 3"), batches.get(0),
                        "Lines must be delivered in enqueue order"));
    }

    @Test
    void onlyTheFirstEnqueueAfterAnIdleFlushSchedulesANewFlush() {
        List<List<String>> batches = new ArrayList<>();
        List<Runnable> scheduled = new ArrayList<>();
        LocalAgentOutputCoalescer coalescer = new LocalAgentOutputCoalescer(batches::add, scheduled::add);

        coalescer.enqueue("a");
        coalescer.enqueue("b");
        coalescer.enqueue("c");
        assertEquals(1, scheduled.size(), "Enqueues while a flush is already scheduled must not schedule another");

        scheduled.get(0).run();
        coalescer.enqueue("d");

        assertEquals(2, scheduled.size(), "The next enqueue after the flush ran must schedule a fresh flush");
        scheduled.get(1).run();
        assertEquals(List.of(List.of("a", "b", "c"), List.of("d")), batches);
    }

    @Test
    void flushWithNothingQueuedNeverCallsTheBatchConsumer() {
        List<List<String>> batches = new ArrayList<>();
        LocalAgentOutputCoalescer coalescer = new LocalAgentOutputCoalescer(batches::add, task -> { });

        coalescer.flush();

        assertTrue(batches.isEmpty(), "An idle flush (nothing queued) must never invoke the batch consumer");
    }

    @Test
    void forceFlushDrainsQueuedLinesSynchronouslyWithoutWaitingForTheScheduler() {
        List<List<String>> batches = new ArrayList<>();
        List<Runnable> scheduled = new ArrayList<>();
        LocalAgentOutputCoalescer coalescer = new LocalAgentOutputCoalescer(batches::add, scheduled::add);

        coalescer.enqueue("final line");
        // Simulates a run completing/cancelling before the throttled flush fired: the terminal path
        // must force a drain itself rather than lose (or indefinitely delay) the last queued line.
        coalescer.flush();

        assertEquals(List.of(List.of("final line")), batches,
                "A forced flush must drain whatever is queued even if the scheduled flush never ran");
    }

    @Test
    void enqueueAfterAForcedFlushSchedulesAFreshFlush() {
        List<List<String>> batches = new ArrayList<>();
        List<Runnable> scheduled = new ArrayList<>();
        LocalAgentOutputCoalescer coalescer = new LocalAgentOutputCoalescer(batches::add, scheduled::add);

        coalescer.enqueue("x");
        coalescer.flush();
        coalescer.enqueue("y");

        assertEquals(2, scheduled.size(), "A forced flush must reset scheduling state exactly like a normal one");
    }

    /**
     * {@code readAsync} reads stdout and stderr on two separate pool threads, both funneling into the
     * same consumer -- the coalescer's {@code enqueue} must be safe under concurrent callers and must
     * never drop a line.
     */
    @Test
    void enqueueIsThreadSafeAcrossConcurrentProducers() throws InterruptedException {
        AtomicInteger totalDelivered = new AtomicInteger();
        List<Runnable> scheduled = new ArrayList<>();
        LocalAgentOutputCoalescer coalescer = new LocalAgentOutputCoalescer(
                batch -> totalDelivered.addAndGet(batch.size()),
                scheduled::add);

        int perThread = 500;
        ExecutorService pool = Executors.newFixedThreadPool(2);
        CountDownLatch ready = new CountDownLatch(2);
        CountDownLatch go = new CountDownLatch(1);
        try {
            for (int t = 0; t < 2; t++) {
                pool.submit(() -> {
                    ready.countDown();
                    await(go);
                    for (int i = 0; i < perThread; i++) {
                        coalescer.enqueue("line " + i);
                    }
                });
            }
            ready.await();
            go.countDown();
            pool.shutdown();
            assertTrue(pool.awaitTermination(10, TimeUnit.SECONDS), "Producer threads must finish promptly");
        } finally {
            pool.shutdownNow();
        }

        // Drain everything -- possibly several scheduled flushes plus a final forced one, matching how
        // production forces a last drain at run completion.
        List<Runnable> toRun = new ArrayList<>(scheduled);
        toRun.forEach(Runnable::run);
        coalescer.flush();

        assertEquals(perThread * 2, totalDelivered.get(),
                "Every line enqueued by both producer threads must be delivered exactly once, none dropped");
    }

    private static void await(CountDownLatch latch) {
        try {
            latch.await();
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
        }
    }
}
