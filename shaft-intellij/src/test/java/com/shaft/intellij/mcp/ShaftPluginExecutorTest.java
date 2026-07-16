package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises {@link ShaftPluginExecutor}'s pool shape directly against the package-private
 * constructor and static pool factory, so it runs without a live IntelliJ {@code Application}
 * (unavailable in this headless Gradle test JVM — same gotcha as prior Phase-1 tickets).
 */
class ShaftPluginExecutorTest {

    @Test
    @Timeout(10)
    void boundedPoolQueuesExcessTasksInsteadOfRunningAllOfThemConcurrently() throws Exception {
        ThreadPoolExecutor pool = (ThreadPoolExecutor) ShaftPluginExecutor.newBoundedExecutor();
        ShaftPluginExecutor instance = new ShaftPluginExecutor(pool);
        try {
            int poolSize = pool.getCorePoolSize();
            int taskCount = poolSize + 3;
            CountDownLatch startedLatch = new CountDownLatch(poolSize);
            CountDownLatch releaseLatch = new CountDownLatch(1);
            CountDownLatch doneLatch = new CountDownLatch(taskCount);
            AtomicInteger runningNow = new AtomicInteger();
            AtomicInteger maxConcurrent = new AtomicInteger();

            for (int i = 0; i < taskCount; i++) {
                instance.executor().execute(() -> {
                    int running = runningNow.incrementAndGet();
                    maxConcurrent.updateAndGet(current -> Math.max(current, running));
                    startedLatch.countDown();
                    try {
                        releaseLatch.await(10, TimeUnit.SECONDS);
                    } catch (InterruptedException interrupted) {
                        Thread.currentThread().interrupt();
                    } finally {
                        runningNow.decrementAndGet();
                        doneLatch.countDown();
                    }
                });
            }

            assertTrue(startedLatch.await(5, TimeUnit.SECONDS),
                    "expected exactly poolSize (" + poolSize + ") tasks to start running concurrently");
            // Give the pool a moment to (incorrectly) start more than poolSize tasks, if it were unbounded.
            Thread.sleep(200);
            assertEquals(poolSize, maxConcurrent.get(),
                    "bounded pool must never run more than its core size concurrently");

            releaseLatch.countDown();
            assertTrue(doneLatch.await(5, TimeUnit.SECONDS),
                    "queued tasks must eventually all complete once running tasks release (no deadlock)");
        } finally {
            instance.dispose();
        }
    }

    @Test
    @Timeout(10)
    void workerThreadsAreDaemonAndNamedPerConvention() throws Exception {
        ExecutorService pool = ShaftPluginExecutor.newBoundedExecutor();
        ShaftPluginExecutor instance = new ShaftPluginExecutor(pool);
        try {
            BlockingQueue<Thread> captured = new ArrayBlockingQueue<>(1);
            instance.executor().execute(() -> captured.offer(Thread.currentThread()));
            Thread worker = captured.poll(5, TimeUnit.SECONDS);

            assertNotNull(worker, "expected a task to run on a pool worker thread");
            assertTrue(worker.isDaemon(), "pool worker threads must be daemon threads");
            assertTrue(worker.getName().matches("shaft-mcp-worker-\\d+"),
                    "worker thread name must match shaft-mcp-worker-N, was: " + worker.getName());
        } finally {
            instance.dispose();
        }
    }

    @Test
    @Timeout(10)
    void disposeShutsDownTheExecutorPromptly() throws Exception {
        ExecutorService pool = ShaftPluginExecutor.newBoundedExecutor();
        ShaftPluginExecutor instance = new ShaftPluginExecutor(pool);

        instance.dispose();

        assertTrue(pool.awaitTermination(5, TimeUnit.SECONDS), "executor must shut down promptly on dispose");
        assertTrue(pool.isShutdown());
    }
}
