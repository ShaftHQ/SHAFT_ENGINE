package com.shaft.gui.browser.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

/** Issue #6775: CDP handlers must not depend on the shared virtual-thread carrier pool. */
public class CdpWorkersTest {
    @Test
    public void handlersRunOnNamedDaemonPlatformThreads() throws Exception {
        ExecutorService pool = CdpWorkers.newPool("shaft-cdp-test");
        try {
            Thread thread = pool.submit(Thread::currentThread).get(5, TimeUnit.SECONDS);
            Assert.assertFalse(thread.isVirtual());
            Assert.assertTrue(thread.isDaemon());
            Assert.assertTrue(thread.getName().startsWith("shaft-cdp-test-"));
        } finally {
            pool.shutdownNow();
        }
    }

    @Test
    public void handlersStillRunWhenVirtualThreadsHoldTheCarriers() throws Exception {
        int carriers = Runtime.getRuntime().availableProcessors() * 2;
        CountDownLatch release = new CountDownLatch(1);
        Object monitor = new Object();
        ExecutorService pool = CdpWorkers.newPool("shaft-cdp-pin");
        try {
            for (int i = 0; i < carriers; i++) {
                Thread.ofVirtual().start(() -> {
                    synchronized (monitor) { // blocked virtual threads must not delay CDP handlers
                        try {
                            release.await();
                        } catch (InterruptedException ignored) {
                            Thread.currentThread().interrupt();
                        }
                    }
                });
            }
            Assert.assertEquals(pool.submit(() -> "ran").get(5, TimeUnit.SECONDS), "ran");
        } finally {
            release.countDown();
            pool.shutdownNow();
        }
    }
}
