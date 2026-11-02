package com.shaft.tools.internal.support;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Daemon platform threads for work that must not run on virtual threads (issue #6871): native calls
 * (OCR, OpenCV loading), long CPU-bound work (PDF rendering, crypto setup), and latency-critical handlers.
 * Such work holds a carrier thread for its whole duration, and a few of them starve every other virtual
 * thread in the JVM, as the CDP handlers did in #6775.
 */
public final class PlatformThreads {
    private PlatformThreads() {
    }

    /**
     * Returns a factory of named daemon platform threads.
     *
     * @param prefix the thread-name prefix; threads are named {@code prefix-N}
     * @return a new thread factory
     */
    public static ThreadFactory daemonFactory(String prefix) {
        AtomicInteger counter = new AtomicInteger();
        return task -> {
            Thread thread = new Thread(task, prefix + "-" + counter.incrementAndGet());
            thread.setDaemon(true);
            return thread;
        };
    }

    /**
     * Starts {@code task} on a new named daemon platform thread.
     *
     * @param name the thread name
     * @param task the work to run
     * @return the started thread
     */
    public static Thread startDaemon(String name, Runnable task) {
        Thread thread = new Thread(task, name);
        thread.setDaemon(true);
        thread.start();
        return thread;
    }
}
