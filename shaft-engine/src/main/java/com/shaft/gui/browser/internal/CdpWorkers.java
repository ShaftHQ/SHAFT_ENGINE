package com.shaft.gui.browser.internal;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Worker pools for CDP network handlers (issue #6775). A paused request only resumes when its handler runs, so the
 * handlers use daemon platform threads: virtual threads share a small carrier pool that pinned virtual threads
 * elsewhere in a long suite can exhaust, which left navigations hanging until the renderer timed out.
 */
final class CdpWorkers {
    private CdpWorkers() {
    }

    /**
     * Creates an unbounded cached pool of named daemon platform threads.
     *
     * @param name the thread-name prefix
     * @return a new executor; the caller shuts it down
     */
    static ExecutorService newPool(String name) {
        AtomicInteger counter = new AtomicInteger();
        return Executors.newCachedThreadPool(task -> {
            Thread thread = new Thread(task, name + "-" + counter.incrementAndGet());
            thread.setDaemon(true);
            return thread;
        });
    }
}
