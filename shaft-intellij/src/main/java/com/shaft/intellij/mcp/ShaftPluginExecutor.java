package com.shaft.intellij.mcp;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;

import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Application-level bounded executor for MCP/local-agent blocking work (issue #3622).
 *
 * <p>The invocation, connection-probe, and local-agent call sites in this package spawn MCP
 * stdio processes and block waiting on their responses. Handing that work to {@code
 * CompletableFuture.supplyAsync} with no explicit {@link Executor} argument silently
 * defaults to {@link java.util.concurrent.ForkJoinPool#commonPool()} — a JVM-wide pool shared
 * with the IDE itself and every other plugin. Blocking I/O run there can starve the common pool
 * for the whole IDE, and that shared pool cannot be bounded, thread-named for diagnostics, or
 * shut down when this plugin unloads. This service gives that work its own small, bounded,
 * named, disposable pool instead.</p>
 */
public final class ShaftPluginExecutor implements Disposable {
    /**
     * A handful of concurrent MCP/local-agent operations (tool calls, connection probes, local
     * CLI runs) is the realistic ceiling for a single IDE session, so the pool is sized off
     * available processors but floored and capped to a small fixed range — no point scaling with
     * large core counts for a workload that is bound by external process I/O, not CPU.
     */
    private static final int MIN_POOL_SIZE = 4;
    private static final int MAX_POOL_SIZE = 8;

    private final ExecutorService executor;

    public ShaftPluginExecutor() {
        this(newBoundedExecutor());
    }

    /**
     * Test seam: accepts a pre-built executor so unit tests can exercise pool behavior (including
     * the pool built by {@link #newBoundedExecutor()}) without a live IntelliJ {@code
     * Application}. Not public API.
     *
     * @param executor the executor this instance wraps and disposes
     */
    ShaftPluginExecutor(ExecutorService executor) {
        this.executor = executor;
    }

    /**
     * Returns the application-level plugin executor.
     *
     * <p>Falls back to a lazily built, standalone instance of the same shape when no IntelliJ
     * {@code Application} is running, mirroring the {@code ApplicationManager.getApplication() ==
     * null} guard already used throughout this plugin's UI code (e.g. {@code
     * GuidedWorkflowPanel.resolveSettings()}, {@code ShaftAssistantPanel.storedCloudKey()}) for
     * the same headless-test gotcha: call sites that used to reach {@code CompletableFuture}'s
     * argless {@code supplyAsync} overload never touched {@code ApplicationManager} at all, so
     * migrating them to pass an executor here must not newly require a live platform in tests
     * that construct UI panels headlessly.
     *
     * @return plugin executor
     */
    public static ShaftPluginExecutor getInstance() {
        if (ApplicationManager.getApplication() == null) {
            return HeadlessHolder.INSTANCE;
        }
        return ApplicationManager.getApplication().getService(ShaftPluginExecutor.class);
    }

    /**
     * Lazy holder for the headless-fallback instance from {@link #getInstance()}: only
     * initialized (and only ever pays for a thread pool) the first time {@link #getInstance()} is
     * called with no live {@code Application}, i.e. in headless tests.
     */
    private static final class HeadlessHolder {
        private static final ShaftPluginExecutor INSTANCE = new ShaftPluginExecutor();
    }

    /**
     * Returns the bounded executor MCP/local-agent blocking work should run on, e.g. as the
     * second argument to {@code CompletableFuture.supplyAsync(supplier, executor())}.
     *
     * @return bounded executor
     */
    public Executor executor() {
        return executor;
    }

    /**
     * Shuts the pool down immediately (interrupting in-flight tasks) so no {@code
     * shaft-mcp-worker} thread survives plugin unload. {@code shutdownNow()} rather than a
     * graceful {@code shutdown()} + await, because in-flight tasks here are blocked on external
     * MCP/local-agent process I/O — waiting for them to finish naturally on unload could hang
     * indefinitely; the underlying {@link ShaftMcpStdioClient}/process handling already tolerates
     * interruption via its own cancellation paths.
     */
    @Override
    public void dispose() {
        executor.shutdownNow();
    }

    /**
     * Builds the bounded pool: fixed core/max size (no unbounded growth), an unbounded work queue
     * (so bursts past the core size queue instead of being rejected — MCP/local-agent calls are
     * user-triggered and infrequent enough that queuing briefly is preferable to failing them),
     * and a {@link ThreadFactory} producing named daemon threads so a leaked reference never keeps
     * the JVM alive and threads are identifiable in diagnostics.
     *
     * @return newly built bounded executor
     */
    static ExecutorService newBoundedExecutor() {
        int size = Math.max(MIN_POOL_SIZE, Math.min(MAX_POOL_SIZE, Runtime.getRuntime().availableProcessors()));
        return new ThreadPoolExecutor(
                size, size, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>(), new WorkerThreadFactory());
    }

    private static final class WorkerThreadFactory implements ThreadFactory {
        private final AtomicInteger counter = new AtomicInteger(1);

        @Override
        public Thread newThread(Runnable runnable) {
            Thread thread = new Thread(runnable, "shaft-mcp-worker-" + counter.getAndIncrement());
            thread.setDaemon(true);
            return thread;
        }
    }
}
