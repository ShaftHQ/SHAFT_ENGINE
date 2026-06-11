package com.shaft.capture.runtime;

import com.shaft.capture.model.Checkpoint;

import java.time.Duration;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

/**
 * Thread-safe local lifecycle API shared by CLI and MCP capture controls.
 */
public final class CaptureManager implements AutoCloseable {
    private static final Duration HEALTH_INTERVAL = Duration.ofSeconds(1);

    private final ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor(runnable -> {
        Thread thread = new Thread(runnable, "shaft-capture-manager");
        thread.setDaemon(false);
        return thread;
    });
    private final Function<CaptureStartRequest, ManagedCaptureRecorder> recorderFactory;
    private volatile ManagedCaptureRecorder recorder;
    private volatile CaptureStatus lastStatus = CaptureStatus.notRunning();
    private CaptureSingleSessionLock sessionLock;
    private ScheduledFuture<?> healthCheck;

    /**
     * Creates the default managed recorder lifecycle.
     */
    public CaptureManager() {
        this(ManagedCaptureRecorder::new);
    }

    CaptureManager(Function<CaptureStartRequest, ManagedCaptureRecorder> recorderFactory) {
        if (recorderFactory == null) {
            throw new IllegalArgumentException("Capture recorder factory is required.");
        }
        this.recorderFactory = recorderFactory;
    }

    /**
     * Starts one managed browser capture.
     *
     * @param request validated start request
     * @return active recorder status
     */
    public synchronized CaptureStatus start(CaptureStartRequest request) {
        if (isActive(lastStatus.state())) {
            throw new IllegalStateException("A SHAFT Capture session is already active.");
        }
        lastStatus = new CaptureStatus(
                CaptureStatus.State.STARTING,
                "",
                request.browser().name().toLowerCase(),
                "",
                0,
                java.util.List.of(),
                request.outputPath().toString(),
                false,
                ProcessHandle.current().pid(),
                null);
        return invoke(() -> {
            sessionLock = CaptureSingleSessionLock.acquire(request.runtimeDirectory());
            try {
                recorder = recorderFactory.apply(request);
                recorder.start();
                lastStatus = recorder.status();
                healthCheck = executor.scheduleWithFixedDelay(
                        this::verifyBrowserHealth,
                        HEALTH_INTERVAL.toMillis(),
                        HEALTH_INTERVAL.toMillis(),
                        TimeUnit.MILLISECONDS);
                return lastStatus;
            } catch (RuntimeException exception) {
                recorder = null;
                releaseLock();
                throw exception;
            }
        });
    }

    /**
     * Returns the latest safe status without captured values.
     *
     * @return recorder status
     */
    public CaptureStatus status() {
        ManagedCaptureRecorder current = recorder;
        if (current != null && isActive(lastStatus.state())) {
            lastStatus = current.status();
        }
        return lastStatus;
    }

    /**
     * Adds a human-review checkpoint to the active session.
     *
     * @param description checkpoint description
     * @param kind checkpoint kind
     * @return updated status
     */
    public CaptureStatus checkpoint(String description, Checkpoint.CheckpointKind kind) {
        return invoke(() -> {
            requireRecorder().checkpoint(description, kind);
            lastStatus = recorder.status();
            return lastStatus;
        });
    }

    /**
     * Stops the active session.
     *
     * @param discard whether to remove capture artifacts after clean shutdown
     * @return final status
     */
    public synchronized CaptureStatus stop(boolean discard) {
        if (recorder == null) {
            return lastStatus;
        }
        lastStatus = copyWithState(status(), CaptureStatus.State.STOPPING);
        return invoke(() -> {
            cancelHealthCheck();
            lastStatus = recorder.stop(discard);
            recorder = null;
            releaseLock();
            return lastStatus;
        });
    }

    @Override
    public synchronized void close() {
        if (recorder != null) {
            invoke(() -> {
                cancelHealthCheck();
                lastStatus = recorder.interrupt();
                recorder = null;
                releaseLock();
                return lastStatus;
            });
        }
        executor.shutdownNow();
    }

    private void verifyBrowserHealth() {
        ManagedCaptureRecorder current = recorder;
        if (current == null || !isActive(lastStatus.state())) {
            return;
        }
        if (!current.isBrowserAlive()) {
            cancelHealthCheck();
            lastStatus = current.interrupt();
            recorder = null;
            releaseLock();
        }
    }

    private ManagedCaptureRecorder requireRecorder() {
        if (recorder == null || !isActive(lastStatus.state())) {
            throw new IllegalStateException("SHAFT Capture is not active.");
        }
        return recorder;
    }

    private <T> T invoke(java.util.concurrent.Callable<T> operation) {
        try {
            return executor.submit(operation).get();
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("SHAFT Capture control was interrupted.", exception);
        } catch (ExecutionException exception) {
            Throwable cause = exception.getCause();
            if (cause instanceof RuntimeException runtimeException) {
                throw runtimeException;
            }
            throw new IllegalStateException("SHAFT Capture control failed.", cause);
        }
    }

    private void cancelHealthCheck() {
        if (healthCheck != null) {
            healthCheck.cancel(false);
            healthCheck = null;
        }
    }

    private void releaseLock() {
        if (sessionLock != null) {
            sessionLock.close();
            sessionLock = null;
        }
    }

    private static boolean isActive(CaptureStatus.State state) {
        return state == CaptureStatus.State.STARTING
                || state == CaptureStatus.State.ACTIVE
                || state == CaptureStatus.State.STOPPING;
    }

    private static CaptureStatus copyWithState(CaptureStatus status, CaptureStatus.State state) {
        return new CaptureStatus(
                state,
                status.sessionId(),
                status.browser(),
                status.currentUrl(),
                status.eventCount(),
                status.warnings(),
                status.outputPath(),
                status.aiEnabled(),
                status.processId(),
                status.startedAt());
    }
}
