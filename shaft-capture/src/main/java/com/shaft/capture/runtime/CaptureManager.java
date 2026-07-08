package com.shaft.capture.runtime;

import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.network.ResourceKind;
import com.shaft.capture.storage.CaptureSessionStore;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
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
    // Transient WebDriver hiccups (a busy page, a slow DevTools round trip) can make one
    // liveness probe fail while the browser is perfectly healthy. Only consecutive failures
    // may tear the session down, or a single flaky check silently discards the recording.
    private static final int HEALTH_FAILURES_BEFORE_INTERRUPT = 3;

    private final ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor(runnable -> {
        Thread thread = new Thread(runnable, "shaft-capture-manager");
        thread.setDaemon(false);
        return thread;
    });
    private static final java.util.Set<String> SUPPORTED_MODES = java.util.Set.of("record", "inspect");

    private final Function<CaptureStartRequest, ManagedCaptureRecorder> recorderFactory;
    private volatile ManagedCaptureRecorder recorder;
    private volatile CaptureStatus lastStatus = CaptureStatus.notRunning();
    private volatile String mode = "record";
    private CaptureSingleSessionLock sessionLock;
    private ScheduledFuture<?> healthCheck;
    private int consecutiveHealthFailures;

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
                cleanupExistingSessionOutput(request.outputPath());
                recorder = recorderFactory.apply(request);
                recorder.start();
                lastStatus = recorder.status();
                consecutiveHealthFailures = 0;
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
    public synchronized CaptureStatus status() {
        return syncRecorderStatus();
    }

    /**
     * Returns the current server-side step list for the active session, so the recorder UI (or an
     * external control client) can rehydrate its step list from the session store rather than
     * page-scoped browser storage.
     *
     * @return ordered safe step summaries, or an empty list when no session is active
     */
    public synchronized java.util.List<com.shaft.capture.model.CaptureStep> steps() {
        ManagedCaptureRecorder current = recorder;
        return current == null ? java.util.List.of() : current.steps();
    }

    /**
     * Returns the recorder's current live authoring mode: {@code record} (default; the recorder
     * captures interactions as replayable steps) or {@code inspect} (the recorder overlay
     * highlights hovered elements for a live locator pick instead of recording an interaction
     * step). This is in-memory only, mirroring {@link #steps()}/{@link #status()}: it reflects the
     * live recorder, not durable session state.
     *
     * @return {@code "record"} or {@code "inspect"}
     */
    public String mode() {
        return mode;
    }

    /**
     * Sets the recorder's live authoring mode.
     *
     * @param requestedMode {@code "record"} or {@code "inspect"} (case-insensitive)
     * @return the mode now in effect
     * @throws IllegalArgumentException when {@code requestedMode} is not a supported mode
     */
    public String setMode(String requestedMode) {
        String normalized = requestedMode == null ? "" : requestedMode.trim().toLowerCase(java.util.Locale.ROOT);
        if (!SUPPORTED_MODES.contains(normalized)) {
            throw new IllegalArgumentException("Unsupported capture mode. Supported: " + SUPPORTED_MODES);
        }
        mode = normalized;
        return mode;
    }

    /**
     * Returns the list of network transactions from the active session.
     *
     * @return ordered network transaction summaries, or an empty list when no session is active
     */
    public synchronized java.util.List<NetworkTransaction> networkTransactions() {
        ManagedCaptureRecorder current = recorder;
        return current == null ? java.util.List.of() : current.networkTransactions();
    }

    /**
     * Returns network transactions from the active session filtered per the supplied capture
     * options, bounded to the most recent {@code limit} entries in capture order.
     *
     * @param options asset-noise and pattern filters; {@code null} applies no filtering
     * @param limit maximum number of transactions to return; non-positive values are treated as
     *              unbounded
     * @return ordered, filtered, bounded network transaction summaries
     */
    public synchronized java.util.List<NetworkTransaction> networkTransactions(
            NetworkCaptureOptions options, int limit) {
        java.util.List<NetworkTransaction> transactions = networkTransactions();
        if (transactions.isEmpty()) {
            return transactions;
        }
        java.util.stream.Stream<NetworkTransaction> filtered = transactions.stream()
                .filter(transaction -> matches(transaction, options));
        if (limit > 0) {
            filtered = filtered.limit(limit);
        }
        return filtered.toList();
    }

    private static boolean matches(NetworkTransaction transaction, NetworkCaptureOptions options) {
        if (options == null) {
            return true;
        }
        if (options.excludeAssets && isAssetResource(transaction.resourceKind())) {
            return false;
        }
        if (!options.excludePattern.isBlank() && matchesGlob(transaction.url(), options.excludePattern)) {
            return false;
        }
        if (!options.includePattern.isBlank() && !matchesGlob(transaction.url(), options.includePattern)) {
            return false;
        }
        return true;
    }

    /**
     * Reports whether a transaction's resource kind is asset-like noise rather than an API call.
     *
     * @param resourceKind {@link com.shaft.capture.model.network.ResourceKind#name()} value, case-insensitive
     * @return {@code true} when the resource kind is asset-type noise
     */
    private static boolean isAssetResource(String resourceKind) {
        if (resourceKind == null || resourceKind.isBlank()) {
            return false;
        }
        try {
            return ResourceKind.valueOf(resourceKind.trim().toUpperCase(java.util.Locale.ROOT)).isAsset();
        } catch (IllegalArgumentException unknownResourceKind) {
            return false;
        }
    }

    private static boolean matchesGlob(String value, String globPattern) {
        if (value == null || globPattern == null || globPattern.isBlank()) {
            return false;
        }
        for (String pattern : globPattern.split("\\|")) {
            String trimmed = pattern.trim();
            if (!trimmed.isEmpty() && value.matches(globToRegex(trimmed))) {
                return true;
            }
        }
        return false;
    }

    private static String globToRegex(String glob) {
        StringBuilder regex = new StringBuilder("(?i)");
        for (char character : glob.toCharArray()) {
            if (character == '*') {
                regex.append(".*");
            } else {
                regex.append(java.util.regex.Pattern.quote(String.valueOf(character)));
            }
        }
        return regex.toString();
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
        CaptureStatus currentStatus = status();
        ManagedCaptureRecorder current = recorder;
        if (current == null || !isActive(currentStatus.state())) {
            return currentStatus;
        }
        lastStatus = copyWithState(currentStatus, CaptureStatus.State.STOPPING);
        return invoke(() -> {
            cancelHealthCheck();
            lastStatus = current.stop(discard);
            if (recorder == current) {
                recorder = null;
            }
            releaseLock();
            return lastStatus;
        });
    }

    @Override
    public synchronized void close() {
        ManagedCaptureRecorder current = recorder;
        if (current != null) {
            invoke(() -> {
                cancelHealthCheck();
                lastStatus = current.interrupt();
                if (recorder == current) {
                    recorder = null;
                }
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
        lastStatus = current.status();
        if (!isActive(lastStatus.state())) {
            cancelHealthCheck();
            recorder = null;
            releaseLock();
            return;
        }
        if (current.isBrowserAlive()) {
            consecutiveHealthFailures = 0;
            return;
        }
        consecutiveHealthFailures++;
        if (consecutiveHealthFailures < HEALTH_FAILURES_BEFORE_INTERRUPT) {
            return;
        }
        cancelHealthCheck();
        lastStatus = current.interrupt();
        recorder = null;
        releaseLock();
    }

    private ManagedCaptureRecorder requireRecorder() {
        syncRecorderStatus();
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

    private void cleanupExistingSessionOutput(Path outputPath) {
        if (!Files.isRegularFile(outputPath)) {
            return;
        }
        try {
            new CaptureSessionStore(outputPath).read();
        } catch (RuntimeException exception) {
            throw new IllegalStateException("Capture output already exists and is not a valid capture session.", exception);
        }
        try {
            Files.delete(outputPath);
        } catch (IOException exception) {
            throw new IllegalStateException(
                    "Capture output could not be replaced. Remove " + outputPath.toAbsolutePath() + " before retrying.",
                    exception);
        }
    }

    private void releaseLock() {
        if (sessionLock != null) {
            sessionLock.close();
            sessionLock = null;
        }
    }

    private CaptureStatus syncRecorderStatus() {
        ManagedCaptureRecorder current = recorder;
        if (current != null && isActive(lastStatus.state())) {
            lastStatus = current.status();
            if (!isActive(lastStatus.state())) {
                cancelHealthCheck();
                recorder = null;
                releaseLock();
            }
        }
        return lastStatus;
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
                status.readiness(),
                status.warnings(),
                status.outputPath(),
                status.aiEnabled(),
                status.processId(),
                status.startedAt(),
                status.networkTransactionCount(),
                status.lastEndpoints());
    }
}
