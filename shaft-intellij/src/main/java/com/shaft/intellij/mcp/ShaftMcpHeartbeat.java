package com.shaft.intellij.mcp;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;
import com.intellij.util.Alarm;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;
import java.time.Duration;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Periodic lightweight MCP connection health check using IntelliJ's Alarm.
 *
 * <p>When the project's shared long-lived MCP client exists, the heartbeat only PEEKS at its
 * liveness — no process is spawned and the shared client is never created, replaced, or closed
 * (respawn churn kills in-progress capture/driver sessions; see {@link ShaftMcpProjectScope}).
 * The connection indicator then means "the MCP session this panel actually uses is alive".
 *
 * <p>Only while no shared client exists yet (nothing has been invoked in this project) does the
 * heartbeat fall back to the historical spawn-probe, which verifies the configured command can
 * launch a fresh process. Consecutive spawn-probe failures back off exponentially (30s, 60s,
 * 120s, 240s, capped at 5 minutes) instead of burning a fresh OS process every 30 seconds
 * against a broken configuration (issue #3399).
 */
public final class ShaftMcpHeartbeat implements Disposable {
    private static final int PING_INTERVAL_MILLIS = 30_000; // 30 seconds
    private static final int MAX_BACKOFF_MILLIS = 300_000; // 5 minutes
    private static final Duration PING_TIMEOUT = Duration.ofSeconds(15);

    /**
     * Reports the shared long-lived MCP client's liveness without side effects, or empty when no
     * shared client exists. Extracted so tests can drive the heartbeat without an IntelliJ
     * service container.
     */
    @FunctionalInterface
    interface SharedClientLiveness {
        Optional<Boolean> peek();
    }

    /**
     * Spawns a fresh probe process for the configured MCP command, resolving the command and
     * settings itself at call time. Extracted so tests never launch a real process (or touch the
     * IntelliJ settings container).
     */
    @FunctionalInterface
    interface ConnectionProbe {
        CompletableFuture<ShaftMcpToolResult> test();
    }

    private final ShaftMcpConnectionState connectionState;
    private final SharedClientLiveness sharedClientLiveness;
    private final ConnectionProbe probe;
    private final Alarm alarm;
    private volatile boolean disposed;
    private int probeFailureStreak;

    /**
     * Creates a heartbeat manager.
     *
     * @param project current IntelliJ project
     * @param connectionState connection state tracker
     */
    public ShaftMcpHeartbeat(@NotNull Project project, @NotNull ShaftMcpConnectionState connectionState) {
        this(connectionState,
                () -> ShaftMcpInvocationService.getInstance(project).peekSharedClientAlive(),
                () -> ShaftMcpConnectionProbe.test(
                        configuredCommand(), configuredSettings(), projectRoot(project)));
    }

    ShaftMcpHeartbeat(@NotNull ShaftMcpConnectionState connectionState,
                      @NotNull SharedClientLiveness sharedClientLiveness,
                      @NotNull ConnectionProbe probe) {
        this.connectionState = connectionState;
        this.sharedClientLiveness = sharedClientLiveness;
        this.probe = probe;
        this.alarm = new Alarm(Alarm.ThreadToUse.POOLED_THREAD, this);
    }

    /**
     * Starts the periodic heartbeat if not already running.
     */
    public synchronized void start() {
        if (disposed || alarm.isDisposed()) {
            return;
        }
        if (!alarm.isEmpty()) {
            return; // Already scheduled
        }
        scheduleNextPing(PING_INTERVAL_MILLIS);
    }

    /**
     * Stops the periodic heartbeat.
     */
    public synchronized void stop() {
        alarm.cancelAllRequests();
    }

    @Override
    public void dispose() {
        disposed = true;
        stop();
    }

    private void scheduleNextPing(int delayMillis) {
        if (disposed || alarm.isDisposed()) {
            return;
        }
        alarm.addRequest(this::ping, delayMillis);
    }

    private void ping() {
        if (disposed) {
            return;
        }
        scheduleNextPing(performPing());
    }

    /**
     * Runs one heartbeat cycle and returns the delay until the next one. Package-private so tests
     * can drive cycles synchronously without IntelliJ's Alarm.
     */
    int performPing() {
        Optional<Boolean> sharedClientAlive = peekSharedClient();
        if (sharedClientAlive.isPresent()) {
            // Free peek at the session the panel actually uses: no process spawn, no backoff
            // needed, and never any churn against the shared client.
            connectionState.setConnected(sharedClientAlive.get());
            probeFailureStreak = 0;
            return PING_INTERVAL_MILLIS;
        }
        try {
            ShaftMcpToolResult result = probe.test().get(PING_TIMEOUT.toSeconds(), TimeUnit.SECONDS);
            connectionState.setConnected(result.success());
            return result.success() ? resetBackoff() : nextBackoff();
        } catch (InterruptedException interruptedException) {
            Thread.currentThread().interrupt();
            connectionState.setConnected(false);
            return nextBackoff();
        } catch (ExecutionException | TimeoutException | RuntimeException exception) {
            connectionState.setConnected(false);
            return nextBackoff();
        }
    }

    private Optional<Boolean> peekSharedClient() {
        try {
            return sharedClientLiveness.peek();
        } catch (RuntimeException exception) {
            // Service container unavailable (headless tests, disposing project): fall back to
            // the spawn-probe path rather than failing the heartbeat cycle.
            return Optional.empty();
        }
    }

    private int resetBackoff() {
        probeFailureStreak = 0;
        return PING_INTERVAL_MILLIS;
    }

    private int nextBackoff() {
        probeFailureStreak = Math.min(probeFailureStreak + 1, 30);
        long backedOff = (long) PING_INTERVAL_MILLIS << Math.min(probeFailureStreak, 4);
        return (int) Math.min(backedOff, MAX_BACKOFF_MILLIS);
    }

    private static String configuredCommand() {
        ShaftSettingsState.Settings settings = configuredSettings();
        if (settings == null || settings.mcpCommand == null) {
            return "";
        }
        return settings.mcpCommand;
    }

    private static ShaftSettingsState.Settings configuredSettings() {
        return ShaftSettingsState.getInstance().getState();
    }

    private static Path projectRoot(Project project) {
        return project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
    }
}
