package com.shaft.intellij.mcp;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;
import com.intellij.util.Alarm;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;
import java.time.Duration;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Periodic lightweight MCP connection health check using IntelliJ's Alarm.
 * Pings the MCP server every ~30 seconds and updates connection state on failure.
 */
public final class ShaftMcpHeartbeat implements Disposable {
    private static final int PING_INTERVAL_MILLIS = 30_000; // 30 seconds
    private static final Duration PING_TIMEOUT = Duration.ofSeconds(15);

    private final Project project;
    private final ShaftMcpConnectionState connectionState;
    private final Alarm alarm;
    private volatile boolean disposed;

    /**
     * Creates a heartbeat manager.
     *
     * @param project current IntelliJ project
     * @param connectionState connection state tracker
     */
    public ShaftMcpHeartbeat(@NotNull Project project, @NotNull ShaftMcpConnectionState connectionState) {
        this.project = project;
        this.connectionState = connectionState;
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
        scheduleNextPing();
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

    private void scheduleNextPing() {
        if (disposed || alarm.isDisposed()) {
            return;
        }
        alarm.addRequest(this::ping, PING_INTERVAL_MILLIS);
    }

    private void ping() {
        if (disposed) {
            return;
        }
        try {
            ShaftMcpToolResult result = ShaftMcpConnectionProbe.test(
                    getCommand(),
                    getSettings(),
                    getProjectRoot()).get(15, TimeUnit.SECONDS);
            connectionState.setConnected(result.success());
        } catch (InterruptedException interruptedException) {
            Thread.currentThread().interrupt();
            connectionState.setConnected(false);
        } catch (ExecutionException | TimeoutException | RuntimeException exception) {
            connectionState.setConnected(false);
        } finally {
            scheduleNextPing();
        }
    }

    private String getCommand() {
        ShaftSettingsState.Settings settings = getSettings();
        if (settings == null || settings.mcpCommand == null) {
            return "";
        }
        return settings.mcpCommand;
    }

    private ShaftSettingsState.Settings getSettings() {
        return ShaftSettingsState.getInstance().getState();
    }

    private Path getProjectRoot() {
        return project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
    }
}
