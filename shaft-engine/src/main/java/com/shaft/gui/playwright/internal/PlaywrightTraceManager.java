package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Tracing;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Starts and attaches Playwright trace artifacts when explicitly enabled.
 */
public final class PlaywrightTraceManager {
    private static final DateTimeFormatter TRACE_TIMESTAMP = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss-SSS");
    private static final ThreadLocal<Path> LAST_TRACE_PATH = new ThreadLocal<>();

    private final BrowserContext browserContext;
    private final Path artifactsDirectory;
    private boolean tracingStarted;

    PlaywrightTraceManager(BrowserContext browserContext, Path artifactsDirectory) {
        this.browserContext = browserContext;
        this.artifactsDirectory = artifactsDirectory;
    }

    static PlaywrightTraceManager startIfEnabled(BrowserContext browserContext, Path artifactsDirectory) {
        PlaywrightTraceManager traceManager = new PlaywrightTraceManager(browserContext, artifactsDirectory);
        if (SHAFT.Properties.playwright.tracingEnabled()) {
            traceManager.start();
        }
        return traceManager;
    }

    public boolean isTracingStarted() {
        return tracingStarted;
    }

    /**
     * Returns the last Playwright trace zip created on the current thread.
     *
     * @return trace zip path, or {@code null} when no trace has been stopped
     */
    public static Path getLastTracePath() {
        return LAST_TRACE_PATH.get();
    }

    public void start() {
        try {
            Files.createDirectories(artifactsDirectory);
            LAST_TRACE_PATH.remove();
            browserContext.tracing().start(new Tracing.StartOptions()
                    .setScreenshots(SHAFT.Properties.playwright.tracingScreenshots())
                    .setSnapshots(SHAFT.Properties.playwright.tracingSnapshots())
                    .setSources(SHAFT.Properties.playwright.tracingSources()));
            tracingStarted = true;
            ReportManager.logDiscrete("Playwright tracing started.", Level.DEBUG);
        } catch (RuntimeException | IOException e) {
            ReportManager.logDiscrete("Could not start Playwright tracing: " + e.getMessage(), Level.WARN);
        }
    }

    public void stopAndAttach() {
        if (!tracingStarted) {
            return;
        }

        Path tracePath = artifactsDirectory.resolve("playwright-trace-" + TRACE_TIMESTAMP.format(LocalDateTime.now()) + ".zip");
        try {
            browserContext.tracing().stop(new Tracing.StopOptions().setPath(tracePath));
            byte[] traceBytes = Files.readAllBytes(tracePath);
            ReportManagerHelper.attach("Playwright Trace", tracePath.getFileName().toString(),
                    new ByteArrayInputStream(traceBytes));
            LAST_TRACE_PATH.set(tracePath);
            ReportManager.logDiscrete("Playwright trace attached: " + tracePath, Level.INFO);
        } catch (RuntimeException | IOException e) {
            ReportManager.logDiscrete("Could not stop or attach Playwright trace: " + e.getMessage(), Level.WARN);
        } finally {
            tracingStarted = false;
        }
    }
}
