package com.shaft.gui.browser.internal;

import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.log.BaseLogEntry;
import org.openqa.selenium.bidi.module.LogInspector;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/** Session-scoped Selenium BiDi console and JavaScript-error bridge. */
public final class BidiConsoleLogSource implements AutoCloseable {
    private static final int EVENT_LIMIT = 1000;
    private static final ConcurrentHashMap<WebDriver, BidiConsoleLogSource> CACHE = new ConcurrentHashMap<>();
    private final List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> events = new ArrayList<>();
    private LogInspector inspector;
    private boolean healthy;

    BidiConsoleLogSource() {
        healthy = true;
    }

    private BidiConsoleLogSource(WebDriver driver) {
        try {
            inspector = new LogInspector(driver);
            inspector.onConsoleEntry(this::record);
            inspector.onJavaScriptException(this::record);
            healthy = true;
        } catch (RuntimeException ignored) {
            close();
        }
    }

    static void install(WebDriver driver, BidiConsoleLogSource source) {
        CACHE.put(driver, source);
    }

    /** Attaches once per session and reports whether BiDi log observation is active. */
    public static boolean attach(WebDriver driver) {
        return driver != null && CACHE.computeIfAbsent(driver, BidiConsoleLogSource::new).healthy;
    }

    /** @return whether a session-scoped listener is already active */
    public static boolean isHealthy(WebDriver driver) {
        BidiConsoleLogSource source = driver == null ? null : CACHE.get(driver);
        return source != null && source.healthy;
    }

    /** @return immutable session console snapshot, oldest first */
    public static List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> snapshot(WebDriver driver) {
        BidiConsoleLogSource source = driver == null ? null : CACHE.get(driver);
        return source == null ? List.of() : source.snapshot();
    }

    /** Clears buffered session console events. */
    public static void clear(WebDriver driver) {
        BidiConsoleLogSource source = driver == null ? null : CACHE.get(driver);
        if (source != null) {
            source.clearEvents();
        }
    }

    /** Moves buffered BiDi events onto the current reporter thread for trace serialization. */
    public static void drainToRecorder(WebDriver driver) {
        BidiConsoleLogSource source = driver == null ? null : CACHE.get(driver);
        if (source == null) {
            return;
        }
        for (BrowserObservabilityRecorder.ConsoleSnapshotEntry entry : source.takeEvents()) {
            BrowserObservabilityRecorder.recordConsole(entry.source(), entry.level(), entry.message(), entry.timestamp());
        }
    }

    /** Removes SHAFT's BiDi log listeners during driver teardown. */
    public static void closeAndRemove(WebDriver driver) {
        BidiConsoleLogSource source = driver == null ? null : CACHE.remove(driver);
        if (source != null) {
            source.close();
        }
    }

    private synchronized void record(BaseLogEntry entry) {
        if (entry != null) {
            record(String.valueOf(entry.getLevel()), entry.getText(), entry.getTimestamp());
        }
    }

    synchronized void record(String level, String text, long timestamp) {
        if (events.size() >= EVENT_LIMIT) {
            events.removeFirst();
        }
        events.add(BrowserObservabilityRecorder.consoleEntry("bidi", level, text, timestamp));
    }

    private synchronized List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> snapshot() {
        return List.copyOf(events);
    }

    private synchronized void clearEvents() {
        events.clear();
    }

    private synchronized List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> takeEvents() {
        List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> snapshot = List.copyOf(events);
        events.clear();
        return snapshot;
    }

    @Override
    public void close() {
        healthy = false;
        clearEvents();
        if (inspector != null) {
            try {
                inspector.close();
            } catch (RuntimeException ignored) {
                // The driver may already be closed.
            } finally {
                inspector = null;
            }
        }
    }
}
