package com.shaft.gui.browser.internal;

import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.log.BaseLogEntry;
import org.openqa.selenium.bidi.module.LogInspector;

import java.util.ArrayList;
import java.util.List;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;

/** Session-scoped Selenium BiDi console and JavaScript-error bridge. */
public final class BidiConsoleLogSource implements AutoCloseable {
    private static final int EVENT_LIMIT = 1000;
    private static final ReferenceQueue<WebDriver> STALE_DRIVERS = new ReferenceQueue<>();
    private static final Map<IdentityWeakReference, Entry> CACHE = new HashMap<>();
    private final List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> events = new ArrayList<>();
    private boolean oldestEventOmitted;
    private final BrowserObservabilityRecorder.ObservationBinding observationBinding =
            BrowserObservabilityRecorder.captureBinding();
    private LogInspector inspector;
    private volatile boolean healthy;

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
        boolean accepted;
        synchronized (CACHE) {
            expungeStaleDrivers();
            Entry entry = CACHE.computeIfAbsent(new IdentityWeakReference(driver, STALE_DRIVERS), ignored -> new Entry());
            accepted = !entry.closed;
            if (accepted) {
                entry.source = source;
            }
        }
        if (!accepted) {
            source.close();
        }
    }

    /** Attaches once per session and reports whether BiDi log observation is active. */
    public static boolean attach(WebDriver driver) {
        if (driver == null) {
            return false;
        }
        synchronized (CACHE) {
            expungeStaleDrivers();
            IdentityWeakReference lookup = new IdentityWeakReference(driver);
            Entry existing = CACHE.get(lookup);
            if (existing != null) {
                return !existing.closed && existing.source != null && existing.source.healthy;
            }
            CACHE.put(new IdentityWeakReference(driver, STALE_DRIVERS), new Entry());
        }
        BidiConsoleLogSource created = new BidiConsoleLogSource(driver);
        synchronized (CACHE) {
            expungeStaleDrivers();
            Entry entry = CACHE.get(new IdentityWeakReference(driver));
            if (entry != null && !entry.closed && entry.source == null) {
                entry.source = created;
                return created.healthy;
            }
            created.close();
            return entry != null && !entry.closed && entry.source != null && entry.source.healthy;
        }
    }

    /** @return whether a session-scoped listener is already active */
    public static boolean isHealthy(WebDriver driver) {
        BidiConsoleLogSource source = source(driver);
        return source != null && source.healthy;
    }

    /** @return immutable session console snapshot, oldest first */
    public static List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> snapshot(WebDriver driver) {
        BidiConsoleLogSource source = source(driver);
        return source == null ? List.of() : source.snapshot();
    }

    /** Clears buffered session console events. */
    public static void clear(WebDriver driver) {
        BidiConsoleLogSource source = source(driver);
        if (source != null) {
            source.clearEvents();
        }
    }

    /** Moves buffered BiDi events onto the current reporter thread for trace serialization. */
    public static void drainToRecorder(WebDriver driver) {
        BidiConsoleLogSource source = source(driver);
        if (source == null) {
            return;
        }
        BrowserObservabilityRecorder.ObservationSession owner =
                BrowserObservabilityRecorder.resolveSession(source.observationBinding);
        ConsoleBatch batch = source.takeEvents();
        if (batch.oldestOmitted()) {
            BrowserObservabilityRecorder.recordConsoleOmission(owner);
        }
        for (BrowserObservabilityRecorder.ConsoleSnapshotEntry entry : batch.events()) {
            BrowserObservabilityRecorder.recordConsole(owner,
                    entry.source(), entry.level(), entry.message(), entry.timestamp());
        }
    }

    /** Removes SHAFT's BiDi log listeners during driver teardown. */
    public static void closeAndRemove(WebDriver driver) {
        BidiConsoleLogSource source;
        synchronized (CACHE) {
            if (driver == null) {
                return;
            }
            expungeStaleDrivers();
            IdentityWeakReference lookup = new IdentityWeakReference(driver);
            Entry entry = CACHE.get(lookup);
            if (entry == null) {
                entry = new Entry();
                CACHE.put(new IdentityWeakReference(driver, STALE_DRIVERS), entry);
            }
            entry.closed = true;
            source = entry.source;
            entry.source = null;
        }
        if (source != null) {
            source.close();
        }
    }

    private static BidiConsoleLogSource source(WebDriver driver) {
        synchronized (CACHE) {
            expungeStaleDrivers();
            Entry entry = driver == null ? null : CACHE.get(new IdentityWeakReference(driver));
            return entry == null || entry.closed ? null : entry.source;
        }
    }

    private static void expungeStaleDrivers() {
        IdentityWeakReference stale;
        while ((stale = (IdentityWeakReference) STALE_DRIVERS.poll()) != null) {
            CACHE.remove(stale);
        }
    }

    private static final class Entry {
        private BidiConsoleLogSource source;
        private boolean closed;
    }

    private static final class IdentityWeakReference extends WeakReference<WebDriver> {
        private final int identityHash;

        private IdentityWeakReference(WebDriver driver) {
            super(driver);
            identityHash = System.identityHashCode(driver);
        }

        private IdentityWeakReference(WebDriver driver, ReferenceQueue<WebDriver> queue) {
            super(driver, queue);
            identityHash = System.identityHashCode(driver);
        }

        @Override public int hashCode() { return identityHash; }

        @Override public boolean equals(Object other) {
            if (this == other) return true;
            if (!(other instanceof IdentityWeakReference reference)) return false;
            WebDriver referent = get();
            return referent != null && referent == reference.get();
        }
    }

    private synchronized void record(BaseLogEntry entry) {
        if (entry != null) {
            record(String.valueOf(entry.getLevel()), entry.getText(), entry.getTimestamp());
        }
    }

    synchronized void record(String level, String text, long timestamp) {
        if (!healthy) {
            return;
        }
        if (events.size() >= EVENT_LIMIT) {
            events.removeFirst();
            oldestEventOmitted = true;
        }
        events.add(BrowserObservabilityRecorder.consoleEntry("bidi", level, text, timestamp));
    }

    private synchronized List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> snapshot() {
        return List.copyOf(events);
    }

    private synchronized void clearEvents() {
        events.clear();
        oldestEventOmitted = false;
    }

    private synchronized ConsoleBatch takeEvents() {
        ConsoleBatch snapshot = new ConsoleBatch(List.copyOf(events), oldestEventOmitted);
        events.clear();
        oldestEventOmitted = false;
        return snapshot;
    }

    private record ConsoleBatch(List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> events,
                                boolean oldestOmitted) { }

    @Override
    public void close() {
        LogInspector currentInspector;
        synchronized (this) {
            healthy = false;
            events.clear();
            oldestEventOmitted = false;
            currentInspector = inspector;
            inspector = null;
        }
        if (currentInspector != null) {
            try {
                currentInspector.close();
            } catch (RuntimeException ignored) {
                // The driver may already be closed.
            }
        }
    }
}
