package com.shaft.gui.browser.internal;

import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.openqa.selenium.WebDriver;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/** Retains immutable legacy browser-log snapshots by exact driver identity. */
public final class LegacyConsoleLogSource {
    private static final int EVENT_LIMIT = 1000;
    private static final ReferenceQueue<WebDriver> STALE_DRIVERS = new ReferenceQueue<>();
    private static final Map<IdentityWeakReference, Entry> SNAPSHOTS = new HashMap<>();

    private LegacyConsoleLogSource() { }

    public static void retain(WebDriver driver,
                              List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> entries) {
        if (driver == null) {
            return;
        }
        synchronized (SNAPSHOTS) {
            expungeStaleDrivers();
            IdentityWeakReference lookup = new IdentityWeakReference(driver);
            Entry entry = SNAPSHOTS.get(lookup);
            if (entry == null) {
                entry = new Entry();
                SNAPSHOTS.put(new IdentityWeakReference(driver, STALE_DRIVERS), entry);
            }
            if (!entry.closed) {
                int incomingFrom = Math.max(0, entries.size() - EVENT_LIMIT);
                int incomingCount = entries.size() - incomingFrom;
                List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> retained =
                        new ArrayList<>(EVENT_LIMIT);
                if (entry.entries != null && incomingCount < EVENT_LIMIT) {
                    int existingFrom = Math.max(0, entry.entries.size() - (EVENT_LIMIT - incomingCount));
                    retained.addAll(entry.entries.subList(existingFrom, entry.entries.size()));
                }
                retained.addAll(entries.subList(incomingFrom, entries.size()));
                entry.entries = List.copyOf(retained);
            }
        }
    }

    public static Optional<List<BrowserObservabilityRecorder.ConsoleSnapshotEntry>> snapshotIfPresent(
            WebDriver driver) {
        if (driver == null) {
            return Optional.empty();
        }
        synchronized (SNAPSHOTS) {
            expungeStaleDrivers();
            Entry entry = SNAPSHOTS.get(new IdentityWeakReference(driver));
            return entry == null || entry.closed || entry.entries == null
                    ? Optional.empty() : Optional.of(List.copyOf(entry.entries));
        }
    }

    public static void closeAndRemove(WebDriver driver) {
        if (driver == null) {
            return;
        }
        synchronized (SNAPSHOTS) {
            expungeStaleDrivers();
            IdentityWeakReference lookup = new IdentityWeakReference(driver);
            Entry entry = SNAPSHOTS.get(lookup);
            if (entry == null) {
                entry = new Entry();
                SNAPSHOTS.put(new IdentityWeakReference(driver, STALE_DRIVERS), entry);
            }
            entry.closed = true;
            entry.entries = null;
        }
    }

    private static void expungeStaleDrivers() {
        IdentityWeakReference stale;
        while ((stale = (IdentityWeakReference) STALE_DRIVERS.poll()) != null) {
            SNAPSHOTS.remove(stale);
        }
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

    private static final class Entry {
        private List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> entries;
        private boolean closed;
    }
}
