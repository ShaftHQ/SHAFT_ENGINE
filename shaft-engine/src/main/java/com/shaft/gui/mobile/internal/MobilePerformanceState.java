package com.shaft.gui.mobile.internal;

import com.shaft.gui.driver.MobilePerformanceSample;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/** Session-identity-keyed bounded storage for Appium performance samples. */
public final class MobilePerformanceState {
    private static final int MAX_SAMPLES = 100;
    private static final ReferenceQueue<AppiumDriver> STALE_DRIVERS = new ReferenceQueue<>();
    private static final Map<IdentityWeakReference, State> STATES = new HashMap<>();

    private MobilePerformanceState() {
        throw new IllegalStateException("Utility class");
    }

    public static void append(AppiumDriver driver, MobilePerformanceSample sample) {
        State state = state(driver);
        synchronized (state) {
            requireOpen(state);
            while (state.samples.size() >= MAX_SAMPLES) {
                state.samples.removeFirst();
            }
            state.samples.addLast(Objects.requireNonNull(sample, "performance sample"));
        }
    }

    public static List<MobilePerformanceSample> history(AppiumDriver driver) {
        State state = state(driver);
        synchronized (state) {
            requireOpen(state);
            return List.copyOf(state.samples);
        }
    }

    /** Returns an immutable history snapshot without creating per-driver state. */
    public static Optional<List<MobilePerformanceSample>> historyIfPresent(AppiumDriver driver) {
        State state = existingState(Objects.requireNonNull(driver, "Appium driver"));
        if (state == null) {
            return Optional.empty();
        }
        synchronized (state) {
            if (state.closed) {
                return Optional.empty();
            }
            return Optional.of(List.copyOf(state.samples));
        }
    }

    public static void clear(AppiumDriver driver) {
        clearAndCount(driver);
    }

    /** Atomically clears and returns the number of samples removed. */
    public static int clearAndCount(AppiumDriver driver) {
        State state = state(driver);
        synchronized (state) {
            requireOpen(state);
            int count = state.samples.size();
            state.samples.clear();
            return count;
        }
    }

    /** Removes buffered state without issuing commands to a closing session. */
    public static void closeAndRemove(WebDriver driver) {
        if (!(driver instanceof AppiumDriver appiumDriver)) {
            return;
        }
        State state;
        synchronized (STATES) {
            expungeStaleDrivers();
            IdentityWeakReference lookup = new IdentityWeakReference(appiumDriver);
            state = STATES.get(lookup);
            if (state == null) {
                state = new State();
                STATES.put(new IdentityWeakReference(appiumDriver, STALE_DRIVERS), state);
            }
        }
        synchronized (state) {
            state.samples.clear();
            state.closed = true;
        }
    }

    private static State state(AppiumDriver driver) {
        Objects.requireNonNull(driver, "Appium driver");
        synchronized (STATES) {
            expungeStaleDrivers();
            IdentityWeakReference lookup = new IdentityWeakReference(driver);
            State state = STATES.get(lookup);
            if (state != null) {
                return state;
            }
            State created = new State();
            STATES.put(new IdentityWeakReference(driver, STALE_DRIVERS), created);
            return created;
        }
    }

    private static State existingState(AppiumDriver driver) {
        synchronized (STATES) {
            expungeStaleDrivers();
            return STATES.get(new IdentityWeakReference(driver));
        }
    }

    private static void expungeStaleDrivers() {
        IdentityWeakReference stale;
        while ((stale = (IdentityWeakReference) STALE_DRIVERS.poll()) != null) {
            STATES.remove(stale);
        }
    }

    private static void requireOpen(State state) {
        if (state.closed) {
            throw new UnsupportedOperationException("The Appium performance-data session has been closed.");
        }
    }

    private static final class State {
        private final Deque<MobilePerformanceSample> samples = new ArrayDeque<>();
        private boolean closed;
    }

    private static final class IdentityWeakReference extends WeakReference<AppiumDriver> {
        private final int identityHash;

        private IdentityWeakReference(AppiumDriver driver) {
            super(driver);
            identityHash = System.identityHashCode(driver);
        }

        private IdentityWeakReference(AppiumDriver driver, ReferenceQueue<AppiumDriver> queue) {
            super(driver, queue);
            identityHash = System.identityHashCode(driver);
        }

        @Override
        public int hashCode() {
            return identityHash;
        }

        @Override
        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (!(other instanceof IdentityWeakReference reference)) {
                return false;
            }
            AppiumDriver driver = get();
            return driver != null && driver == reference.get();
        }
    }
}
