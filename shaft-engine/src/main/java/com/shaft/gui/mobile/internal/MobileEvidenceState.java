package com.shaft.gui.mobile.internal;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/** Coordinates evidence publication with terminal driver teardown. */
public final class MobileEvidenceState {
    private static final Map<IdentityWeakReference, State> STATES = new HashMap<>();
    private static final ReferenceQueue<AppiumDriver> STALE_DRIVERS = new ReferenceQueue<>();

    private MobileEvidenceState() {
        throw new IllegalStateException("Utility class");
    }

    public static void begin(AppiumDriver driver) {
        AppiumDriver required = Objects.requireNonNull(driver, "driver");
        if (required.getSessionId() == null) {
            throw unsupported();
        }
        State state = state(required);
        synchronized (state) {
            requireOpen(state);
        }
    }

    public static void publish(AppiumDriver driver, Runnable publication) {
        AppiumDriver required = Objects.requireNonNull(driver, "driver");
        Runnable requiredPublication = Objects.requireNonNull(publication, "publication");
        State state = existingState(required);
        if (state == null) {
            throw unsupported();
        }
        synchronized (state) {
            requireOpen(state);
            if (required.getSessionId() == null) {
                throw unsupported();
            }
            requiredPublication.run();
        }
    }

    public static void closeAndRemove(WebDriver driver) {
        if (!(driver instanceof AppiumDriver appiumDriver)) {
            return;
        }
        State state = state(appiumDriver);
        synchronized (state) {
            state.closed = true;
        }
    }

    private static State state(AppiumDriver driver) {
        synchronized (STATES) {
            expungeStaleDrivers();
            return STATES.computeIfAbsent(new IdentityWeakReference(driver, STALE_DRIVERS), ignored -> new State());
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
            throw unsupported();
        }
    }

    private static UnsupportedOperationException unsupported() {
        return new UnsupportedOperationException("Mobile evidence capture requires one live Appium session.");
    }

    private static final class State {
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
            AppiumDriver mine = get();
            return mine != null && mine == reference.get();
        }
    }
}
