package com.shaft.gui.mobile.internal;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/** Weak-identity lifecycle owner shared by explicit and automatic Appium screen recording. */
public final class MobileRecordingState {
    public enum Owner { EXPLICIT, AUTOMATIC }

    private static final ReferenceQueue<AppiumDriver> STALE_DRIVERS = new ReferenceQueue<>();
    private static final Map<IdentityWeakReference, State> STATES = new HashMap<>();

    private MobileRecordingState() {
        throw new IllegalStateException("Utility class");
    }

    public static void start(AppiumDriver driver, Owner owner, long maxBytes, Runnable providerStart) {
        State state = state(driver);
        synchronized (state) {
            requireOpen(state);
            if (state.phase != Phase.IDLE) {
                throw new IllegalStateException("A mobile screen recording is already active or changing state.");
            }
            state.phase = Phase.STARTING;
            state.owner = Objects.requireNonNull(owner, "recording owner");
            state.maxBytes = maxBytes;
        }
        try {
            Objects.requireNonNull(providerStart, "provider start").run();
        } catch (RuntimeException exception) {
            synchronized (state) {
                if (state.phase != Phase.CLOSED) {
                    reset(state);
                }
            }
            throw exception;
        }
        synchronized (state) {
            requireOpen(state);
            state.phase = Phase.ACTIVE;
        }
    }

    public static byte[] stop(AppiumDriver driver, Owner owner, Supplier<String> providerStop) {
        State state = state(driver);
        long maxBytes;
        synchronized (state) {
            requireOpen(state);
            if (state.phase != Phase.ACTIVE || state.owner != owner) {
                throw new IllegalStateException("No mobile screen recording owned by this caller is active.");
            }
            state.phase = Phase.STOPPING;
            maxBytes = state.maxBytes;
        }
        String payload;
        try {
            payload = Objects.requireNonNull(providerStop, "provider stop").get();
        } catch (RuntimeException exception) {
            synchronized (state) {
                if (state.phase != Phase.CLOSED) {
                    state.phase = Phase.ACTIVE;
                }
            }
            throw exception;
        }
        synchronized (state) {
            requireOpen(state);
            reset(state);
        }
        return decode(payload, maxBytes);
    }

    /** Reports whether one caller still owns an active provider recording. */
    public static boolean isActive(AppiumDriver driver, Owner owner) {
        State state = state(driver);
        synchronized (state) {
            return state.phase == Phase.ACTIVE && state.owner == owner;
        }
    }

    /** Marks one driver terminal without issuing a provider command. */
    public static void closeAndRemove(WebDriver driver) {
        if (!(driver instanceof AppiumDriver appiumDriver)) {
            return;
        }
        State state = state(appiumDriver);
        synchronized (state) {
            state.phase = Phase.CLOSED;
            state.owner = null;
            state.maxBytes = 0;
        }
    }

    private static byte[] decode(String payload, long maxBytes) {
        if (payload == null || payload.isBlank()) {
            throw new IllegalArgumentException("The screen recording provider returned an empty payload.");
        }
        long maximumEncodedLength = ((maxBytes + 2) / 3) * 4;
        if (payload.length() > maximumEncodedLength + 2) {
            throw new IllegalArgumentException("The screen recording exceeds the configured decoded-byte limit.");
        }
        final byte[] decoded;
        try {
            decoded = Base64.getDecoder().decode(payload);
        } catch (IllegalArgumentException malformed) {
            throw new IllegalArgumentException("The screen recording provider returned malformed Base64.", malformed);
        }
        if (decoded.length > maxBytes) {
            throw new IllegalArgumentException("The screen recording exceeds the configured decoded-byte limit.");
        }
        return decoded;
    }

    private static void reset(State state) {
        state.phase = Phase.IDLE;
        state.owner = null;
        state.maxBytes = 0;
    }

    private static void requireOpen(State state) {
        if (state.phase == Phase.CLOSED) {
            throw new UnsupportedOperationException("The Appium screen-recording session has been closed.");
        }
    }

    private static State state(AppiumDriver driver) {
        Objects.requireNonNull(driver, "Appium driver");
        synchronized (STATES) {
            expungeStaleDrivers();
            IdentityWeakReference lookup = new IdentityWeakReference(driver);
            State existing = STATES.get(lookup);
            if (existing != null) {
                return existing;
            }
            State created = new State();
            STATES.put(new IdentityWeakReference(driver, STALE_DRIVERS), created);
            return created;
        }
    }

    private static void expungeStaleDrivers() {
        IdentityWeakReference stale;
        while ((stale = (IdentityWeakReference) STALE_DRIVERS.poll()) != null) {
            STATES.remove(stale);
        }
    }

    private enum Phase { IDLE, STARTING, ACTIVE, STOPPING, CLOSED }

    private static final class State {
        private Phase phase = Phase.IDLE;
        private Owner owner;
        private long maxBytes;
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
