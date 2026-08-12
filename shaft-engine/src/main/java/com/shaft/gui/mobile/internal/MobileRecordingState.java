package com.shaft.gui.mobile.internal;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
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

    /** Retains one bounded descriptor only after caller-owned publication succeeds. */
    public static void retainSavedRecording(AppiumDriver driver, Path path, byte[] recording) {
        Objects.requireNonNull(driver, "Appium driver");
        Path normalized = Objects.requireNonNull(path, "recording path").toAbsolutePath().normalize();
        byte[] requiredRecording = Objects.requireNonNull(recording, "recording bytes");
        SavedRecording saved = new SavedRecording(normalized, requiredRecording.length, sha256(requiredRecording));
        State state = existingState(driver);
        if (state == null) {
            return;
        }
        synchronized (state) {
            if (state.phase != Phase.CLOSED) {
                state.savedRecording = saved;
            }
        }
    }

    /** Returns one atomic non-creating recording lifecycle snapshot. */
    public static Optional<Snapshot> snapshotIfPresent(AppiumDriver driver) {
        State state = existingState(Objects.requireNonNull(driver, "Appium driver"));
        if (state == null) {
            return Optional.empty();
        }
        synchronized (state) {
            if (state.phase == Phase.CLOSED) {
                return Optional.empty();
            }
            return Optional.of(new Snapshot(state.phase != Phase.IDLE,
                    Optional.ofNullable(state.savedRecording)));
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
            state.savedRecording = null;
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

    private static State existingState(AppiumDriver driver) {
        synchronized (STATES) {
            expungeStaleDrivers();
            return STATES.get(new IdentityWeakReference(driver));
        }
    }

    private static String sha256(byte[] recording) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(recording));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
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
        private SavedRecording savedRecording;
    }

    /** Immutable descriptor for the latest successfully published recording. */
    public record SavedRecording(Path path, long sizeBytes, String sha256) {
        public SavedRecording {
            path = Objects.requireNonNull(path, "recording path").toAbsolutePath().normalize();
            if (sizeBytes < 0) {
                throw new IllegalArgumentException("Recording size must not be negative.");
            }
            sha256 = Objects.requireNonNull(sha256, "recording digest");
            if (!sha256.matches("[0-9a-f]{64}")) {
                throw new IllegalArgumentException("Recording digest must be lowercase SHA-256 hex.");
            }
        }

        @Override
        public String toString() {
            return "SavedRecording[sizeBytes=" + sizeBytes + "]";
        }
    }

    /** Immutable read-only view of recording activity and the latest successful save. */
    public record Snapshot(boolean recordingInProgress, Optional<SavedRecording> savedRecording) {
        public Snapshot {
            savedRecording = Objects.requireNonNull(savedRecording, "saved recording");
        }
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
