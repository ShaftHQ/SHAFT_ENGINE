package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;

/** Injectable host and mutation boundary for BrowserStack Local. */
interface BrowserStackLocalToolchainOperations {
    void hostPreflight(List<SetupAction> actions) throws IOException;

    default void preStatePreflight(List<SetupAction> actions, boolean offline) throws IOException {
        java.util.Objects.requireNonNull(actions, "actions");
        if (offline) return;
    }

    void lockedPreflight(List<SetupAction> actions, boolean offline) throws IOException;

    void install(SetupAction action) throws IOException;

    SetupStatus status(SetupAction action);

    default long startTunnel(Path binary, String accessKey, Path logFile) throws IOException {
        java.util.Objects.requireNonNull(binary, "binary");
        java.util.Objects.requireNonNull(accessKey, "accessKey");
        java.util.Objects.requireNonNull(logFile, "logFile");
        throw new UnsupportedOperationException("BrowserStack Local start is not available.");
    }

    default boolean processRunning(long pid, Path binary) throws IOException {
        java.util.Objects.requireNonNull(binary, "binary");
        throw new IOException("BrowserStack Local process inspection is not available.");
    }

    default void stopProcess(long pid, Path binary) throws IOException {
        java.util.Objects.requireNonNull(binary, "binary");
        throw new UnsupportedOperationException("BrowserStack Local stop is not available.");
    }

    default void awaitReady(Duration timeout) throws IOException {
        java.util.Objects.requireNonNull(timeout, "timeout");
        throw new UnsupportedOperationException("BrowserStack Local readiness is not available.");
    }
}
