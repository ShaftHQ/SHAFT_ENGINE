package com.shaft.infrastructure;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;

/** Injectable host and mutation boundary for the Healenium setup transaction. */
interface HealeniumToolchainOperations {
    void hostPreflight(List<SetupAction> actions) throws IOException;

    default void preStatePreflight(List<SetupAction> actions, boolean offline) throws IOException {
        java.util.Objects.requireNonNull(actions, "actions");
        // Healenium has no safe read that must occur before the shared state directory exists.
        // offline is part of the shared provider SPI and is unused until that read exists.
        if (offline) return;
    }

    void lockedPreflight(List<SetupAction> actions, boolean offline) throws IOException;

    void install(SetupAction action) throws IOException;

    SetupStatus status(SetupAction action);

    default void composeUp(Path composeFile, String project) throws IOException {
        java.util.Objects.requireNonNull(composeFile, "composeFile");
        java.util.Objects.requireNonNull(project, "project");
        throw new UnsupportedOperationException("Compose start is not available.");
    }

    default void composeDown(Path composeFile, String project) throws IOException {
        java.util.Objects.requireNonNull(composeFile, "composeFile");
        java.util.Objects.requireNonNull(project, "project");
        throw new UnsupportedOperationException("Compose stop is not available.");
    }

    default boolean composeRunning(Path composeFile, String project) throws IOException {
        java.util.Objects.requireNonNull(composeFile, "composeFile");
        java.util.Objects.requireNonNull(project, "project");
        throw new IOException("Compose inspection is not available.");
    }

    default void awaitReady(URI backend, URI imitate, Duration timeout) throws IOException {
        java.util.Objects.requireNonNull(backend, "backend");
        java.util.Objects.requireNonNull(imitate, "imitate");
        java.util.Objects.requireNonNull(timeout, "timeout");
        throw new UnsupportedOperationException("Healenium readiness is not available.");
    }
}
