package com.shaft.infrastructure;

import java.net.URI;
import java.time.Duration;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.Objects;
import java.util.Optional;

/** Immutable caller policy for planning, verifying, installing, and starting setup profiles. */
public record SetupOptions(SetupProfile profile, SetupMode mode, ShaftCachePaths paths,
                           boolean offline, boolean autoStart, boolean preferSystemTools,
                           boolean reuseOwnedProcesses, Duration startupTimeout,
                           Duration shutdownTimeout, Optional<URI> remoteEndpoint) {
    public SetupOptions {
        Objects.requireNonNull(profile, "profile");
        Objects.requireNonNull(mode, "mode");
        Objects.requireNonNull(paths, "paths");
        startupTimeout = positive(startupTimeout, "startupTimeout");
        shutdownTimeout = positive(shutdownTimeout, "shutdownTimeout");
        remoteEndpoint = Objects.requireNonNull(remoteEndpoint, "remoteEndpoint");
        remoteEndpoint.ifPresent(SetupOptions::validateRemoteEndpoint);
    }

    public static SetupOptions defaults(SetupProfile profile, ShaftCachePaths paths) {
        return new SetupOptions(profile, SetupMode.EXTERNAL, paths, false, false,
                true, true, Duration.ofMinutes(2), Duration.ofSeconds(30), Optional.empty());
    }

    /** An explicit remote endpoint always keeps local setup non-mutating. */
    public SetupMode effectiveMode() {
        return remoteEndpoint.isPresent() ? SetupMode.EXTERNAL : mode;
    }

    public SetupOptions withMode(SetupMode value) {
        return copy(value, offline, autoStart, preferSystemTools, reuseOwnedProcesses,
                startupTimeout, shutdownTimeout, remoteEndpoint);
    }

    /**
     * Reuses the same execution policy and owned paths for another setup profile.
     *
     * @param value replacement profile
     * @return a copy with the selected profile
     */
    public SetupOptions withProfile(SetupProfile value) {
        return new SetupOptions(Objects.requireNonNull(value, "value"), mode, paths, offline, autoStart,
                preferSystemTools, reuseOwnedProcesses, startupTimeout, shutdownTimeout, remoteEndpoint);
    }

    public SetupOptions withOffline(boolean value) {
        return copy(mode, value, autoStart, preferSystemTools, reuseOwnedProcesses,
                startupTimeout, shutdownTimeout, remoteEndpoint);
    }

    public SetupOptions withAutoStart(boolean value) {
        return copy(mode, offline, value, preferSystemTools, reuseOwnedProcesses,
                startupTimeout, shutdownTimeout, remoteEndpoint);
    }

    public SetupOptions withPreferSystemTools(boolean value) {
        return copy(mode, offline, autoStart, value, reuseOwnedProcesses,
                startupTimeout, shutdownTimeout, remoteEndpoint);
    }

    public SetupOptions withReuseOwnedProcesses(boolean value) {
        return copy(mode, offline, autoStart, preferSystemTools, value,
                startupTimeout, shutdownTimeout, remoteEndpoint);
    }

    public SetupOptions withTimeouts(Duration startup, Duration shutdown) {
        return copy(mode, offline, autoStart, preferSystemTools, reuseOwnedProcesses,
                startup, shutdown, remoteEndpoint);
    }

    public SetupOptions withRemoteEndpoint(URI endpoint) {
        return copy(mode, offline, autoStart, preferSystemTools, reuseOwnedProcesses,
                startupTimeout, shutdownTimeout, Optional.of(Objects.requireNonNull(endpoint, "endpoint")));
    }

    /** Content address of every option that can change setup execution or its destinations. */
    public String policyDigest() {
        String value = String.join("\u0000", profile.name(), mode.name(), effectiveMode().name(),
                paths.cacheRoot().toString(), paths.dataRoot().toString(), paths.downloads().toString(),
                paths.tools().toString(), paths.state().toString(), paths.receipts().toString(),
                Boolean.toString(offline),
                Boolean.toString(autoStart), Boolean.toString(preferSystemTools),
                Boolean.toString(reuseOwnedProcesses), startupTimeout.toString(), shutdownTimeout.toString(),
                remoteEndpoint.map(URI::toString).orElse(""));
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            for (int index = 0; index < value.length(); index++) {
                char codeUnit = value.charAt(index);
                digest.update((byte) (codeUnit >>> 8));
                digest.update((byte) codeUnit);
            }
            return "sha256:" + HexFormat.of().formatHex(digest.digest());
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
    }

    private SetupOptions copy(SetupMode nextMode, boolean nextOffline, boolean nextAutoStart,
                              boolean nextPreferSystemTools, boolean nextReuseOwnedProcesses,
                              Duration nextStartupTimeout, Duration nextShutdownTimeout,
                              Optional<URI> nextRemoteEndpoint) {
        return new SetupOptions(profile, nextMode, paths, nextOffline, nextAutoStart,
                nextPreferSystemTools, nextReuseOwnedProcesses, nextStartupTimeout,
                nextShutdownTimeout, nextRemoteEndpoint);
    }

    private static Duration positive(Duration value, String name) {
        Objects.requireNonNull(value, name);
        if (value.isZero() || value.isNegative()) throw new IllegalArgumentException(name + " must be positive.");
        return value;
    }

    private static void validateRemoteEndpoint(URI endpoint) {
        if (!endpoint.isAbsolute() || endpoint.getHost() == null
                || !("http".equalsIgnoreCase(endpoint.getScheme())
                || "https".equalsIgnoreCase(endpoint.getScheme()))) {
            throw new IllegalArgumentException("remoteEndpoint must be an absolute HTTP(S) URI.");
        }
    }
}
