package com.shaft.capture.control;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.shaft.capture.runtime.CaptureStartRequest;
import com.shaft.capture.runtime.CaptureStatus;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Set;

/**
 * Atomic local state files used by the detached capture process.
 */
public final class CaptureControlFiles {
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

    private final Path runtimeDirectory;

    /**
     * Creates local control files under a runtime directory.
     *
     * @param runtimeDirectory local runtime directory
     */
    public CaptureControlFiles(Path runtimeDirectory) {
        if (runtimeDirectory == null) {
            throw new IllegalArgumentException("Capture runtime directory is required.");
        }
        this.runtimeDirectory = runtimeDirectory.toAbsolutePath().normalize();
    }

    /**
     * Returns the local runtime directory.
     *
     * @return runtime directory
     */
    public Path runtimeDirectory() {
        return runtimeDirectory;
    }

    /**
     * Writes the safe status snapshot.
     *
     * @param status safe status
     */
    public void writeStatus(CaptureStatus status) {
        writeJson(statusPath(), status, false);
    }

    /**
     * Reads the safe status snapshot.
     *
     * @return latest status or not-running status
     */
    public CaptureStatus readStatus() {
        if (!Files.isRegularFile(statusPath())) {
            return CaptureStatus.notRunning();
        }
        return readJson(statusPath(), CaptureStatus.class);
    }

    /**
     * Writes loopback control metadata.
     *
     * @param descriptor control descriptor
     */
    public void writeDescriptor(ControlDescriptor descriptor) {
        writeJson(descriptorPath(), descriptor, false);
    }

    /**
     * Reads loopback control metadata.
     *
     * @return descriptor
     */
    public ControlDescriptor readDescriptor() {
        return readJson(descriptorPath(), ControlDescriptor.class);
    }

    /**
     * Writes the per-session authorization token with owner-only permissions where supported.
     *
     * @param token authorization token
     */
    public void writeToken(String token) {
        writeText(tokenPath(), token == null ? "" : token, true);
    }

    /**
     * Reads the per-session authorization token.
     *
     * @return token
     */
    public String readToken() {
        try {
            return Files.readString(tokenPath(), StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT Capture authorization token is unavailable.", exception);
        }
    }

    /**
     * Writes a one-time daemon launch request.
     *
     * @param request start request
     * @return request file
     */
    public Path writeLaunchRequest(CaptureStartRequest request) {
        Path path = runtimeDirectory.resolve("launch-" + java.util.UUID.randomUUID() + ".json");
        writeJson(path, new LaunchRequest(
                request.targetUrl(),
                request.browser().name(),
                request.outputPath().toString(),
                request.runtimeDirectory().toString(),
                request.headless()), true);
        return path;
    }

    /**
     * Reads and removes a one-time daemon launch request.
     *
     * @param path request file
     * @return request
     */
    public CaptureStartRequest consumeLaunchRequest(Path path) {
        Path normalized = path.toAbsolutePath().normalize();
        if (!normalized.startsWith(runtimeDirectory) || !normalized.getFileName().toString().startsWith("launch-")) {
            throw new IllegalArgumentException("Capture launch request is outside the runtime directory.");
        }
        try {
            LaunchRequest request = readJson(normalized, LaunchRequest.class);
            return new CaptureStartRequest(
                    request.targetUrl(),
                    com.shaft.capture.runtime.CaptureBrowser.parse(request.browser()),
                    Path.of(request.outputPath()),
                    Path.of(request.runtimeDirectory()),
                    request.headless());
        } finally {
            try {
                Files.deleteIfExists(normalized);
            } catch (IOException ignored) {
                // The consumed request contains no reusable credential.
            }
        }
    }

    /**
     * Removes active control metadata while preserving final status.
     */
    public void clearActiveControl() {
        try {
            Files.deleteIfExists(descriptorPath());
            Files.deleteIfExists(tokenPath());
        } catch (IOException ignored) {
            // Stale metadata is rejected by PID and endpoint checks.
        }
    }

    /**
     * Returns whether active control metadata exists.
     *
     * @return true when descriptor and token are present
     */
    public boolean hasActiveControl() {
        return Files.isRegularFile(descriptorPath()) && Files.isRegularFile(tokenPath());
    }

    private Path statusPath() {
        return runtimeDirectory.resolve("status.json");
    }

    private Path descriptorPath() {
        return runtimeDirectory.resolve("control.json");
    }

    private Path tokenPath() {
        return runtimeDirectory.resolve("control.token");
    }

    private static <T> T readJson(Path path, Class<T> type) {
        try {
            return MAPPER.readValue(Files.readString(path, StandardCharsets.UTF_8), type);
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT Capture local control state is unreadable.", exception);
        }
    }

    private static void writeJson(Path path, Object value, boolean restricted) {
        try {
            writeText(path, MAPPER.writeValueAsString(value) + "\n", restricted);
        } catch (JsonProcessingException exception) {
            throw new IllegalStateException("SHAFT Capture local control state could not be serialized.", exception);
        }
    }

    private static void writeText(Path path, String value, boolean restricted) {
        Path temporary = null;
        try {
            Files.createDirectories(path.getParent());
            temporary = Files.createTempFile(path.getParent(), "." + path.getFileName(), ".tmp");
            if (restricted) {
                restrict(temporary);
            }
            Files.writeString(temporary, value, StandardCharsets.UTF_8);
            try {
                Files.move(temporary, path, StandardCopyOption.ATOMIC_MOVE,
                        StandardCopyOption.REPLACE_EXISTING);
            } catch (AtomicMoveNotSupportedException ignored) {
                Files.move(temporary, path, StandardCopyOption.REPLACE_EXISTING);
            }
            if (restricted) {
                restrict(path);
            }
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT Capture local control state could not be written.", exception);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup of an unpublished temporary state file.
                }
            }
        }
    }

    private static void restrict(Path path) {
        try {
            Files.setPosixFilePermissions(path, Set.of(
                    PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE));
        } catch (UnsupportedOperationException | IOException ignored) {
            // Non-POSIX filesystems use the runtime directory's inherited permissions.
        }
    }

    /**
     * Safe loopback endpoint metadata.
     *
     * @param port loopback TCP port
     * @param processId owning process ID
     */
    public record ControlDescriptor(int port, long processId) {
        /**
         * Creates validated endpoint metadata.
         */
        public ControlDescriptor {
            if (port < 1 || port > 65535 || processId < 1) {
                throw new IllegalArgumentException("Capture control endpoint metadata is invalid.");
            }
        }
    }

    private record LaunchRequest(
            String targetUrl,
            String browser,
            String outputPath,
            String runtimeDirectory,
            boolean headless) {
    }
}
