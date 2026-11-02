package com.shaft.commandline.session;

import com.shaft.commandline.mcp.McpException;
import com.shaft.commandline.util.Json;

import java.io.IOException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Optional;

/**
 * Persists the current {@link SessionInfo} as JSON at {@code <baseDir>/cli-session.json}.
 */
public final class SessionStore {

    private static final String FILE_NAME = "cli-session.json";

    private final Path baseDir;

    /**
     * Creates a store rooted at {@code <user.home>/.shaft}.
     */
    public SessionStore() {
        this(Path.of(System.getProperty("user.home"), ".shaft"));
    }

    /**
     * Creates a store rooted at the given directory, for tests.
     *
     * @param baseDir the directory the session file lives in
     */
    public SessionStore(Path baseDir) {
        this.baseDir = baseDir;
    }

    /**
     * @return the path to the session file, whether or not it currently exists
     */
    public Path path() {
        return baseDir.resolve(FILE_NAME);
    }

    /**
     * Loads the persisted session, tolerating a missing or corrupt file.
     *
     * @return the persisted session, or empty if none is stored or it cannot be parsed
     */
    public Optional<SessionInfo> load() {
        Path file = path();
        if (!Files.isRegularFile(file)) {
            return Optional.empty();
        }
        try {
            return Optional.ofNullable(Json.MAPPER.readValue(file.toFile(), SessionInfo.class));
        } catch (RuntimeException exception) {
            return Optional.empty();
        }
    }

    /**
     * Persists the session, writing atomically via a temp file plus move-replace.
     *
     * @param info the session to persist
     */
    public void save(SessionInfo info) {
        try {
            Files.createDirectories(baseDir);
        } catch (IOException exception) {
            throw new McpException("Could not create session directory '" + baseDir + "': "
                    + exception.getMessage(), exception);
        }
        Path destination = path();
        Path temporary = baseDir.resolve("." + FILE_NAME + "." + ProcessHandle.current().pid() + ".tmp");
        try {
            Json.MAPPER.writeValue(temporary.toFile(), info);
            moveReplacing(temporary, destination);
        } catch (RuntimeException | IOException exception) {
            throw new McpException("Could not write session file '" + destination + "': "
                    + exception.getMessage(), exception);
        } finally {
            try {
                Files.deleteIfExists(temporary);
            } catch (IOException ignored) {
                // Best-effort cleanup; the move already published or failed above.
            }
        }
    }

    /**
     * Deletes the session file if present.
     */
    public void clear() {
        try {
            Files.deleteIfExists(path());
        } catch (IOException exception) {
            throw new McpException("Could not remove session file '" + path() + "': "
                    + exception.getMessage(), exception);
        }
    }

    private static void moveReplacing(Path temporary, Path destination) throws IOException {
        try {
            Files.move(temporary, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
        } catch (AtomicMoveNotSupportedException exception) {
            Files.move(temporary, destination, StandardCopyOption.REPLACE_EXISTING);
        }
    }
}
