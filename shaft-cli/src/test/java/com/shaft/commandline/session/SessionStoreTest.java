package com.shaft.commandline.session;

import com.shaft.commandline.mcp.McpException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for {@link SessionStore}: round-trip persistence, corrupt-file tolerance, and path
 * resolution.
 */
class SessionStoreTest {

    @Test
    void pathResolvesUnderBaseDir(@TempDir Path baseDir) {
        SessionStore store = new SessionStore(baseDir);

        assertEquals(baseDir.resolve("cli-session.json"), store.path());
    }

    @Test
    void loadReturnsEmptyWhenNoFileExists(@TempDir Path baseDir) {
        SessionStore store = new SessionStore(baseDir);

        assertTrue(store.load().isEmpty());
    }

    @Test
    void saveThenLoadRoundTripsSessionInfo(@TempDir Path baseDir) {
        SessionStore store = new SessionStore(baseDir);
        SessionInfo original = new SessionInfo(8081, 12345L, "/path/to/shaft-mcp.jar", "2026-07-12T00:00:00Z");

        store.save(original);
        Optional<SessionInfo> loaded = store.load();

        assertTrue(loaded.isPresent());
        assertEquals(original, loaded.get());
        assertTrue(Files.isRegularFile(store.path()));
    }

    @Test
    void clearDeletesTheSessionFile(@TempDir Path baseDir) {
        SessionStore store = new SessionStore(baseDir);
        store.save(new SessionInfo(8081, 12345L, "/path/to/shaft-mcp.jar", "2026-07-12T00:00:00Z"));

        store.clear();

        assertFalse(Files.exists(store.path()));
        assertTrue(store.load().isEmpty());
    }

    @Test
    void clearOnMissingFileDoesNotThrow(@TempDir Path baseDir) {
        SessionStore store = new SessionStore(baseDir);

        assertDoesNotThrow(store::clear);
    }

    @Test
    void loadToleratesCorruptFile(@TempDir Path baseDir) throws IOException {
        SessionStore store = new SessionStore(baseDir);
        Files.createDirectories(baseDir);
        Files.writeString(store.path(), "{ not valid json ", StandardCharsets.UTF_8);

        assertTrue(store.load().isEmpty());
    }

    @Test
    void loadToleratesEmptyFile(@TempDir Path baseDir) throws IOException {
        SessionStore store = new SessionStore(baseDir);
        Files.createDirectories(baseDir);
        Files.writeString(store.path(), "", StandardCharsets.UTF_8);

        assertTrue(store.load().isEmpty());
    }

    @Test
    void saveOverwritesPreviousSession(@TempDir Path baseDir) {
        SessionStore store = new SessionStore(baseDir);
        store.save(new SessionInfo(8081, 111L, "/a.jar", "2026-07-12T00:00:00Z"));

        SessionInfo replacement = new SessionInfo(9090, 222L, "/b.jar", "2026-07-12T01:00:00Z");
        store.save(replacement);

        assertEquals(Optional.of(replacement), store.load());
    }

    @Test
    void saveRejectsAFileUsedAsTheSessionDirectory(@TempDir Path temp) throws IOException {
        Path file = temp.resolve("not-a-directory");
        Files.writeString(file, "x");
        SessionStore store = new SessionStore(file);

        McpException failure = assertThrows(McpException.class,
                () -> store.save(new SessionInfo(1, 2L, "jar", "2026-11-11T00:00:00Z")));

        assertTrue(failure.getMessage().contains("Could not create session directory"), failure.getMessage());
        assertTrue(failure.getMessage().contains(file.toString()), failure.getMessage());
    }

    @Test
    void saveRejectsWhenTheSessionPathIsADirectory(@TempDir Path baseDir) throws IOException {
        SessionStore store = new SessionStore(baseDir);
        Files.createDirectories(store.path());

        McpException failure = assertThrows(McpException.class,
                () -> store.save(new SessionInfo(1, 2L, "jar", "2026-11-11T00:00:00Z")));

        assertTrue(failure.getMessage().contains("Could not write session file"), failure.getMessage());
        assertTrue(failure.getMessage().contains(store.path().toString()), failure.getMessage());
    }

    @Test
    void clearRejectsANonEmptyDirectoryAtTheSessionPath(@TempDir Path baseDir) throws IOException {
        SessionStore store = new SessionStore(baseDir);
        Files.createDirectories(store.path());
        Files.writeString(store.path().resolve("child"), "x");

        McpException failure = assertThrows(McpException.class, store::clear);

        assertTrue(failure.getMessage().contains("Could not remove session file"), failure.getMessage());
        assertTrue(failure.getMessage().contains(store.path().toString()), failure.getMessage());
    }
}
