package com.shaft.capture.storage;

import com.shaft.capture.model.network.BodyRef;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for NetworkBodyStore.
 */
class NetworkBodyStoreTest {
    private NetworkBodyStore store;

    @TempDir
    Path sessionDir;

    @BeforeEach
    void setUp() {
        store = new NetworkBodyStore();
    }

    @Test
    void testStoreNullBody_ReturnsNull() {
        BodyRef result = store.store(null, "application/json", sessionDir);
        assertNull(result, "Null body should produce null BodyRef");
    }

    @Test
    void testStoreEmptyBody_ReturnsNull() {
        BodyRef result = store.store(new byte[0], "application/json", sessionDir);
        assertNull(result, "Empty body should produce null BodyRef");
    }

    @Test
    void testStoreValidBody_ComputesSha256AndSize() {
        byte[] body = "test body content".getBytes(StandardCharsets.UTF_8);
        BodyRef result = store.store(body, "text/plain", sessionDir);

        assertNotNull(result, "BodyRef should not be null for non-empty body");
        assertNotNull(result.sha256(), "SHA-256 should be computed");
        assertFalse(result.sha256().isEmpty(), "SHA-256 should not be empty");
        assertEquals(body.length, result.sizeBytes(), "Size should match original body length");
        assertFalse(result.truncated(), "Should not be truncated when under cap");
    }

    @Test
    void testStoreTruncatedBody_FlagsAndLimitsStoredSize() throws Exception {
        byte[] body = new byte[2000000]; // 2 MiB
        for (int i = 0; i < body.length; i++) {
            body[i] = (byte) (i % 256);
        }

        int maxBodyBytes = 1000000; // 1 MiB cap
        BodyRef result = store.store(body, "application/octet-stream", sessionDir, maxBodyBytes);

        assertNotNull(result, "BodyRef should not be null");
        assertEquals(2000000L, result.sizeBytes(), "Original size should be recorded");
        assertTrue(result.truncated(), "Should be flagged as truncated");

        // Verify stored file is capped
        Path storedFile = sessionDir.resolve(result.sha256() + ".bin");
        assertTrue(Files.exists(storedFile), "Stored file should exist");
        long storedSize = Files.size(storedFile);
        assertEquals(maxBodyBytes, storedSize, "Stored file should be capped at maxBodyBytes");
    }

    @Test
    void testStoredFileSha256Matches_Original() throws Exception {
        byte[] body = "deterministic test".getBytes(StandardCharsets.UTF_8);
        BodyRef result1 = store.store(body, "text/plain", sessionDir, 1000);

        // Store same body again with different cap (should produce same SHA-256)
        BodyRef result2 = store.store(body, "text/plain", sessionDir, 2000);

        assertEquals(result1.sha256(), result2.sha256(), "SHA-256 should be deterministic for same body");
    }

    @Test
    void testStoreWritesAtomicallyWithTempFile() throws Exception {
        byte[] body = "atomic write test".getBytes(StandardCharsets.UTF_8);
        BodyRef result = store.store(body, "text/plain", sessionDir);

        assertNotNull(result, "BodyRef should be created");

        // Verify the file exists at the expected location
        Path storedFile = sessionDir.resolve(result.sha256() + ".bin");
        assertTrue(Files.exists(storedFile), "Stored file should exist");

        // Verify no temp files are left behind
        long tempFileCount = Files.list(sessionDir)
                .filter(p -> p.getFileName().toString().endsWith(".tmp"))
                .count();
        assertEquals(0, tempFileCount, "No temporary files should be left behind");
    }

    @Test
    void testStoreWithDefaultMaxBodyBytes() {
        byte[] body = "short content".getBytes(StandardCharsets.UTF_8);
        BodyRef result = store.store(body, "text/plain", sessionDir);

        assertNotNull(result, "BodyRef should be created with default cap");
        assertEquals(body.length, result.sizeBytes(), "Size should match");
        assertFalse(result.truncated(), "Should not be truncated with default cap");
    }

    @Test
    void testStoredFileIsActuallyTruncated_NotSymlinked() throws Exception {
        byte[] body = new byte[2000];
        for (int i = 0; i < body.length; i++) {
            body[i] = (byte) i;
        }

        int maxBodyBytes = 1000;
        BodyRef result = store.store(body, "application/octet-stream", sessionDir, maxBodyBytes);

        Path storedFile = sessionDir.resolve(result.sha256() + ".bin");
        byte[] storedContent = Files.readAllBytes(storedFile);

        assertEquals(maxBodyBytes, storedContent.length, "Stored content should be truncated");
        for (int i = 0; i < maxBodyBytes; i++) {
            assertEquals((byte) i, storedContent[i], "Stored content should match truncated original");
        }
    }
}
