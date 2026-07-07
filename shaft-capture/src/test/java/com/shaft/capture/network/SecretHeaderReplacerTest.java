package com.shaft.capture.network;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for SecretHeaderReplacer.
 */
class SecretHeaderReplacerTest {
    private SecretHeaderReplacer replacer;
    private ObjectMapper mapper;

    @TempDir
    Path sessionDir;

    @BeforeEach
    void setUp() {
        replacer = new SecretHeaderReplacer();
        mapper = JsonMapper.builder().build();
    }

    @Test
    void testReplaceSecrets_NullHeaders_ReturnsNull() {
        Map<String, String> result = replacer.replaceSecrets(null, "session123");
        assertNull(result, "Null headers should produce null result");
    }

    @Test
    void testReplaceSecrets_AuthorizationHeader() {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer abc123token");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1");

        assertNotNull(result, "Result should not be null");
        assertTrue(result.get("Authorization").startsWith("secret-ref:"),
                "Authorization value should be replaced with secret-ref token");
        assertFalse(result.get("Authorization").contains("abc123token"),
                "Raw token should not appear in replaced map");
    }

    @Test
    void testReplaceSecrets_CookieHeader() {
        Map<String, String> headers = new HashMap<>();
        headers.put("Cookie", "sessionId=xyz789abc");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1");

        assertNotNull(result, "Result should not be null");
        assertTrue(result.get("Cookie").startsWith("secret-ref:"),
                "Cookie value should be replaced with secret-ref token");
        assertFalse(result.get("Cookie").contains("xyz789abc"),
                "Raw cookie value should not appear in replaced map");
    }

    @Test
    void testReplaceSecrets_CustomApiKeyHeader() {
        Map<String, String> headers = new HashMap<>();
        headers.put("X-Api-Key", "secret-api-key-value");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1");

        assertNotNull(result, "Result should not be null");
        assertTrue(result.get("X-Api-Key").startsWith("secret-ref:"),
                "X-Api-Key value should be replaced with secret-ref token");
        assertFalse(result.get("X-Api-Key").contains("secret-api-key-value"),
                "Raw API key should not appear in replaced map");
    }

    @Test
    void testReplaceSecrets_NonSensitiveHeaders_Unchanged() {
        Map<String, String> headers = new HashMap<>();
        headers.put("Content-Type", "application/json");
        headers.put("User-Agent", "TestClient/1.0");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1");

        assertNotNull(result, "Result should not be null");
        assertEquals("application/json", result.get("Content-Type"),
                "Non-sensitive headers should not be replaced");
        assertEquals("TestClient/1.0", result.get("User-Agent"),
                "Non-sensitive headers should not be replaced");
    }

    @Test
    void testReplaceSecrets_MixedHeaders() {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token123");
        headers.put("Content-Type", "application/json");
        headers.put("Cookie", "session=abc");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1");

        assertNotNull(result, "Result should not be null");
        assertTrue(result.get("Authorization").startsWith("secret-ref:"),
                "Sensitive header should be replaced");
        assertEquals("application/json", result.get("Content-Type"),
                "Non-sensitive header should remain");
        assertTrue(result.get("Cookie").startsWith("secret-ref:"),
                "Sensitive header should be replaced");
    }

    @Test
    void testReplaceSecrets_DeterministicEnvName() {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token123");

        Map<String, String> result1 = replacer.replaceSecrets(headers, "session1");
        Map<String, String> result2 = replacer.replaceSecrets(headers, "session1");

        assertEquals(result1.get("Authorization"), result2.get("Authorization"),
                "ENV_NAME should be deterministic for same header, value, and session");
    }

    @Test
    void testReplaceSecrets_DifferentSessions_DifferentEnvNames() {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token123");

        Map<String, String> result1 = replacer.replaceSecrets(headers, "session1");
        Map<String, String> result2 = replacer.replaceSecrets(headers, "session2");

        assertNotNull(result1.get("Authorization"), "Result 1 should have replaced value");
        assertNotNull(result2.get("Authorization"), "Result 2 should have replaced value");
        assertFalse(result1.get("Authorization").equals(result2.get("Authorization")),
                "Different sessions should produce different ENV_NAMEs");
    }

    @Test
    void testReplaceSecrets_EmptyHeaderValue_NotReplaced() {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1");

        assertEquals("", result.get("Authorization"),
                "Empty sensitive header values should not be replaced");
    }

    @Test
    void testReplaceSecretsWithStorage_NoSecretsFile_WhenDisabled() throws Exception {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token123");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1", sessionDir, false);

        assertNotNull(result, "Result should not be null");
        assertTrue(result.get("Authorization").startsWith("secret-ref:"),
                "Authorization should be replaced");

        Path secretsPath = sessionDir.resolve(".secrets.json");
        assertFalse(Files.exists(secretsPath),
                "Secrets file should NOT be created when storeSecretsLocally=false");
    }

    @Test
    void testReplaceSecretsWithStorage_WritesSecretsFile_WhenEnabled() throws Exception {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token123");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1", sessionDir, true);

        assertNotNull(result, "Result should not be null");
        assertTrue(result.get("Authorization").startsWith("secret-ref:"),
                "Authorization should be replaced");

        Path secretsPath = sessionDir.resolve(".secrets.json");
        assertTrue(Files.exists(secretsPath),
                "Secrets file should be created when storeSecretsLocally=true");

        // Verify file content
        Map<String, String> secretsContent = mapper.readValue(Files.readString(secretsPath),
                mapper.getTypeFactory().constructMapType(Map.class, String.class, String.class));
        assertFalse(secretsContent.isEmpty(), "Secrets file should contain entries");
    }

    @Test
    void testReplaceSecretsWithStorage_NoSecretsFile_WhenNoSensitiveHeaders() throws Exception {
        Map<String, String> headers = new HashMap<>();
        headers.put("Content-Type", "application/json");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1", sessionDir, true);

        assertNotNull(result, "Result should not be null");
        assertEquals("application/json", result.get("Content-Type"));

        Path secretsPath = sessionDir.resolve(".secrets.json");
        assertFalse(Files.exists(secretsPath),
                "Secrets file should not be created when there are no sensitive headers");
    }

    @Test
    void testReplaceSecretsWithStorage_OwnerOnlyPermissions() throws Exception {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer secret123");

        replacer.replaceSecrets(headers, "session1", sessionDir, true);

        Path secretsPath = sessionDir.resolve(".secrets.json");
        assertTrue(Files.exists(secretsPath), "Secrets file should exist");

        try {
            // Try to read POSIX permissions
            Set<PosixFilePermission> perms = Files.getPosixFilePermissions(secretsPath);
            // Verify owner-only (should have OWNER_READ and OWNER_WRITE, nothing else)
            assertTrue(perms.contains(PosixFilePermission.OWNER_READ),
                    "Owner should have read permission");
            assertTrue(perms.contains(PosixFilePermission.OWNER_WRITE),
                    "Owner should have write permission");
            assertFalse(perms.contains(PosixFilePermission.GROUP_READ),
                    "Group should not have read permission");
            assertFalse(perms.contains(PosixFilePermission.OTHERS_READ),
                    "Others should not have read permission");
        } catch (UnsupportedOperationException ignored) {
            // Windows does not support POSIX permissions; test passes (ACL fallback)
        }
    }

    @Test
    void testReplaceSecretsWithStorage_MultipleHeaders() throws Exception {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token1");
        headers.put("Cookie", "session=abc123");
        headers.put("X-Api-Key", "key-secret");
        headers.put("Content-Type", "application/json");

        Map<String, String> result = replacer.replaceSecrets(headers, "session1", sessionDir, true);

        assertNotNull(result, "Result should not be null");
        assertTrue(result.get("Authorization").startsWith("secret-ref:"),
                "Authorization should be replaced");
        assertTrue(result.get("Cookie").startsWith("secret-ref:"),
                "Cookie should be replaced");
        assertTrue(result.get("X-Api-Key").startsWith("secret-ref:"),
                "X-Api-Key should be replaced");
        assertEquals("application/json", result.get("Content-Type"),
                "Content-Type should not be replaced");

        Path secretsPath = sessionDir.resolve(".secrets.json");
        assertTrue(Files.exists(secretsPath), "Secrets file should exist");

        @SuppressWarnings("unchecked")
        Map<String, String> secretsContent = mapper.readValue(Files.readString(secretsPath),
                mapper.getTypeFactory().constructMapType(Map.class, String.class, String.class));

        assertEquals(3, secretsContent.size(), "Secrets file should contain 3 entries");
        assertTrue(secretsContent.containsValue("Bearer token1"),
                "Bearer token should be in secrets file");
        assertTrue(secretsContent.containsValue("session=abc123"),
                "Cookie should be in secrets file");
        assertTrue(secretsContent.containsValue("key-secret"),
                "API key should be in secrets file");
    }
}
