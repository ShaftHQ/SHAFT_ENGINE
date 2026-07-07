package com.shaft.capture.network;

import com.shaft.capture.format.CaptureFormatException;
import com.shaft.capture.storage.OwnerOnlyFilePermissions;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * Replaces sensitive header values with deterministic secret-ref tokens,
 * optionally storing raw values in a git-ignored .secrets.json file with owner-only permissions.
 */
public final class SecretHeaderReplacer {
    private static final HexFormat HEX = HexFormat.of();
    private static final ObjectMapper MAPPER = JsonMapper.builder().build();

    /**
     * Headers that are known to contain sensitive data and should be replaced.
     */
    private static final Set<String> SENSITIVE_HEADERS = Set.of(
            "Authorization",
            "Proxy-Authorization",
            "Cookie",
            "Set-Cookie",
            "X-Api-Key"
    );

    /**
     * Replaces every sensitive header value with a secret-ref token.
     * The raw value is never present in the returned map.
     * ENV_NAME derivation is deterministic and collision-safe for the same session.
     *
     * @param headers original headers map (may be null)
     * @param sessionId unique session identifier for collision-safe ENV_NAME derivation
     * @return new map with sensitive values replaced by secret-ref tokens, or null if input is null
     */
    public Map<String, String> replaceSecrets(Map<String, String> headers, String sessionId) {
        if (headers == null) {
            return null;
        }

        Map<String, String> replaced = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

        for (Map.Entry<String, String> entry : headers.entrySet()) {
            String headerName = entry.getKey();
            String headerValue = entry.getValue();

            if (isSensitive(headerName) && headerValue != null && !headerValue.isEmpty()) {
                // Generate deterministic ENV_NAME from header name, value, and session ID
                String envName = deriveDeterministicEnvName(headerName, headerValue, sessionId);
                replaced.put(headerName, "secret-ref:" + envName);
            } else {
                replaced.put(headerName, headerValue);
            }
        }

        return replaced;
    }

    /**
     * Replaces sensitive headers and optionally writes raw secrets to .secrets.json.
     *
     * @param headers original headers map (may be null)
     * @param sessionId unique session identifier for collision-safe ENV_NAME derivation
     * @param sessionDir directory where .secrets.json will be written
     * @param storeSecretsLocally if true, write raw secrets to .secrets.json with owner-only permissions
     * @return new map with sensitive values replaced by secret-ref tokens, or null if input is null
     * @throws CaptureFormatException if secrets file write fails
     */
    public Map<String, String> replaceSecrets(Map<String, String> headers, String sessionId,
                                               Path sessionDir, boolean storeSecretsLocally) {
        Map<String, String> replaced = replaceSecrets(headers, sessionId);

        if (storeSecretsLocally && headers != null) {
            Map<String, String> secrets = new HashMap<>();

            for (Map.Entry<String, String> entry : headers.entrySet()) {
                String headerName = entry.getKey();
                String headerValue = entry.getValue();

                if (isSensitive(headerName) && headerValue != null && !headerValue.isEmpty()) {
                    String envName = deriveDeterministicEnvName(headerName, headerValue, sessionId);
                    secrets.put(envName, headerValue);
                }
            }

            if (!secrets.isEmpty()) {
                writeSecretsFile(sessionDir, secrets);
            }
        }

        return replaced;
    }

    /**
     * Checks if a header name is known to be sensitive.
     *
     * @param headerName the header name
     * @return true if the header is sensitive, false otherwise
     */
    private boolean isSensitive(String headerName) {
        if (headerName == null) {
            return false;
        }
        // Check against known sensitive headers (case-insensitive)
        for (String sensitive : SENSITIVE_HEADERS) {
            if (sensitive.equalsIgnoreCase(headerName)) {
                return true;
            }
        }
        // Heuristic: headers ending with -key or containing api-key (case-insensitive)
        String lower = headerName.toLowerCase(Locale.ENGLISH);
        return lower.endsWith("-key") || lower.contains("api-key") || lower.contains("x-api-key");
    }

    /**
     * Derives a deterministic, collision-safe ENV_NAME from header name, value, and session ID.
     * Format: {HEADER_NAME}_{SESSION_HASH}_{VALUE_HASH}
     *
     * @param headerName the header name
     * @param headerValue the header value
     * @param sessionId the session identifier
     * @return deterministic ENV_NAME
     */
    private String deriveDeterministicEnvName(String headerName, String headerValue, String sessionId) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");

            // Hash session ID
            String sessionHash = hashToShort(sessionId, digest);

            // Hash header value
            String valueHash = hashToShort(headerValue, digest);

            // Normalize header name for ENV format (uppercase, replace non-alphanumeric with underscore)
            String normalizedHeader = headerName.toUpperCase(Locale.ENGLISH)
                    .replaceAll("[^A-Z0-9]", "_");

            return normalizedHeader + "_" + sessionHash + "_" + valueHash;
        } catch (NoSuchAlgorithmException e) {
            // Fallback: use a simple hash combination
            throw new CaptureFormatException("SHA-256 not available for ENV_NAME derivation.", e);
        }
    }

    /**
     * Computes a short hash (first 8 hex chars) from a string.
     *
     * @param input string to hash
     * @param digest MessageDigest instance
     * @return first 8 characters of the hex hash
     */
    private String hashToShort(String input, MessageDigest digest) {
        digest.reset();
        digest.update(input.getBytes(StandardCharsets.UTF_8));
        String hex = HEX.formatHex(digest.digest());
        return hex.substring(0, Math.min(8, hex.length()));
    }

    /**
     * Writes raw secret values to a .secrets.json file with owner-only permissions.
     * Supports POSIX permissions (0600) where available, falls back to Windows owner-only ACL.
     *
     * @param sessionDir directory where .secrets.json will be written
     * @param secrets map of ENV_NAME to raw secret value
     * @throws CaptureFormatException if write fails
     */
    private void writeSecretsFile(Path sessionDir, Map<String, String> secrets) {
        try {
            Path secretsPath = sessionDir.resolve(".secrets.json");
            String json = MAPPER.writerWithDefaultPrettyPrinter()
                    .writeValueAsString(new TreeMap<>(secrets)) + "\n";
            atomicWrite(secretsPath, json);
            OwnerOnlyFilePermissions.restrictToOwner(secretsPath);
        } catch (IOException exception) {
            throw new CaptureFormatException("Secrets file could not be written.", exception);
        }
    }

    /**
     * Atomically writes a string to a file using temp file + move pattern.
     *
     * @param destination target file path
     * @param content string content to write
     * @throws IOException if write fails
     */
    private static void atomicWrite(Path destination, String content) throws IOException {
        Path absolute = destination.toAbsolutePath().normalize();
        Path temporary = null;
        try {
            Files.createDirectories(absolute.getParent());
            temporary = Files.createTempFile(absolute.getParent(), "." + absolute.getFileName(), ".tmp");
            Files.writeString(temporary, content, StandardCharsets.UTF_8);
            moveReplacing(temporary, absolute);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup
                }
            }
        }
    }

    /**
     * Moves a file atomically if supported, with fallback retries on access denied.
     *
     * @param temporary source file
     * @param destination target file
     * @throws IOException if move fails
     */
    private static void moveReplacing(Path temporary, Path destination) throws IOException {
        boolean atomic = true;
        for (int attempt = 1; attempt <= 50; attempt++) {
            try {
                if (atomic) {
                    Files.move(temporary, destination, StandardCopyOption.ATOMIC_MOVE,
                            StandardCopyOption.REPLACE_EXISTING);
                } else {
                    Files.move(temporary, destination, StandardCopyOption.REPLACE_EXISTING);
                }
                return;
            } catch (AtomicMoveNotSupportedException ignored) {
                atomic = false;
            } catch (AccessDeniedException exception) {
                if (attempt == 50) {
                    throw exception;
                }
                try {
                    Thread.sleep(10);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while publishing secrets file.", interrupted);
                }
            }
        }
    }

}
