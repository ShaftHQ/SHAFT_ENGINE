package com.shaft.ai.agentic;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.Locale;
import java.util.Objects;

/**
 * Deterministic seed state bound to every phase (FR-2).
 *
 * @param journeyId stable journey identifier
 * @param journeyText user journey description
 * @param pageContent untrusted page content captured for the fixture (may be empty)
 * @param seedHash SHA-256 of the canonical seed material
 */
public record AgenticSeed(String journeyId, String journeyText, String pageContent, String seedHash) {
    /**
     * Creates a validated seed. Blank journey id or text fails closed.
     */
    public AgenticSeed {
        journeyId = requireText(journeyId, "journeyId");
        journeyText = requireText(journeyText, "journeyText");
        pageContent = pageContent == null ? "" : pageContent;
        seedHash = requireText(seedHash, "seedHash").toLowerCase(Locale.ROOT);
    }

    /**
     * Builds a seed and computes its hash from canonical material.
     *
     * @param journeyId journey id
     * @param journeyText journey text
     * @param pageContent untrusted page content
     * @return deterministic seed
     */
    public static AgenticSeed of(String journeyId, String journeyText, String pageContent) {
        String id = requireText(journeyId, "journeyId");
        String text = requireText(journeyText, "journeyText");
        String page = pageContent == null ? "" : pageContent;
        return new AgenticSeed(id, text, page, hash(id, text, page));
    }

    /**
     * Verifies that a candidate hash matches this seed.
     *
     * @param candidateHash hash to compare
     * @return {@code true} when equal
     */
    public boolean matches(String candidateHash) {
        return seedHash.equalsIgnoreCase(Objects.requireNonNullElse(candidateHash, ""));
    }

    /**
     * Recomputes the hash from current fields and compares to {@link #seedHash()}.
     *
     * @return {@code true} when the seed is internally consistent
     */
    public boolean isConsistent() {
        return seedHash.equals(hash(journeyId, journeyText, pageContent));
    }

    static String hash(String journeyId, String journeyText, String pageContent) {
        String canonical = journeyId + '\n' + journeyText + '\n' + pageContent;
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hashed = digest.digest(canonical.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(hashed);
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 unavailable", exception);
        }
    }

    private static String requireText(String value, String label) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(label + " is required.");
        }
        return value.trim();
    }
}
