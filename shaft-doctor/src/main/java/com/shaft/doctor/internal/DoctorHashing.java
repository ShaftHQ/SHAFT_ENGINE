package com.shaft.doctor.internal;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

/**
 * SHA-256 helpers for stable identifiers and integrity metadata.
 */
public final class DoctorHashing {
    private DoctorHashing() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Computes a lowercase SHA-256 digest.
     *
     * @param bytes bytes to hash
     * @return hexadecimal digest
     */
    public static String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
        }
    }
}
