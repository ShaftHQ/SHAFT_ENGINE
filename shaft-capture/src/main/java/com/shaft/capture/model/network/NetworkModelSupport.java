package com.shaft.capture.model.network;

import java.util.Objects;

/**
 * Small validation helpers mirroring {@code com.shaft.capture.model.ModelSupport}
 * for the network record package, which cannot see that package-private type.
 */
final class NetworkModelSupport {
    private NetworkModelSupport() {
        throw new IllegalStateException("Utility class");
    }

    static String requireText(String value, String field) {
        String normalized = Objects.requireNonNullElse(value, "").trim();
        if (normalized.isEmpty()) {
            throw new IllegalArgumentException(field + " is required.");
        }
        return normalized;
    }

    static String text(String value) {
        return Objects.requireNonNullElse(value, "").trim();
    }
}
