package com.shaft.fuzz;

import com.shaft.gui.browser.internal.PermissionOrigin;

import java.nio.charset.StandardCharsets;

/**
 * Jazzer target (#7012): {@code PermissionOrigin.normalize} rejects bad input only with
 * {@link IllegalArgumentException}, and a normalized origin normalizes to itself.
 */
public final class PermissionOriginFuzzer {
    private PermissionOriginFuzzer() {
    }

    public static void fuzzerTestOneInput(byte[] data) {
        String normalized;
        try {
            normalized = PermissionOrigin.normalize(new String(data, StandardCharsets.UTF_8));
        } catch (IllegalArgumentException expected) {
            return;
        }
        if (!normalized.equals(PermissionOrigin.normalize(normalized))) {
            throw new IllegalStateException("normalize is not idempotent for " + normalized);
        }
    }
}
