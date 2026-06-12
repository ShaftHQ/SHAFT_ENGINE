package com.shaft.heal.internal;

import com.shaft.heal.model.LocatorFingerprint;

import java.util.Objects;

/**
 * Checksummed bounded local history record.
 *
 * @param schemaVersion history schema version
 * @param key lookup key
 * @param pageKey query-free page key
 * @param originalLocator original locator
 * @param context frame and shadow context
 * @param fingerprint verified element fingerprint
 * @param visualReference optional local visual reference
 * @param updatedAt UTC timestamp
 * @param checksum record checksum
 */
public record HistoryRecord(
        String schemaVersion,
        String key,
        String pageKey,
        String originalLocator,
        String context,
        LocatorFingerprint fingerprint,
        String visualReference,
        String updatedAt,
        String checksum) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable history record.
     */
    public HistoryRecord {
        schemaVersion = Objects.requireNonNullElse(schemaVersion, CURRENT_SCHEMA_VERSION);
        key = Objects.requireNonNullElse(key, "");
        pageKey = Objects.requireNonNullElse(pageKey, "");
        originalLocator = Objects.requireNonNullElse(originalLocator, "");
        context = Objects.requireNonNullElse(context, "");
        fingerprint = Objects.requireNonNull(fingerprint, "fingerprint");
        visualReference = Objects.requireNonNullElse(visualReference, "");
        updatedAt = Objects.requireNonNullElse(updatedAt, "");
        checksum = Objects.requireNonNullElse(checksum, "");
    }

    /**
     * Returns a copy with a checksum.
     *
     * @param value checksum
     * @return checksummed record
     */
    public HistoryRecord withChecksum(String value) {
        return new HistoryRecord(schemaVersion, key, pageKey, originalLocator, context,
                fingerprint, visualReference, updatedAt, value);
    }
}
