package com.shaft.heal.internal;

import com.shaft.heal.model.HealingContext;
import com.shaft.heal.model.HealingPlatform;
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
 * @param contextMetadata platform and execution context metadata
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
        HealingContext contextMetadata,
        LocatorFingerprint fingerprint,
        String visualReference,
        String updatedAt,
        String checksum) {
    public static final String CURRENT_SCHEMA_VERSION = "2.0";

    /**
     * Creates a backward-compatible web-only history record.
     */
    public HistoryRecord(
            String schemaVersion,
            String key,
            String pageKey,
            String originalLocator,
            String context,
            LocatorFingerprint fingerprint,
            String visualReference,
            String updatedAt,
            String checksum) {
        this(schemaVersion, key, pageKey, originalLocator, context,
                new HealingContext(
                        HealingContext.CURRENT_SCHEMA_VERSION,
                        HealingPlatform.WEB,
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        ""),
                fingerprint, visualReference, updatedAt, checksum);
    }

    /**
     * Creates an immutable history record.
     */
    public HistoryRecord {
        schemaVersion = Objects.requireNonNullElse(schemaVersion, CURRENT_SCHEMA_VERSION);
        key = Objects.requireNonNullElse(key, "");
        pageKey = Objects.requireNonNullElse(pageKey, "");
        originalLocator = Objects.requireNonNullElse(originalLocator, "");
        context = Objects.requireNonNullElse(context, "");
        contextMetadata = contextMetadata == null
                ? new HealingContext(
                        HealingContext.CURRENT_SCHEMA_VERSION,
                        HealingPlatform.UNKNOWN,
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "")
                : contextMetadata;
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
                contextMetadata, fingerprint, visualReference, updatedAt, value);
    }
}
