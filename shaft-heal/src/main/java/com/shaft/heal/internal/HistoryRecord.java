package com.shaft.heal.internal;

import com.shaft.heal.model.HealingContext;
import com.shaft.heal.model.HealingPlatform;
import com.shaft.heal.model.LocatorFingerprint;

import java.util.List;
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
 * @param outcomes list of healing outcomes
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
        String checksum,
        List<HistoryOutcome> outcomes) {
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
                fingerprint, visualReference, updatedAt, checksum, List.of());
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
        outcomes = outcomes == null ? List.of() : List.copyOf(outcomes);
    }

    /**
     * Returns a copy with a checksum.
     *
     * @param value checksum
     * @return checksummed record
     */
    public HistoryRecord withChecksum(String value) {
        return new HistoryRecord(schemaVersion, key, pageKey, originalLocator, context,
                contextMetadata, fingerprint, visualReference, updatedAt, value, outcomes);
    }

    /**
     * Returns a copy with an appended outcome.
     *
     * @param outcome healing outcome to append
     * @return record with outcome appended
     */
    public HistoryRecord withOutcome(HistoryOutcome outcome) {
        List<HistoryOutcome> updated = new java.util.ArrayList<>(outcomes);
        updated.add(outcome);
        return new HistoryRecord(schemaVersion, key, pageKey, originalLocator, context,
                contextMetadata, fingerprint, visualReference, updatedAt, checksum, updated);
    }
}
