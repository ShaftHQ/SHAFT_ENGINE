package com.shaft.heal.internal;

import java.util.List;
import java.util.Objects;

/**
 * Versioned local history document.
 *
 * @param schemaVersion document schema version
 * @param records retained history records
 */
public record HistoryDocument(String schemaVersion, List<HistoryRecord> records) {
    public static final String CURRENT_SCHEMA_VERSION = "2.0";

    /**
     * Creates an immutable document.
     */
    public HistoryDocument {
        schemaVersion = Objects.requireNonNullElse(schemaVersion, CURRENT_SCHEMA_VERSION);
        records = records == null ? List.of() : List.copyOf(records);
    }
}
