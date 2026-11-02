package com.shaft.doctor.label;

import com.shaft.doctor.model.CauseCategory;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Local confirmed defect-label memory for Doctor (issue #5971 / S3-05).
 *
 * <p>No cloud ML: labels are user-confirmed cause categories keyed by Doctor historical-signature
 * keys. The workspace store is gitignored and must never retain evidence paths or secrets.
 */
public final class ConfirmedCauseLabelModels {
    private ConfirmedCauseLabelModels() {
    }

    /**
     * On-disk store document.
     *
     * @param schemaVersion store schema version
     * @param labels signature → confirmed entry (insertion-ordered)
     */
    public record StoreDocument(String schemaVersion, Map<String, LabelEntry> labels) {
        /**
         * Current store schema version.
         */
        public static final String CURRENT_SCHEMA_VERSION = "1.0";

        /**
         * Creates an immutable store document.
         */
        public StoreDocument {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                    ? CURRENT_SCHEMA_VERSION
                    : schemaVersion.trim();
            Map<String, LabelEntry> copy = new LinkedHashMap<>();
            if (labels != null) {
                labels.forEach((key, value) -> {
                    if (key != null && !key.isBlank() && value != null) {
                        copy.put(key.trim(), value);
                    }
                });
            }
            labels = Map.copyOf(copy);
        }

        /**
         * Empty store.
         *
         * @return empty document
         */
        public static StoreDocument empty() {
            return new StoreDocument(CURRENT_SCHEMA_VERSION, Map.of());
        }
    }

    /**
     * One confirmed label for a Doctor signature.
     *
     * @param causeCategory persisted CauseCategory (PRODUCT/TEST/LOCATOR/TIMING_SYNCHRONIZATION/ENVIRONMENT_CONFIGURATION)
     * @param displayAlias short user-facing alias (PRODUCT/TEST/ENVIRONMENT/LOCATOR/TIMING)
     * @param confirmedAtUtc ISO-8601 UTC timestamp of last confirm/override
     */
    public record LabelEntry(CauseCategory causeCategory, String displayAlias, String confirmedAtUtc) {
        /**
         * Creates an immutable label entry. Evidence paths are never accepted.
         */
        public LabelEntry {
            causeCategory = causeCategory == null ? CauseCategory.UNKNOWN : causeCategory;
            displayAlias = text(displayAlias);
            if (displayAlias.isBlank()) {
                displayAlias = ConfirmedCauseLabelStore.displayAlias(causeCategory);
            }
            confirmedAtUtc = text(confirmedAtUtc);
        }
    }

    /**
     * Suggestion returned when a signature matches a stored label.
     *
     * @param schemaVersion suggestion schema version
     * @param matched whether a stored label was found
     * @param signature looked-up signature key (blank when none)
     * @param causeCategory suggested category, or UNKNOWN when unmatched
     * @param displayAlias short alias, or blank when unmatched
     * @param message human-readable status
     * @param storePath relative or absolute store path used (never contains evidence file contents)
     */
    public record Suggestion(
            String schemaVersion,
            boolean matched,
            String signature,
            CauseCategory causeCategory,
            String displayAlias,
            String message,
            String storePath) {
        /**
         * Current suggestion schema version.
         */
        public static final String CURRENT_SCHEMA_VERSION = "1.0";

        /**
         * Creates an immutable suggestion.
         */
        public Suggestion {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                    ? CURRENT_SCHEMA_VERSION
                    : schemaVersion.trim();
            signature = text(signature);
            causeCategory = causeCategory == null ? CauseCategory.UNKNOWN : causeCategory;
            displayAlias = text(displayAlias);
            message = requireText(message, "Suggestion message");
            storePath = text(storePath);
        }

        /**
         * Unmatched empty suggestion.
         *
         * @param storePath store path consulted
         * @return unmatched suggestion
         */
        public static Suggestion none(String storePath) {
            return new Suggestion(
                    CURRENT_SCHEMA_VERSION,
                    false,
                    "",
                    CauseCategory.UNKNOWN,
                    "",
                    "No confirmed defect label for this signature.",
                    storePath);
        }
    }

    /**
     * Result of confirming (or overriding) a label.
     *
     * @param schemaVersion result schema version
     * @param status ok or error
     * @param message human-readable outcome
     * @param signature persisted signature key
     * @param causeCategory persisted category
     * @param displayAlias short alias
     * @param replaced whether an existing label was overwritten
     * @param storePath store path written
     * @param warnings non-fatal notes (e.g. redacted evidence path ignored)
     */
    public record ConfirmResult(
            String schemaVersion,
            String status,
            String message,
            String signature,
            CauseCategory causeCategory,
            String displayAlias,
            boolean replaced,
            String storePath,
            List<String> warnings) {
        /**
         * Current confirm-result schema version.
         */
        public static final String CURRENT_SCHEMA_VERSION = "1.0";

        /**
         * Creates an immutable confirm result.
         */
        public ConfirmResult {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                    ? CURRENT_SCHEMA_VERSION
                    : schemaVersion.trim();
            status = requireText(status, "Confirm status");
            message = requireText(message, "Confirm message");
            signature = text(signature);
            causeCategory = causeCategory == null ? CauseCategory.UNKNOWN : causeCategory;
            displayAlias = text(displayAlias);
            storePath = text(storePath);
            warnings = listOf(warnings);
        }
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    private static String requireText(String value, String label) {
        String cleaned = text(value);
        if (cleaned.isBlank()) {
            throw new IllegalArgumentException(label + " is required.");
        }
        return cleaned;
    }

    private static List<String> listOf(List<String> values) {
        return values == null ? List.of() : List.copyOf(values);
    }
}
