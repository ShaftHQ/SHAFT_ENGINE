package com.shaft.mcp;

import com.shaft.doctor.label.ConfirmedCauseLabelModels;
import com.shaft.doctor.model.CauseCategory;

import java.util.List;

/**
 * MCP projection for confirmed Doctor defect-label confirm/suggest (issue #5971 / S3-05).
 *
 * @param schemaVersion result schema version
 * @param action confirm or suggest
 * @param status ok or error
 * @param message human-readable outcome
 * @param signature Doctor historical-signature key
 * @param causeCategory suggested or persisted category
 * @param displayAlias PRODUCT/TEST/ENVIRONMENT/LOCATOR/TIMING
 * @param matched whether a stored label was found (suggest) or written (confirm)
 * @param replaced whether confirm overwrote a prior label
 * @param storePath gitignored store path
 * @param warnings non-fatal notes (e.g. redacted evidence path)
 */
public record McpCauseLabelResult(
        String schemaVersion,
        String action,
        String status,
        String message,
        String signature,
        CauseCategory causeCategory,
        String displayAlias,
        boolean matched,
        boolean replaced,
        String storePath,
        List<String> warnings) {
    /**
     * Current schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable MCP cause-label result.
     */
    public McpCauseLabelResult {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                ? CURRENT_SCHEMA_VERSION
                : schemaVersion.trim();
        action = action == null ? "" : action.trim();
        status = status == null ? "error" : status.trim();
        message = message == null ? "" : message.trim();
        signature = signature == null ? "" : signature.trim();
        causeCategory = causeCategory == null ? CauseCategory.UNKNOWN : causeCategory;
        displayAlias = displayAlias == null ? "" : displayAlias.trim();
        storePath = storePath == null ? "" : storePath.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    static McpCauseLabelResult fromConfirm(ConfirmedCauseLabelModels.ConfirmResult result) {
        return new McpCauseLabelResult(
                CURRENT_SCHEMA_VERSION,
                "confirm",
                result.status(),
                result.message(),
                result.signature(),
                result.causeCategory(),
                result.displayAlias(),
                "ok".equals(result.status()),
                result.replaced(),
                result.storePath(),
                result.warnings());
    }

    static McpCauseLabelResult fromSuggest(ConfirmedCauseLabelModels.Suggestion suggestion) {
        return new McpCauseLabelResult(
                CURRENT_SCHEMA_VERSION,
                "suggest",
                suggestion.matched() ? "ok" : "empty",
                suggestion.message(),
                suggestion.signature(),
                suggestion.causeCategory(),
                suggestion.displayAlias(),
                suggestion.matched(),
                false,
                suggestion.storePath(),
                List.of());
    }
}
