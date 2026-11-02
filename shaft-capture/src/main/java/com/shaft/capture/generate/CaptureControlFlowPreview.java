package com.shaft.capture.generate;

import java.util.List;

/**
 * Deterministic control-flow preview reviewed before applying generated guards.
 *
 * @param schemaVersion preview schema version
 * @param sessionId source Capture session
 * @param deterministicFingerprint fingerprint of the deterministic source reviewed by the user
 * @param suggestions deterministic control-flow suggestions
 */
public record CaptureControlFlowPreview(
        String schemaVersion,
        String sessionId,
        String deterministicFingerprint,
        List<CaptureGenerationReport.ControlFlowSuggestion> suggestions) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable preview.
     */
    public CaptureControlFlowPreview {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        sessionId = sessionId == null ? "" : sessionId;
        deterministicFingerprint = deterministicFingerprint == null ? "" : deterministicFingerprint;
        suggestions = suggestions == null ? List.of() : List.copyOf(suggestions);
    }
}
