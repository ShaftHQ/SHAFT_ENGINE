package com.shaft.mcp;

import java.util.List;

/**
 * One Gherkin step classification for the fluent-API gap map (issue #5954).
 *
 * @param stepText           original step line
 * @param classification     mapped | new-helper | needs-recording | ambiguous
 * @param shaftType          engine type name when mapped, otherwise empty
 * @param shaftMethod        engine method name when mapped, otherwise empty
 * @param candidates         ambiguous candidate labels (type#method), otherwise empty
 * @param note               short rationale; never invents locator strings
 */
public record McpDesignGapMapStep(
        String stepText,
        String classification,
        String shaftType,
        String shaftMethod,
        List<String> candidates,
        String note) {
    public static final String MAPPED = "mapped";
    public static final String NEW_HELPER = "new-helper";
    public static final String NEEDS_RECORDING = "needs-recording";
    public static final String AMBIGUOUS = "ambiguous";

    public McpDesignGapMapStep {
        stepText = stepText == null ? "" : stepText;
        classification = classification == null ? NEEDS_RECORDING : classification;
        shaftType = shaftType == null ? "" : shaftType;
        shaftMethod = shaftMethod == null ? "" : shaftMethod;
        candidates = candidates == null ? List.of() : List.copyOf(candidates);
        note = note == null ? "" : note;
    }
}
