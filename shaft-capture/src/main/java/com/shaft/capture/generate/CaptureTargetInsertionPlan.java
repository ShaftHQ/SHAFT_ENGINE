package com.shaft.capture.generate;

import java.nio.file.Path;
import java.util.List;

/**
 * Focused record-at-target insertion snippets derived from generated Capture source.
 *
 * @param targetSource existing source file selected by the user
 * @param insertAfter insertion anchor supplied by the user
 * @param imports imports required by locator or action snippets
 * @param locatorFields locator fields copied from generated source
 * @param actionSnippet generated action lines for the target method
 * @param placement human-readable placement guidance
 * @param anchorFound whether the target source contains the supplied anchor
 * @param warnings safe validation warnings
 */
public record CaptureTargetInsertionPlan(
        Path targetSource,
        String insertAfter,
        List<String> imports,
        String locatorFields,
        String actionSnippet,
        String placement,
        boolean anchorFound,
        List<String> warnings) {
    /**
     * Creates an immutable insertion plan.
     */
    public CaptureTargetInsertionPlan {
        insertAfter = insertAfter == null ? "" : insertAfter.trim();
        imports = imports == null ? List.of() : List.copyOf(imports);
        locatorFields = locatorFields == null ? "" : locatorFields.trim();
        actionSnippet = actionSnippet == null ? "" : actionSnippet.trim();
        placement = placement == null ? "" : placement.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
