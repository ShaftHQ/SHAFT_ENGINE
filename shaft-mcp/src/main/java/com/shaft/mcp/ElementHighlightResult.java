package com.shaft.mcp;

/**
 * Result of the {@code element_highlight} tool: how many live elements a locator matches and whether
 * they were outlined in the browser (issue #6640).
 *
 * @param activeEngine the engine that answered the query
 * @param count the number of matching elements, zero when none match
 * @param highlighted true when the matches were outlined; false when none match or the engine has no DOM
 */
public record ElementHighlightResult(String activeEngine, int count, boolean highlighted) {
    /**
     * Creates an immutable element highlight result.
     */
    public ElementHighlightResult {
        activeEngine = activeEngine == null ? ActiveEngine.NONE.name() : activeEngine;
    }
}
