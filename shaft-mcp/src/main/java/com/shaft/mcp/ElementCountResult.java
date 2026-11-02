package com.shaft.mcp;

/**
 * Result of the {@code element_count} tool: how many live elements a locator matches (issue #6421).
 *
 * @param activeEngine the engine that answered the query
 * @param count the number of matching elements, zero when none match
 */
public record ElementCountResult(String activeEngine, int count) {
    /**
     * Creates an immutable element count result.
     */
    public ElementCountResult {
        activeEngine = activeEngine == null ? ActiveEngine.NONE.name() : activeEngine;
    }
}
