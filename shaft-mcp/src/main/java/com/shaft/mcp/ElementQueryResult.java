package com.shaft.mcp;

/**
 * Result returned by unified, engine-dispatching {@code element_is_*} query tools. {@code activeEngine}
 * always echoes which {@link ActiveEngine} answered the query (design doc amendment A2).
 *
 * @param activeEngine the engine that answered the query
 * @param value the query result
 */
public record ElementQueryResult(String activeEngine, boolean value) {
    /**
     * Creates an immutable element query result.
     */
    public ElementQueryResult {
        activeEngine = activeEngine == null ? ActiveEngine.NONE.name() : activeEngine;
    }
}
