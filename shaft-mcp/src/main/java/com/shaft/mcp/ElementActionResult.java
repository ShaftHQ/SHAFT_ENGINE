package com.shaft.mcp;

import java.util.List;

/**
 * Result returned by unified, engine-dispatching {@code element_*} tools. {@code activeEngine}
 * always echoes which {@link ActiveEngine} handled the call (design doc amendment A2).
 */
public record ElementActionResult(
        String activeEngine,
        boolean recorded,
        McpCodeBlock codeBlock,
        List<String> warnings) {
    /**
     * Creates an immutable element action result.
     */
    public ElementActionResult {
        activeEngine = activeEngine == null ? ActiveEngine.NONE.name() : activeEngine;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
