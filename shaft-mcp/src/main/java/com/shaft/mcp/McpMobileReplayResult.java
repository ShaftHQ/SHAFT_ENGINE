package com.shaft.mcp;

import java.nio.file.Path;
import java.util.List;

/**
 * Result for MCP mobile recording code generation or replay.
 */
public record McpMobileReplayResult(
        Path recordingPath,
        boolean successful,
        int replayedActionCount,
        List<McpCodeBlock> codeBlocks,
        List<String> warnings) {
    /**
     * Creates an immutable mobile replay result.
     */
    public McpMobileReplayResult {
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
