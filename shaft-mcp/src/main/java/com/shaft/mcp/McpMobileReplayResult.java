package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerationReport;

import java.nio.file.Path;
import java.util.List;

/**
 * Result for MCP mobile recording code generation or replay.
 *
 * @param recordingPath original MCP recording path
 * @param successful whether generation or replay completed
 * @param replayedActionCount number of actions replayed against a live session
 * @param codeBlocks copy-paste replay blocks
 * @param warnings safe warnings
 * @param captureSessionPath optional Capture-compatible session path
 * @param sourcePath optional generated source path
 * @param testDataPath optional generated test-data path
 * @param reportPath optional generation report path
 * @param reviewPath optional deterministic review path
 * @param report optional Capture generation report
 */
public record McpMobileReplayResult(
        Path recordingPath,
        boolean successful,
        int replayedActionCount,
        List<McpCodeBlock> codeBlocks,
        List<String> warnings,
        Path captureSessionPath,
        Path sourcePath,
        Path testDataPath,
        Path reportPath,
        Path reviewPath,
        CaptureGenerationReport report) {
    /**
     * Creates a replay result without Capture generation metadata.
     *
     * @param recordingPath original recording path
     * @param successful whether generation or replay completed
     * @param replayedActionCount number of actions replayed
     * @param codeBlocks copy-paste replay blocks
     * @param warnings safe warnings
     */
    public McpMobileReplayResult(
            Path recordingPath,
            boolean successful,
            int replayedActionCount,
            List<McpCodeBlock> codeBlocks,
            List<String> warnings) {
        this(recordingPath, successful, replayedActionCount, codeBlocks, warnings,
                null, null, null, null, null, null);
    }

    /**
     * Creates an immutable mobile replay result.
     */
    public McpMobileReplayResult {
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
