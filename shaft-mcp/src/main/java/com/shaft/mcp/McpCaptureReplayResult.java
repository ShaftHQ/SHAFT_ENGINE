package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerationReport;

import java.nio.file.Path;
import java.util.List;

/**
 * MCP result for Capture generation, replay, and reusable code snippets.
 *
 * @param sourcePath generated source path
 * @param testDataPath generated test-data path
 * @param reportPath generation report path
 * @param successful whether generation and requested validations passed
 * @param codeBlocks copy-paste blocks extracted from generated output
 * @param report deterministic generation report
 * @param warnings safe warnings
 */
public record McpCaptureReplayResult(
        Path sourcePath,
        Path testDataPath,
        Path reportPath,
        boolean successful,
        List<McpCodeBlock> codeBlocks,
        CaptureGenerationReport report,
        List<String> warnings) {
    /**
     * Creates an immutable capture replay result.
     */
    public McpCaptureReplayResult {
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
