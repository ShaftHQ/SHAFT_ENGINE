package com.shaft.mcp;

import java.util.List;

/**
 * Browser screenshot payload returned by the MCP browser inspection tools.
 */
public record McpScreenshotResult(
        String mediaType,
        int byteLength,
        String base64,
        String outputPath,
        List<String> warnings) {
}
