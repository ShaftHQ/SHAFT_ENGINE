package com.shaft.mcp;

import java.util.List;

/**
 * Browser DOM snapshot returned by the MCP browser inspection tools.
 */
public record McpPageDomSnapshot(
        String currentUrl,
        String title,
        String dom,
        int characterCount,
        boolean truncated,
        List<String> warnings) {
}
