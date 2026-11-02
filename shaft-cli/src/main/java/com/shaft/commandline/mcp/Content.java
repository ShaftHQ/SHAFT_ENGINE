package com.shaft.commandline.mcp;

/**
 * A single MCP content block returned inside a {@link CallToolResult}. shaft-mcp
 * emits {@code {"type":"text","text":"..."}} blocks; the {@code text} is frequently a
 * JSON string itself.
 *
 * @param type the content block type, typically {@code text}
 * @param text the textual payload, or {@code null} for non-text blocks
 */
public record Content(String type, String text) {
}
