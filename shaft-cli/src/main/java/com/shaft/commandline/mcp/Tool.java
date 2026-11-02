package com.shaft.commandline.mcp;

import tools.jackson.databind.JsonNode;

/**
 * A tool advertised by shaft-mcp via {@code tools/list}.
 *
 * @param name        the tool name, e.g. {@code browser_navigate}
 * @param description the human-readable description (first sentence is shown in the tools listing)
 * @param inputSchema the raw JSON Schema for the tool's arguments, or {@code null} if absent
 */
public record Tool(String name, String description, JsonNode inputSchema) {
}
