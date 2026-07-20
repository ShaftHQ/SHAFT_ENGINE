package com.shaft.mcp;

import java.util.List;

/**
 * One tool's mechanically-dumped shape: name, owning service class, live description, and its
 * parameter list -- the "mechanical half" of design doc Decision 4 (amendment A5), everything
 * {@link ToolIndexDumper} can read straight off the live Spring-registered {@code ToolCallback}/
 * {@code @McpTool} schemas without guessing. Curated fields (intentKeywords, slashAlias,
 * cliCommand, mutation, sensitive, example) live only in the overlay merged by
 * {@code scripts/mcp/generate_tool_index.py}, never here.
 */
public record ToolIndexEntry(String name, String service, String description, List<ToolIndexParam> params) {
}
