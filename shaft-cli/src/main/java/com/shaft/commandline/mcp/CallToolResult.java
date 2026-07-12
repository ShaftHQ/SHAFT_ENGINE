package com.shaft.commandline.mcp;

import java.util.List;

/**
 * The result of a {@code tools/call}. shaft-mcp returns
 * {@code {"content":[{"type":"text","text":"..."}],"isError":false}} on both transports.
 *
 * @param content the content blocks
 * @param isError whether the tool reported a tool-level error
 */
public record CallToolResult(List<Content> content, boolean isError) {

    /**
     * Concatenates every text content block, newline-separated.
     *
     * @return the joined text of all {@code text} content blocks
     */
    public String text() {
        return content.stream()
                .filter(block -> "text".equals(block.type()) && block.text() != null)
                .map(Content::text)
                .reduce((a, b) -> a + System.lineSeparator() + b)
                .orElse("");
    }
}
