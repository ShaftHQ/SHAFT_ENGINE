package com.shaft.mcp;

import java.util.List;

/**
 * One official SHAFT user-guide match returned by {@code shaft_guide_search}.
 *
 * @param title page title
 * @param section matched section title
 * @param url canonical guide URL
 * @param score local relevance score
 * @param excerpt short official guide excerpt
 * @param codeBlocks nearby official guide code examples
 */
public record McpGuideMatch(
        String title,
        String section,
        String url,
        double score,
        String excerpt,
        List<McpCodeBlock> codeBlocks) {
    /**
     * Creates an immutable guide match.
     */
    public McpGuideMatch {
        title = text(title);
        section = text(section);
        url = text(url);
        excerpt = text(excerpt);
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
