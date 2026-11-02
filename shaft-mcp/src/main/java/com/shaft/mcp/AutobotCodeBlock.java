package com.shaft.mcp;

/**
 * Structured code block returned by a cloud provider codegen response.
 *
 * @param language code language, usually java
 * @param path repository-relative target path the block belongs in, or blank
 * @param insertionAnchor existing method or textual anchor to insert after, or blank
 * @param code block content without Markdown fences
 */
public record AutobotCodeBlock(
        String language,
        String path,
        String insertionAnchor,
        String code) {
    /**
     * Creates an immutable code block.
     */
    public AutobotCodeBlock {
        language = language == null || language.isBlank() ? "java" : language.trim();
        path = path == null ? "" : path.trim();
        insertionAnchor = insertionAnchor == null ? "" : insertionAnchor.trim();
        code = code == null ? "" : code;
    }
}
