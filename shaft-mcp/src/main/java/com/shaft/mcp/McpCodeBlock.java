package com.shaft.mcp;

import java.util.List;

/**
 * Safe copy-paste code block returned by local MCP remediation tools.
 *
 * @param id stable block identifier
 * @param title short title
 * @param kind snippet type
 * @param language code language
 * @param imports imports needed by the block
 * @param code block content without Markdown fences
 * @param placement where the block is intended to be pasted
 * @param copyPasteReady whether the block is directly usable without filling placeholders
 * @param evidenceIds evidence references supporting the block
 * @param warnings safe warnings
 */
public record McpCodeBlock(
        String id,
        String title,
        Kind kind,
        String language,
        List<String> imports,
        String code,
        String placement,
        boolean copyPasteReady,
        List<String> evidenceIds,
        List<String> warnings) {
    /**
     * Code block type.
     */
    public enum Kind {
        LOCATOR,
        WAIT,
        ASSERTION,
        SETUP,
        ACTION,
        TEST_METHOD,
        FULL_CLASS,
        INVESTIGATION,
        PROVIDER_ADVISORY
    }

    /**
     * Creates an immutable code block.
     */
    public McpCodeBlock {
        id = text(id);
        title = text(title);
        kind = kind == null ? Kind.INVESTIGATION : kind;
        language = language == null || language.isBlank() ? "java" : language.trim();
        imports = imports == null ? List.of() : List.copyOf(imports);
        code = text(code);
        placement = text(placement);
        evidenceIds = evidenceIds == null ? List.of() : List.copyOf(evidenceIds);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
