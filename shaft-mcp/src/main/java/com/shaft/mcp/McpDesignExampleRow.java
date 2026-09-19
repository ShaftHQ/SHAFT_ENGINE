package com.shaft.mcp;

import java.util.List;

/**
 * One Examples row in an outline-shaped Design pack (issue #5950).
 */
public record McpDesignExampleRow(String id, String kind, List<String> cells) {
    public McpDesignExampleRow {
        id = id == null ? "" : id;
        kind = kind == null ? "" : kind;
        cells = cells == null ? List.of() : List.copyOf(cells);
    }
}
